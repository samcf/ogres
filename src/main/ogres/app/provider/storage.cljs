(ns ogres.app.provider.storage
  (:require [datascript.core :as ds]
            [datascript.transit :as dt]
            [dexie]
            [dexie-export-import
             :refer [exportDB importDB peakImportFile]
             :rename {exportDB export-db importDB import-db peakImportFile peek-import-file}]
            [ogres.app.const :refer [VERSION]]
            [ogres.app.dom :refer [local-type]]
            [ogres.app.provider.events :refer [use-subscribe]]
            [ogres.app.provider.state :as state :refer [use-query]]
            [ogres.app.util :refer [debounce]]
            [uix.core :refer [defui $ create-context use-callback use-context use-effect]]))

(def ^:private context (create-context))

(def ^:private ignored-attrs
  #{:local/type
    :local/status
    :local/privileged?
    :local/sharing?
    :session/state})

(defn initialize []
  (let [store (dexie. "ogres.app")]
    (.stores (.version store 1) #js {:images "checksum" :app "release, updated"})
    store))

(defn use-store []
  (use-context context))

(defui provider
  "Provides an instance of the Dexie object, a convenience wrapper around
   the browser's IndexedDB data store."
  [{:keys [children]}]
  (let [store (initialize)]
    ($ (.-Provider context) {:value store} children)))

(defui ^:private marshaller
  []
  (let [conn  (use-context state/context)
        store (use-store)]
    (use-effect
     (fn []
       (ds/listen! conn :marshaller
                   (debounce
                    (fn [{:keys [db-after]}]
                      (if (= (:local/status (ds/entity db-after [:db/ident :local])) :ready)
                        (-> db-after
                            (ds/db-with [[:db/retract [:db/ident :session] :session/host]
                                         [:db/retract [:db/ident :session] :session/conns]])
                            (ds/filter (fn [_ [_ attr _ _]] (not (contains? ignored-attrs attr))))
                            (ds/datoms :eavt)
                            (dt/write-transit-str)
                            (as-> marshalled #js {:release VERSION :updated (* -1 (.now js/Date)) :data marshalled})
                            (as-> record (.put (.table store "app") record))))) 200))
       (fn [] (ds/unlisten! conn :marshaller)))) []))

(defui ^:private unmarshaller
  []
  (let [conn    (use-context state/context)
        store   (use-store)
        tx-data [[:db/add [:db/ident :local] :local/status :ready]
                 [:db/add [:db/ident :local] :local/type (local-type)]]]
    (use-effect
     (fn []
       (-> (.table store "app")
           (.get VERSION)
           (.then
            (fn [record]
              (if (nil? record)
                (ds/transact! conn tx-data)
                (->
                 (.-data record)
                 (dt/read-transit-str)
                 (ds/conn-from-datoms state/schema)
                 (ds/db)
                 (ds/db-with tx-data)
                 (as-> data (ds/reset-conn! conn data))))))
           (.catch
            (fn []
              (ds/transact! conn tx-data)))))) []))

(defui ^:private reset-handler []
  (let [store (use-store)]
    (use-subscribe :storage/reset
      (use-callback
       (fn []
         (.delete store)
         (.reload (.-location js/window))) [store]))))

(defui ^:private remove-handler []
  (let [store (use-store)
        on-remove
        (use-callback
         (fn [{[id] :args}]
           (.delete (.table store "images") id)) [store])
        on-remove-bulk
        (use-callback
         (fn [{[ids] :args}]
           (.bulkDelete (.table store "images") (clj->js ids))) [store])]
    (use-subscribe :tokens/remove on-remove)
    (use-subscribe :scene-images/remove on-remove)
    (use-subscribe :tokens/remove-all on-remove-bulk)
    (use-subscribe :scene-images/remove-all on-remove-bulk)))

(defui ^:private backup-handler []
  (let [store (use-store)]
    (use-subscribe :storage/backup
      (use-callback
       (fn []
         (.then
          (export-db store)
          (fn [blob]
            (let [blob-url (js/window.URL.createObjectURL blob)
                  shadow-anchor (js/document.createElement "a")]
              (set! shadow-anchor -href blob-url)
              (set! shadow-anchor -download
                    (str "ogres-"
                         (first (.split (.toISOString (js/Date.)) "T"))
                         ".json"))
              (.click shadow-anchor)
              (.remove shadow-anchor)))))
       [store]))))

(defui ^:private restore-handler []
  (let [store (use-store)]
    (use-subscribe :storage/restore
      (use-callback
       (fn [{[file] :args}]
         (-> (peek-import-file file)
             (.then
              (fn [metadata]
                (if (= (.-formatName metadata) "dexie")
                  (.delete store)
                  (throw (js/Error. "Selected file is not the right format.")))))
             (.then (fn [] (import-db file)))
             (.then
              (fn [^js/object db]
                (.toArray (.-app db))))
             (.then
              (fn [records]
                (.sort records (fn [^js/object a ^js/object b] (- (.-updated a) (.-updated b))))
                (let [release (.-release (aget records 0))
                      params  (js/URLSearchParams. #js {"r" release})]
                  (.replace (.-location js/window) (str "/?" (.toString params))))))
             (.catch
              (fn [error]
                (js/console.error error)
                (js/alert
                 "There was a problem trying to restore from this file. See
                  developer console for more details.")))))
       [store]))))

(defui handlers
  "Registers event handlers related to IndexedDB, such as those involved in
   saving and loading the application state." []
  (let [{type :local/type} (use-query [:local/type])]
    (case type
      :host ($ :<>
              ($ unmarshaller)
              ($ marshaller)
              ($ remove-handler)
              ($ reset-handler)
              ($ backup-handler)
              ($ restore-handler))
      :view ($ :<> ($ unmarshaller))
      :conn ($ :<> ($ remove-handler)))))
