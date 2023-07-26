(ns ogres.app.provider.storage
  (:require [datascript.core :as ds]
            [datascript.transit :as dt]
            [dexie]
            [ogres.app.env :as env]
            [ogres.app.provider.events :refer [use-subscribe]]
            [ogres.app.provider.state :as state :refer [use-query]]
            [ogres.app.timing :refer [debounce]]
            [uix.core :refer [defui $ create-context use-callback use-context use-effect]]))

(def context (create-context))

(def ignored-attrs
  #{:local/type
    :local/loaded?
    :local/privileged?
    :local/sharing?
    :local/paused?
    :local/modifier
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

(defui marshaller
  "Listens to transactions to the DataScript database and serializes the
   application state to the browser's IndexedDB store. This is only performed
   on the host window and only after the application has already initialized
   the state."
  []
  (let [conn  (use-context state/context)
        store (use-store)]
    (use-effect
     (fn []
       (ds/listen! conn :marshaller
                   (debounce
                    (fn [{:keys [db-after]}]
                      (if (:local/loaded? (ds/entity db-after [:db/ident :local]))
                        (-> db-after
                            (ds/db-with [[:db/retract [:db/ident :session] :session/host]
                                         [:db/retract [:db/ident :session] :session/conns]])
                            (ds/filter (fn [_ [_ attr _ _]] (not (contains? ignored-attrs attr))))
                            (ds/datoms :eavt)
                            (dt/write-transit-str)
                            (as-> marshalled #js {:release env/VERSION :updated (* -1 (.now js/Date)) :data marshalled})
                            (as-> record (.put (.table store "app") record))))) 200))
       (fn [] (ds/unlisten! conn :marshaller)))) []))

(defui unmarshaller
  "Initializes the DataScript database from the serialized state within the
   browser's IndexedDB store. This is only run once for both the host and
   view window."
  []
  (let [conn    (use-context state/context)
        store   (use-store)
        tx-data [[:db/add [:db/ident :local] :local/loaded? true]
                 [:db/add [:db/ident :local] :local/type (state/local-type)]]]
    (use-effect
     (fn []
       (-> (.table store "app")
           (.get env/VERSION)
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

(defui reset-handler
  []
  (let [store (use-store)]
    (use-subscribe :storage/reset
      (use-callback
       (fn []
         (.delete store)
         (.reload (.-location js/window))) [store]))))

(defui remove-handler []
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

(defui handlers
  "Registers event handlers related to IndexedDB, such as those involved in
   saving and loading the application state." []
  (let [{type :local/type} (use-query [:local/type])]
    (case type
      :host ($ :<> ($ unmarshaller) ($ marshaller) ($ remove-handler) ($ reset-handler))
      :view ($ :<> ($ unmarshaller))
      :conn ($ :<> ($ remove-handler)))))
