(ns ogre.tools.storage
  (:require [datascript.core :as ds]
            [datascript.transit :as dt]
            [dexie]
            [ogre.tools.env :as env]
            [ogre.tools.provider.events :refer [subscribe!]]
            [ogre.tools.provider.state :as state :refer [use-query]]
            [ogre.tools.timing :refer [debounce]]
            [uix.core.alpha :as uix :refer [defcontext]]))

(defcontext context)

(def ignored-attrs
  #{:local/type
    :local/loaded?
    :local/privileged?
    :local/sharing?
    :local/paused?
    :local/modifier
    :session/state})

(defn initialize []
  (let [store (dexie. "ogre.tools")]
    (.stores (.version store 1) #js {:images "checksum" :app "release, updated"})
    store))

(defn provider
  "Provides an instance of the Dexie object, a convenience wrapper around
   the browser's IndexedDB data store."
  [child]
  (let [store (initialize)]
    (uix/context-provider [context store] child)))

(defn use-store []
  (uix/context context))

(defn marshaller
  "Listens to transactions to the DataScript database and serializes the
   application state to the browser's IndexedDB store. This is only performed
   on the host window and only after the application has already initialized
   the state." []
  (let [conn  (uix/context state/context)
        store (use-store)]
    (uix/effect!
     (fn []
       (ds/listen! conn :marshaller
                   (debounce
                    (fn [{:keys [db-after]}]
                      (if (:local/loaded? (ds/entity db-after [:db/ident :local]))
                        (-> db-after
                            (ds/db-with [[:db/retractEntity [:db/ident :session]]])
                            (ds/filter (fn [_ [_ attr _ _]] (not (contains? ignored-attrs attr))))
                            (ds/datoms :eavt)
                            (dt/write-transit-str)
                            (as-> marshalled #js {:release env/VERSION :updated (* -1 (.now js/Date)) :data marshalled})
                            (as-> record (.put (.table store "app") record))))) 200))
       (fn [] (ds/unlisten! conn :marshaller))) []) nil))

(defn unmarshaller
  "Initializes the DataScript database from the serialized state within the
   browser's IndexedDB store. This is only run once for both the host and
   view window." []
  (let [conn    (uix/context state/context)
        store   (use-store)
        tx-data [[:db/add [:db/ident :local] :local/loaded? true]
                 [:db/add [:db/ident :local] :local/type (state/local-type)]]]
    (uix/effect!
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
              (ds/transact! conn tx-data))))) []) nil))

(defn reset-handler []
  (let [store (use-store)]
    (subscribe!
     (fn []
       (.delete store)
       (.reload (.-location js/window))) :storage/reset []) nil))

(defn handlers
  "Registers event handlers related to IndexedDB, such as those involved in
   saving and loading the application state." []
  (let [{type :local/type} (use-query [:local/type])]
    (case type
      :host [:<> [unmarshaller] [marshaller] [reset-handler]]
      :view [unmarshaller]
      :conn [:<>])))
