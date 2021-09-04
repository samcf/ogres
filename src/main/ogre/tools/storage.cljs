(ns ogre.tools.storage
  (:require [ogre.tools.state :refer [schema state]]
            [ogre.tools.query :as query]
            [uix.core.alpha :as uix :refer [defcontext]]
            [datascript.core :as ds]
            [datascript.transit :as dt]
            [dexie]))

(defcontext storage)

(def ignored-attrs
  #{:viewer/host?
    :viewer/loaded?
    :viewer/privileged?
    :share/open?
    :share/paused?})

(defn host? [element]
  (->
   (.. element -location -search)
   (js/URLSearchParams.)
   (.get "share")
   (not= "true")))

(defn initialize []
  (let [store (dexie. "ogre.tools")]
    (.stores (.version store 1) #js {:images "checksum" :app "++id"})
    store))

(defn unmarshaller
  "Initializes the DataScript database from the serialized state within the
   browser's IndexedDB store. This is only run once for both the host and
   guest windows." []
  (let [{:keys [conn data]}            (uix/context state)
        {:keys [store]}                (uix/context storage)
        {:keys [db/id viewer/loaded?]} (query/viewer data)
        tx-data
        [[:db/add id :viewer/loaded? true]
         [:db/add id :viewer/host? (host? js/window)]]]
    (uix/effect!
     (fn []
       (-> (.table store "app")
           (.get 1)
           (.then
            (fn [record]
              (let []
                (if (nil? record)
                  (ds/transact! conn tx-data)
                  (->
                   (.-data record)
                   (dt/read-transit-str)
                   (ds/conn-from-datoms schema)
                   (ds/db)
                   (ds/db-with tx-data)
                   (as-> data (ds/reset-conn! conn data))))))))) [nil]) nil))

(defn marshaller
  "Listens to transactions to the DataScript database and serializes the
   application state to the browser's IndexedDB store. This is only performed
   on the host window and only after the application has already initialized
   the state." []
  (let [{:keys [conn data]} (uix/context state)
        {:keys [store]} (uix/context storage)
        {:keys [viewer/host? viewer/loaded?]} (query/viewer data)]
    (uix/effect!
     (fn []
       (ds/listen!
        conn :marshaller
        (fn [report]
          (when (and host? loaded?)
            (-> (ds/filter
                 (:db-after report)
                 (fn [_ [_ attr _ _]]
                   (not (contains? ignored-attrs attr))))
                (ds/datoms :eavt)
                (dt/write-transit-str)
                (as-> marshalled
                      (-> (.table store "app")
                          (.put #js {:id 1 :data marshalled})))))))
       (fn [] (ds/unlisten! conn :marshaller))) [loaded?]) nil))

(defn handlers
  "Registers event handlers related to IndexedDB, such as those involved in
   saving and loading the application state." []
  (let [{:keys [conn]} (uix/context state)
        {:keys [store]} (uix/context storage)]
    (uix/effect!
     (fn []
       (ds/listen!
        conn :reset
        (fn [{[event _ _] :tx-meta}]
          (when (= event :storage/reset)
            (do (.delete store)
                (.reload (.-location js/window))))))) [nil])
    [:<>
     [unmarshaller]
     [marshaller]]))

(defn provider
  "Provides an instance of the Dexie object, a convenience wrapper around
   the browser's IndexedDB data store."
  [child]
  (let [store (initialize)]
    (uix/context-provider [storage {:store store}] child)))
