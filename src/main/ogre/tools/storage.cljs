(ns ogre.tools.storage
  (:require [datascript.core :as ds]
            [datascript.transit :as dt]
            [dexie]
            [ogre.tools.state :refer [schema state use-query]]
            [uix.core.alpha :as uix :refer [defcontext]]))

(defcontext storage)

(def ignored-attrs
  #{:root/host?
    :root/loaded?
    :root/privileged?
    :share/open?
    :share/paused?
    :canvas/modifier
    :image/url})

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
  (let [[conn]          (uix/context state)
        {:keys [store]} (uix/context storage)
        tx-data
        [[:db/add [:db/ident :root] :root/loaded? true]
         [:db/add [:db/ident :root] :root/host? (host? js/window)]]]
    (uix/effect!
     (fn []
       (-> (.table store "app")
           (.get 1)
           (.then
            (fn [record]
              (if (nil? record)
                (ds/transact! conn tx-data)
                (->
                 (.-data record)
                 (dt/read-transit-str)
                 (ds/conn-from-datoms schema)
                 (ds/db)
                 (ds/db-with tx-data)
                 (as-> data (ds/reset-conn! conn data))))))
           (.catch
            (fn []
              (ds/transact! conn tx-data))))) []) nil))

(defn marshaller
  "Listens to transactions to the DataScript database and serializes the
   application state to the browser's IndexedDB store. This is only performed
   on the host window and only after the application has already initialized
   the state." []
  (let [[conn]          (uix/context state)
        {:keys [store]} (uix/context storage)
        [data]          (use-query {:pull [:root/host? :root/loaded?]})
        {:root/keys [host? loaded?]} data]
    (uix/effect!
     (fn []
       (ds/listen!
        conn :marshaller
        (fn [report]
          (if (and host? loaded?)
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
  (let [[conn]          (uix/context state)
        {:keys [store]} (uix/context storage)]
    (uix/effect!
     (fn []
       (ds/listen!
        conn :reset
        (fn [{[event _ _] :tx-meta}]
          (when (= event :storage/reset)
            (do (.delete store)
                (.reload (.-location js/window))))))) [])
    [:<>
     [unmarshaller]
     [marshaller]]))

(defn provider
  "Provides an instance of the Dexie object, a convenience wrapper around
   the browser's IndexedDB data store."
  [child]
  (let [store (initialize)]
    (uix/context-provider [storage {:store store}] child)))
