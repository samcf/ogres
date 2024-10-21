(ns ogres.app.provider.idb
  (:require [ogres.app.provider.events :as events]
            [uix.core :as uix :refer [defui $]]
            [shadow.cljs.modern :refer (js-await)]
            ["@msgpack/msgpack" :as MessagePack]))

(def ^:private context (uix/create-context))

(defn ^:private download [data filename]
  (let [anchor (js/document.createElement "a")
        url    (js/URL.createObjectURL data)]
    (set! anchor -href url)
    (set! anchor -download filename)
    (.click anchor)
    (.remove anchor)))

(defn ^:private update-values [f object]
  (js/Object.fromEntries
   (.map (js/Object.entries object)
         (fn [[k v]] #js [k (f v)]))))

(defn ^:private upgrade
  "Responds to the 'upgradeneeded' event triggered when opening a
   new connection to the IndexedDB store by initializing the
   necessary object stores and their indices."
  [event]
  (let [idb (.. event -target -result)
        app (.createObjectStore idb "app" #js {"keyPath" "release"})]
    (.createIndex app "updated" "updated")
    (.createObjectStore idb "images" #js {"keyPath" "checksum"})))

(defn ^:private initialize
  "Returns a Promise which resolves with an IndexedDB database
   connection to the store given by name and version. This will
   automatically create the necessary object stores and indices."
  [name version]
  (js/Promise.
   (fn [resolve reject]
     (let [open    (.open js/indexedDB name version)
           success (fn [event] (resolve (.. event -target -result)))]
       (.addEventListener open "success" success)
       (.addEventListener open "upgradeneeded" upgrade)
       (.addEventListener open "error" js/console.error)
       (fn []
         (.removeEventListener open "success" success)
         (.removeEventListener open "upgradeneeded" upgrade)
         (.removeEventListener open "error" reject))))))

(defn ^:private marshal-value [[k v]]
  (if (instance? js/Blob v)
    (.then (.arrayBuffer v) (fn [buf] #js [k (js/Uint8Array. buf)]))
    (js/Promise.resolve #js [k v])))

(defn ^:private marshal-record [record]
  (.then (js/Promise.all (.map (js/Object.entries record) marshal-value))
         js/Object.fromEntries))

(defn ^:private marshal-store [idb store]
  (js/Promise.
   (fn [resolve]
     (let [tx (.transaction idb #js [store] "readonly")
           st (.objectStore tx store)
           rq (.getAll st)]
       (.addEventListener rq "success"
         (fn [event]
           (.then (js/Promise.all (.map (.. event -target -result) marshal-record))
                  (fn [records] (resolve #js [store records])))))))))

(defn ^:private marshal
  "Returns a Promise which resolves with a single object, a cloned
   instance of the given IndexedDB object, whose keys are the names
   of the object stores and whose values is an array of the store's
   records."
  [idb]
  (let [stores (js/Array.from (.-objectStoreNames idb))
        stores (.map stores (fn [store] (marshal-store idb store)))]
    (.then (js/Promise.all stores) js/Object.fromEntries)))

(defn ^:private unmarshal-value [value]
  (if (instance? js/Uint8Array. value)
    (js/Blob. #js [value] #js {"type" "image/jpeg"})
    value))

(defn ^:private unmarshal
  "Returns a Promise which resolves with the given database object
   whose records have been converted into types appropriate for
   storing in IndexedDB.
   ```
   (let [db #js {\"app\" #js [] \"images\" #js []}]
     (.then (unmarshal db)
            (fn [db]
              (js/console.log db))))
   ```"
  [object]
  (->> (update-values (fn [xs] (.map xs (fn [x] (update-values unmarshal-value x)))) object)
       (js/Promise.resolve)))

(defn ^:private replace-store
  "Removes all records from the given object store and replaces them
   with the given records. Returns a Promise which resolves when the
   transaction is completed."
  [idb [name records]]
  (js/Promise.
   (fn [resolve]
     (let [tx (.transaction idb #js [name] "readwrite")
           st (.objectStore tx name)
           rq (.clear st)]
       (.addEventListener tx "complete" resolve)
       (.addEventListener rq "success"
         (fn []
           (doseq [record records]
             (.put st record))
           (.commit tx)))))))

(defui ^:private listeners []
  (let [req (uix/use-context context)]
    ;; Resets the application to its original state, deleting the current
    ;; IndexedDB database and then reloads the page once completed.
    (events/use-subscribe :store/reset
      (uix/use-callback
       (fn []
         (let [delete (.deleteDatabase js/indexedDB "ogres.app")
               reload (fn [] (.. js/window -location reload))]
           (.addEventListener delete "blocked" reload)
           (.addEventListener delete "success" reload))) []))

    ;; Marshals the IndexedDB database as a MessagePack binary then
    ;; automatically downloads it to the user's filesystem. This
    ;; backup file can be used to restore the application state,
    ;; including all images.
    (events/use-subscribe :store/create-backup
      (uix/use-callback
       (fn []
         (js-await [idb req]
           (-> (marshal idb)
               (.then MessagePack/encode)
               (.then (fn [bytes] (js/Blob. #js [bytes] #js {"type" "application/octet-stream"})))
               (.then (fn [bytes] (download bytes "ogres.app.backup")))))) [req]))

    ;; Restores the application using a backup file and reloads the
    ;; page once completed.
    (events/use-subscribe :store/restore-backup
      (uix/use-callback
       (fn [file]
         (js-await [idb req]
           (-> (MessagePack/decodeAsync (.stream file))
               (.then unmarshal)
               (.then (fn [db] (js/Promise.all (.map (js/Object.entries db) (fn [store] (replace-store idb store))))))
               (.then (fn [] (.. js/window -location reload)))
               (.catch js/console.error)))) [req]))))

(defui provider
  "Provides a React context whose value is a Promise which resolves
   with an instance of IDBDatabase, an IndexedDB database connection."
  [props]
  (let [[req] (uix/use-state (initialize "ogres.app" 10))]
    ($ context {:value req}
      ($ listeners)
      (:children props))))

(defn use-reader
  "Returns a function which, when called with a key, returns a Promise
   that resolves with the object found in the IndexedDB object store for
   it, or nil if it does not exist.
   ```
   (let [read (use-reader \"images\")]
     (.then (read \"123\")
            (fn [record]
              (prn record))))
   ```"
  [table]
  (let [req (uix/use-context context)]
    (uix/use-callback
     (fn [key]
       (js-await [idb req]
         (js/Promise.
          (fn [resolve]
            (let [tx (.transaction idb #js [table] "readonly")
                  st (.objectStore tx table)
                  on-success
                  (fn [event]
                    (let [result (.. event -target -result)]
                      (resolve result)))]
              (.addEventListener (.get st key) "success" on-success)))))) [table req])))

(defn use-writer
  "Returns a function which, when called with an operation name and a
   collection of values, performs that operation against the IndexedDB
   object store for each given value. Returns a Promise when completed
   resolved to nothing. Supported operations include :add, :delete, and
   :put.
   ```
   (let [write (use-writer \"images\")]
     (.then (write :put [#js {\"checksum\" 123 \"data\" [1 2 3]}])
            (fn []
              (prn \"put completed\"))))
   ```"
  [table]
  (let [req (uix/use-context context)]
    (uix/use-callback
     (fn [op records]
       (js-await [idb req]
         (js/Promise.
          (fn [resolve]
            (let [tx (.transaction idb #js [table] "readwrite")
                  st (.objectStore tx table)
                  op (aget st (name op))]
              (.addEventListener tx "complete" resolve)
              (doseq [record records]
                (.call op st record))
              (.commit tx)))))) [table req])))
