(ns ogres.app.provider.idb
  (:require [uix.core :as uix :refer [$ defui]]
            [shadow.cljs.modern :refer (js-await)]))

(def ^:private context (uix/create-context (js/Promise.resolve)))

(defn ^:private upgrade [event]
  (let [idb (.. event -target -result)
        app (.createObjectStore idb "app" #js {"keyPath" "release"})]
    (.createIndex app "updated" "updated")
    (.createObjectStore idb "images" #js {"keyPath" "checksum"})))

(defn ^:private initialize [name version]
  (js/Promise.
   (fn [resolve]
     (let [open    (.open js/indexedDB name version)
           success (fn [event] (resolve (.. event -target -result)))]
       (.addEventListener open "success" success)
       (.addEventListener open "upgradeneeded" upgrade)
       (.addEventListener open "error" js/console.error)
       (fn []
         (.removeEventListener open "success" success)
         (.removeEventListener open "upgradeneeded" upgrade)
         (.removeEventListener open "error" js/console.error))))))

(defui provider
  "Provides a React context whose value is a Promise which resolves
   with an instance of IDBDatabase, an IndexedDB database connection."
  [props]
  ($ context {:value (initialize "ogres.app" 10)}
    (:children props)))

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
     (fn [op data]
       (js-await [idb req]
         (js/Promise.
          (fn [resolve]
            (let [tx (.transaction idb #js [table] "readwrite")
                  st (.objectStore tx table)
                  op (aget st (name op))]
              (.addEventListener tx "complete" resolve)
              (loop [[x & xs] data]
                (.call op st x)
                (if (seq xs)
                  (recur xs)
                  (.commit tx)))))))) [table req])))
