(ns ogre.tools.state
  (:require [uix.core.alpha :as uix :refer [defcontext]]
            [datascript.core :as ds]
            [ogre.tools.txs :as txs]))

(def schema
  {:db/ident          {:db/unique :db.unique/identity}
   :root/canvases     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/scenes       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/stamps       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/canvas       {:db/valueType :db.type/ref}
   :canvas/scene      {:db/valueType :db.type/ref}
   :canvas/tokens     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :canvas/shapes     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :canvas/selected   {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :canvas/initiative {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :token/stamp       {:db/valueType :db.type/ref}
   :image/checksum    {:db/unique :db.unique/identity}})

(defn initial-data []
  (ds/db-with
   (ds/empty-db schema)
   [[:db/add -1 :db/ident :root]
    [:db/add -1 :root/canvases -2]
    [:db/add -1 :root/canvas -2]
    [:db/add -1 :bounds/self [0 0 0 0]]
    [:db/add -1 :bounds/host [0 0 0 0]]
    [:db/add -1 :bounds/guest [0 0 0 0]]
    [:db/add -2 :db/ident :canvas]
    [:db/add -2 :element/name ""]]))

(defcontext state)

(def root-query
  '[:find (pull $ ?id pattern) . :in $ pattern :where [?id :db/ident :root]])

(defn use-query
  "React hook to run queries against the underlying DataScript database."
  ([] (let [[_ dispatch] (uix/context state)] [dispatch]))
  ([{:keys [query pull args key] :or {query root-query args []}}]
   (let [[conn dispatch] (uix/context state)
         listen-key      (deref (uix/state (ds/squuid)))
         get-result      (uix/callback
                          (fn []
                            (let [args (if pull (conj args pull) args)]
                              (apply ds/q query @conn args))) [])
         prev-state      (uix/state (get-result))]

     (uix/effect!
      (fn []
        (let [canceled? (atom false)]
          (ds/listen!
           conn listen-key
           (fn []
             (if-not @canceled?
               (let [next-state (get-result)]
                 (if-not (= @prev-state next-state)
                   (reset! prev-state next-state))))))
          (fn []
            (reset! canceled? true)
            (ds/unlisten! conn listen-key)))) [])

     [@prev-state dispatch])))

(defn create-dispatch [conn]
  (fn [event & args]
    (let [tx (apply txs/transact @conn event args)]
      (ds/transact! conn tx [event args tx]) nil)))

(defn provider
  "Provides a DataScript in-memory database to the application and causes
   re-renders when transactions are performed."
  [child]
  (let [conn (ds/conn-from-db (initial-data))]
    (uix/context-provider
     [state [conn (create-dispatch conn)]] child)))
