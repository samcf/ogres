(ns ogre.tools.state
  (:require [uix.core.alpha :as uix :refer [defcontext]]
            [datascript.core :as ds]
            [ogre.tools.txs :as txs]))

(def schema
  {:db/ident          {:db/unique :db.unique/identity}
   :root/tokens       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/canvases     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/scenes       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/canvas       {:db/valueType :db.type/ref}
   :canvas/scene      {:db/valueType :db.type/ref}
   :canvas/tokens     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :canvas/shapes     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :canvas/selected   {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :canvas/initiative {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :image/checksum    {:db/unique :db.unique/identity}
   :element/flags     {:db/cardinality :db.cardinality/many}})

(defn initial-data []
  (ds/db-with
   (ds/empty-db schema)
   [[:db/add -1 :db/ident :root]
    [:db/add -1 :root/loaded? false]
    [:db/add -1 :root/canvases -2]
    [:db/add -1 :root/canvas -2]
    [:db/add -1 :root/tokens -3]
    [:db/add -1 :root/host? true]
    [:db/add -1 :root/shortcuts? true]
    [:db/add -1 :root/tooltips? true]
    [:db/add -1 :bounds/self [0 0 0 0]]
    [:db/add -1 :bounds/host [0 0 0 0]]
    [:db/add -1 :bounds/guest [0 0 0 0]]
    [:db/add -2 :db/ident :canvas]
    [:db/add -2 :element/type :canvas]
    [:db/add -2 :canvas/mode :select]
    [:db/add -2 :canvas/lighting :bright]
    [:db/add -2 :canvas/theme :light]
    [:db/add -2 :pos/vec [0 0]]
    [:db/add -2 :grid/size 70]
    [:db/add -2 :grid/origin [0 0]]
    [:db/add -2 :grid/show true]
    [:db/add -2 :zoom/scale 1]
    [:db/add -2 :panel/curr :canvas]
    [:db/add -2 :panel/prev :canvas]
    [:db/add -3 :element/type :token]
    [:db/add -3 :token/light [5 5]]
    [:db/add -3 :token/size {:name :medium :size 5}]
    [:db/add -3 :aura/label ""]
    [:db/add -3 :aura/radius 0]]))

(defcontext state)

(def root-query
  '[:find (pull $ ?id pattern) . :in $ pattern :where [?id :db/ident :root]])

(defn use-query
  "React hook to run queries against the underlying DataScript database."
  ([] (let [[_ dispatch] (uix/context state)] [dispatch]))
  ([{:keys [query pull args] :or {query root-query args []}}]
   (let [[conn dispatch] (uix/context state)
         result (apply ds/q query @conn (if pull (conj args pull) args))]
     [result dispatch])))

(defn provider
  "Provides a DataScript in-memory database to the application and causes
   re-renders when transactions are performed."
  [child]
  (let [bean     (uix/state 0)
        pawn     (uix/state (ds/conn-from-db (initial-data)))
        conn     (deref pawn)
        dispatch (uix/callback
                  (fn [event & args]
                    (let [tx (apply txs/transact @conn event args)]
                      (ds/transact! conn tx [event args tx]))) [])]

    (uix/effect!
     (fn []
       (ds/listen!
        conn :rerender
        (fn [{:keys [db-after]}]
          (let [data (ds/pull db-after [:root/host? :share/paused?] [:db/ident :root])]
            (if (or (:root/host? data) (not (:share/paused? data)))
              (swap! bean inc)))))
       (fn [] (ds/unlisten! conn :rerender))) [nil])

    (uix/context-provider
     [state [conn dispatch]] child)))
