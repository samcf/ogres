(ns ogre.tools.state
  (:require [uix.core.alpha :as uix :refer [defcontext]]
            [datascript.core :as ds :refer [squuid]]
            [ogre.tools.txs :as txs]))

(goog-define VERSION "latest")
(goog-define PATH "/release")

(def schema
  {:db/ident          {:db/unique :db.unique/identity}
   :entity/key        {:db/unique :db.unique/identity}
   :image/checksum    {:db/unique :db.unique/identity}
   :root/canvases     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/scenes       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/stamps       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/local        {:db/valueType :db.type/ref :db/isComponent true}
   :canvas/scene      {:db/valueType :db.type/ref}
   :canvas/tokens     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :canvas/shapes     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :canvas/masks      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :canvas/initiative {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :window/canvas     {:db/valueType :db.type/ref}
   :window/selected   {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :local/window      {:db/valueType :db.type/ref}
   :local/windows     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :token/stamp       {:db/valueType :db.type/ref}})

(defn initial-data []
  (ds/db-with
   (ds/empty-db schema)
   [[:db/add -1 :db/ident :root]
    [:db/add -1 :entity/key (squuid)]
    [:db/add -1 :root/release VERSION]
    [:db/add -1 :root/canvases -2]
    [:db/add -1 :root/local -3]

    [:db/add -2 :entity/key (squuid)]
    [:db/add -2 :element/name ""]

    [:db/add -3 :db/ident :local]
    [:db/add -3 :entity/key (squuid)]
    [:db/add -3 :local/window -4]
    [:db/add -3 :local/windows -4]

    [:db/add -4 :entity/key (squuid)]
    [:db/add -4 :window/canvas -2]]))

[{:root/conn
  [{:local/window
    [:window/vec]}]}]

(defcontext state)

(def root-query
  '[:find (pull $ ?id pattern) . :in $ pattern :where [?id :db/ident :root]])

(defn listening? [data]
  (let [result (ds/pull data [:local/host? :local/paused?] [:db/ident :local])]
    (or (:local/host? result) (not (:local/paused? result)))))

(defn use-query
  "React hook to run queries against the underlying DataScript database."
  ([] (let [[_ dispatch] (uix/context state)] dispatch))
  ([{:keys [query pull args] :or {query root-query args []}}]
   (let [[conn dispatch] (uix/context state)
         listen-key      (deref (uix/state (squuid)))
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
           (fn [{:keys [db-after]}]
             (if (and (listening? db-after) (not @canceled?))
               (let [next-state (get-result)]
                 (if-not (= @prev-state next-state)
                   (reset! prev-state next-state))))))
          (fn []
            (reset! canceled? true)
            (ds/unlisten! conn listen-key)))) [])

     [@prev-state dispatch])))

(defn use-pull
  ([] (let [[_ dispatch] (uix/context state)] dispatch))
  ([pattern] (use-pull pattern [:db/ident :root]))
  ([pattern entity]
   (let [[conn dispatch] (uix/context state)
         listen-key      (deref (uix/state (squuid)))
         get-result      (uix/callback #(ds/pull @conn pattern entity) [])
         prev-state      (uix/state (get-result))]
     (uix/effect!
      (fn []
        (let [canceled? (atom false)]
          (ds/listen!
           conn listen-key
           (fn [{:keys [db-after]}]
             (if (and (listening? db-after) (not @canceled?))
               (let [next-state (get-result)]
                 (if-not (= @prev-state next-state)
                   (reset! prev-state next-state))))))
          (fn []
            (reset! canceled? true)
            (ds/unlisten! conn listen-key)))) [])
     [@prev-state dispatch])))

(defn create-dispatch [conn]
  (fn [event & args]
    (let [select  [:entity/key {:local/window [:entity/key {:window/canvas [:entity/key]}]}]
          result  (ds/pull @conn select [:db/ident :local])
          {local :entity/key
           {window :entity/key
            {canvas :entity/key} :window/canvas} :local/window} result
          context {:data @conn :event event :local local :window window :canvas canvas}
          changes (apply txs/transact context args)]
      (ds/transact! conn changes [event args changes]) nil)))

(defn provider
  "Provides a DataScript in-memory database to the application and causes
   re-renders when transactions are performed."
  [child]
  (let [conn (ds/conn-from-db (initial-data))]
    (uix/context-provider
     [state [conn (create-dispatch conn)]] child)))
