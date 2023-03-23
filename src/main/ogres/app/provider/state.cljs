(ns ogres.app.provider.state
  (:require [uix.core.alpha :as uix :refer [defcontext]]
            [datascript.core :as ds :refer [squuid]]
            [ogres.app.env :as env]
            [ogres.app.provider.events :refer [use-publish]]
            [ogres.app.txs :refer [transact]]))

(def schema
  {:db/ident          {:db/unique :db.unique/identity}
   :entity/key        {:db/unique :db.unique/identity}
   :canvas/image      {:db/valueType :db.type/ref}
   :canvas/initiative {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :canvas/masks      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :canvas/shapes     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :canvas/tokens     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :image/checksum    {:db/unique :db.unique/identity}
   :local/window      {:db/valueType :db.type/ref}
   :local/windows     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/canvases     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/local        {:db/valueType :db.type/ref :db/isComponent true}
   :root/session      {:db/valueType :db.type/ref :db/isComponent true}
   :root/scenes       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/stamps       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :session/host      {:db/valueType :db.type/ref}
   :session/conns     {:db/valueType :db.type/ref :db.cardinality :db.cardinality/many :db/isComponent true}
   :token/image       {:db/valueType :db.type/ref}
   :window/canvas     {:db/valueType :db.type/ref}
   :window/selected   {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}})

(defn local-type []
  (let [search (.. js/window -location -search)
        params (js/URLSearchParams. search)]
    (cond (= (.get params "share") "true") :view
          (string? (.get params "join"))   :conn
          :else                            :host)))

(defn initial-data []
  (ds/db-with
   (ds/empty-db schema)
   [[:db/add -1 :db/ident :root]
    [:db/add -1 :root/release env/VERSION]
    [:db/add -1 :root/canvases -2]
    [:db/add -1 :root/local -3]
    [:db/add -1 :root/session -5]
    [:db/add -2 :entity/key (squuid)]
    [:db/add -3 :db/ident :local]
    [:db/add -3 :entity/key (squuid)]
    [:db/add -3 :local/loaded? false]
    [:db/add -3 :local/color "#03a9f4"]
    [:db/add -3 :local/window -4]
    [:db/add -3 :local/windows -4]
    [:db/add -3 :local/type (local-type)]
    [:db/add -3 :panel/expanded #{:tokens}]
    [:db/add -4 :entity/key (squuid)]
    [:db/add -4 :window/canvas -2]
    [:db/add -5 :db/ident :session]]))

(defcontext context (ds/conn-from-db (initial-data)))

(defonce context-value (ds/conn-from-db (initial-data)))

(defn provider
  "Provides a DataScript in-memory database to the application and causes
   re-renders when transactions are performed."
  [child]
  (uix/context-provider [context context-value] child))

(defn listening? [data]
  (let [select [:local/type :local/paused?]
        {:keys [local/type local/paused?]} (ds/pull data select [:db/ident :local])]
    (or (= type :host) (not paused?))))

(defn use-query
  ([pattern]
   (use-query pattern [:db/ident :local]))
  ([pattern entity]
   (let [conn       (uix/context context)
         listen-key (deref (uix/state (squuid)))
         get-result (uix/callback #(ds/pull @conn pattern entity) [])
         prev-state (uix/state (get-result))]
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
     @prev-state)))

(defn use-dispatch []
  (let [conn    (uix/context context)
        query   [:entity/key {:local/window [:entity/key {:window/canvas [:entity/key]}]}]
        publish (use-publish)
        result  (use-query query)
        context {:local  (:entity/key result)
                 :window (:entity/key (:local/window result))
                 :canvas (:entity/key (:window/canvas (:local/window result)))}]
    (fn [topic & args]
      (publish {:topic topic :args args})
      (let [context (assoc context :data @conn :event topic)
            tx-data (apply transact context args)]
        (if (seq tx-data)
          (let [report (ds/transact! conn tx-data)]
            (publish {:topic :tx/commit :args (list report)})))))))
