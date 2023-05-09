(ns ogres.app.provider.state
  (:require [datascript.core :as ds :refer [squuid]]
            [ogres.app.env :as env]
            [ogres.app.provider.events :refer [use-publish]]
            [ogres.app.txs :refer [transact]]
            [uix.core :refer [defui $ create-context use-context use-callback use-state use-effect]]))

(def schema
  {:db/key            {:db/unique :db.unique/identity}
   :db/ident          {:db/unique :db.unique/identity}
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
   :window/selected   {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :initiative/turn   {:db/valueType :db.type/ref}})

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
    [:db/add -2 :db/key (squuid)]
    [:db/add -3 :db/ident :local]
    [:db/add -3 :db/key (squuid)]
    [:db/add -3 :local/loaded? false]
    [:db/add -3 :local/color "#03a9f4"]
    [:db/add -3 :local/window -4]
    [:db/add -3 :local/windows -4]
    [:db/add -3 :local/type (local-type)]
    [:db/add -3 :panel/expanded #{:tokens}]
    [:db/add -4 :db/key (squuid)]
    [:db/add -4 :window/canvas -2]
    [:db/add -5 :db/ident :session]]))

(def context (create-context (ds/conn-from-db (initial-data))))

(defonce context-value (ds/conn-from-db (initial-data)))

(defui provider
  "Provides a DataScript in-memory database to the application and causes
   re-renders when transactions are performed."
  [{:keys [children]}]
  ($ (.-Provider context) {:value context-value} children))

(defn listening? [data]
  (let [select [:local/type :local/paused?]
        {:keys [local/type local/paused?]} (ds/pull data select [:db/ident :local])]
    (or (= type :host) (not paused?))))

(defn use-query
  ([pattern]
   (use-query pattern [:db/ident :local]))
  ([pattern entity-id]
   (let [conn                   (use-context context)
         get-result             (use-callback #(ds/pull @conn pattern entity-id) ^:lint/disable [])
         [listen-key]           (use-state (random-uuid))
         [prev-state set-state] (use-state (get-result))]
     (use-effect
      (fn []
        (let [canceled? (atom false)]
          (ds/listen!
           conn listen-key
           (fn [{:keys [db-after]}]
             (if (and (listening? db-after) (not @canceled?))
               (let [next-state (get-result)]
                 (if (not= prev-state next-state)
                   (set-state next-state))))))
          (fn []
            (reset! canceled? true)
            (ds/unlisten! conn listen-key)))) ^:lint/disable [prev-state])
     prev-state)))

(defn use-dispatch []
  (let [conn    (use-context context)
        query   [:db/key {:local/window [:db/key {:window/canvas [:db/key]}]}]
        publish (use-publish)
        result  (use-query query)
        {local :db/key
         {window :db/key
          {canvas :db/key} :window/canvas} :local/window} result]
    (use-callback
     (fn [topic & args]
       (publish {:topic topic :args args})
       (let [context (hash-map :data @conn :event topic :local local :window window :canvas canvas)
             tx-data (apply transact context args)]
         (if (seq tx-data)
           (let [report (ds/transact! conn tx-data)]
             (publish {:topic :tx/commit :args (list report)}))))) ^:lint/disable [publish local window canvas])))
