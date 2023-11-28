(ns ogres.app.provider.state
  (:require [datascript.core :as ds]
            [ogres.app.const :refer [VERSION]]
            [ogres.app.provider.events :refer [use-publish]]
            [ogres.app.events :refer [event-tx-fn]]
            [uix.core :refer [defui $ create-context use-context use-callback use-state use-effect]]))

(def schema
  {:db/ident          {:db/unique :db.unique/identity}
   :root/local        {:db/valueType :db.type/ref :db/isComponent true}
   :root/scene-images {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/scenes       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/session      {:db/valueType :db.type/ref :db/isComponent true}
   :root/token-images {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :session/conns     {:db/valueType :db.type/ref :db.cardinality :db.cardinality/many :db/isComponent true}
   :session/host      {:db/valueType :db.type/ref}
   :image/checksum    {:db/unique :db.unique/identity}
   :local/uuid        {:db/unique :db.unique/identity}
   :local/camera      {:db/valueType :db.type/ref}
   :local/cameras     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :camera/scene      {:db/valueType :db.type/ref}
   :camera/selected   {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :scene/image       {:db/valueType :db.type/ref}
   :scene/initiative  {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :scene/masks       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/shapes      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/tokens      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :token/image       {:db/valueType :db.type/ref}
   :initiative/turn   {:db/valueType :db.type/ref}})

(defn local-type []
  (let [search (.. js/window -location -search)
        params (js/URLSearchParams. search)]
    (cond (= (.get params "share") "true") :view
          (string? (.get params "join"))   :conn
          :else                            :host)))

(defn ^:private initial-data []
  (ds/db-with
   (ds/empty-db schema)
   [[:db/add -1 :db/ident :root]
    [:db/add -1 :root/release VERSION]
    [:db/add -1 :root/scenes -2]
    [:db/add -1 :root/local -3]
    [:db/add -1 :root/session -5]
    [:db/add -2 :db/empty true]
    [:db/add -3 :db/ident :local]
    [:db/add -3 :local/status :none]
    [:db/add -3 :local/color "#03a9f4"]
    [:db/add -3 :local/camera -4]
    [:db/add -3 :local/cameras -4]
    [:db/add -3 :local/type (local-type)]
    [:db/add -3 :panel/expanded #{:tokens}]
    [:db/add -4 :camera/scene -2]
    [:db/add -5 :db/ident :session]]))

(def context
  (create-context (ds/conn-from-db (initial-data))))

(defonce ^:private context-value
  (ds/conn-from-db (initial-data)))

(defui provider
  "Provides a DataScript in-memory database to the application and causes
   re-renders when transactions are performed."
  [{:keys [children]}]
  ($ (.-Provider context) {:value context-value} children))

(defn ^:private listening?
  [data]
  (let [select [:local/type :local/paused?]
        {:keys [local/type local/paused?]} (ds/pull data select [:db/ident :local])]
    (or (= type :host) (not paused?))))

(defn ^:private tx-fn [data event args]
  (apply event-tx-fn data event args))

(defn use-query
  ([pattern]
   (use-query pattern [:db/ident :local]))
  ([pattern entity-id]
   (let [conn                   (use-context context)
         get-result             (use-callback #(ds/pull @conn pattern entity-id) ^:lint/disable [])
         [listen-key]           (use-state random-uuid)
         [prev-state set-state] (use-state get-result)]
     (use-effect
      (fn []
        (ds/listen!
         conn listen-key
         (fn [{:keys [db-after]}]
           (if (listening? db-after)
             (let [next-state (get-result)]
               (if (not= prev-state next-state)
                 (set-state next-state))))))
        (fn []
          (ds/unlisten! conn listen-key))) ^:lint/disable [prev-state])
     prev-state)))

(defn use-dispatch []
  (let [publish (use-publish)
        conn    (use-context context)]
    (use-callback
     (fn [topic & args]
       (publish {:topic topic :args args})
       (let [tx-data [[:db.fn/call tx-fn topic args]]
             report  (ds/transact! conn tx-data)]
         (if (seq (:tx-data report))
           (publish {:topic :tx/commit :args (list report)})))) ^:lint/disable [publish])))
