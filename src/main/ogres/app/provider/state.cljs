(ns ogres.app.provider.state
  (:require [datascript.core :as ds]
            [ogres.app.const :refer [VERSION]]
            [uix.core :refer [defui $ create-context use-context use-callback use-state use-effect]]))

(def schema
  {:db/ident          {:db/unique :db.unique/identity}
   :root/user         {:db/valueType :db.type/ref :db/isComponent true}
   :root/scene-images {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/scenes       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/session      {:db/valueType :db.type/ref :db/isComponent true}
   :root/token-images {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :session/conns     {:db/valueType :db.type/ref :db.cardinality :db.cardinality/many :db/isComponent true}
   :session/host      {:db/valueType :db.type/ref}
   :image/checksum    {:db/unique :db.unique/identity}
   :user/uuid         {:db/unique :db.unique/identity}
   :user/camera       {:db/valueType :db.type/ref}
   :user/cameras      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :user/dragging     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :camera/scene      {:db/valueType :db.type/ref}
   :camera/selected   {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :scene/image       {:db/valueType :db.type/ref}
   :scene/initiative  {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :scene/masks       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/shapes      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/tokens      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :token/image       {:db/valueType :db.type/ref}
   :initiative/turn   {:db/valueType :db.type/ref}
   :initiative/played {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}})

(defn initial-data [type]
  (ds/db-with
   (ds/empty-db schema)
   [[:db/add -1 :db/ident :root]
    [:db/add -1 :root/release VERSION]
    [:db/add -1 :root/scenes -2]
    [:db/add -1 :root/user -3]
    [:db/add -1 :root/session -5]
    [:db/add -2 :db/empty true]
    [:db/add -3 :db/ident :user]
    [:db/add -3 :user/status :none]
    [:db/add -3 :user/color "red"]
    [:db/add -3 :user/camera -4]
    [:db/add -3 :user/cameras -4]
    [:db/add -3 :user/type type]
    [:db/add -3 :panel/selected :tokens]
    [:db/add -4 :camera/scene -2]
    [:db/add -5 :db/ident :session]]))

(def context (create-context (ds/conn-from-db (initial-data :host))))

(defui provider
  "Provides a DataScript in-memory database to the application and causes
   re-renders when transactions are performed."
  [{:keys [children type] :or {type :host}}]
  (let [conn (ds/conn-from-db (initial-data type))]
    ($ (.-Provider context) {:value conn} children)))

(defn use-query
  ([pattern]
   (use-query pattern [:db/ident :user]))
  ([pattern entity-id]
   (let [conn                   (use-context context)
         get-result             (use-callback #(ds/pull @conn pattern entity-id) ^:lint/disable [])
         [listen-key]           (use-state random-uuid)
         [prev-state set-state] (use-state get-result)]
     (use-effect
      (fn []
        (ds/listen!
         conn listen-key
         (fn []
           (let [next-state (get-result)]
             (if (not= prev-state next-state)
               (set-state next-state)))))
        (fn []
          (ds/unlisten! conn listen-key))) ^:lint/disable [prev-state])
     prev-state)))
