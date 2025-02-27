(ns ogres.app.provider.state
  (:require [datascript.core :as ds]
            [datascript.transit :as dt]
            [goog.functions :refer [throttle]]
            [ogres.app.const :refer [VERSION]]
            [ogres.app.provider.events :as events]
            [ogres.app.provider.idb :as idb]
            [uix.core :as uix :refer [defui $]]))

(def schema
  {:camera/scene      {:db/valueType :db.type/ref}
   :camera/selected   {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :db/ident          {:db/unique :db.unique/identity}
   :image/hash        {:db/unique :db.unique/identity}
   :image/thumbnail   {:db/valueType :db.type/ref :db/isComponent true}
   :initiative/played {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :initiative/turn   {:db/valueType :db.type/ref}
   :root/scene-images {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/scenes       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/session      {:db/valueType :db.type/ref :db/isComponent true}
   :root/token-images {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :root/user         {:db/valueType :db.type/ref :db/isComponent true}
   :scene/image       {:db/valueType :db.type/ref}
   :scene/initiative  {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :scene/masks       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/shapes      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/tokens      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :scene/notes       {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :session/conns     {:db/valueType :db.type/ref :db.cardinality :db.cardinality/many :db/isComponent true}
   :session/host      {:db/valueType :db.type/ref}
   :token/image       {:db/valueType :db.type/ref}
   :user/camera       {:db/valueType :db.type/ref}
   :user/cameras      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :user/dragging     {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :user/uuid         {:db/unique :db.unique/identity}})

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
    [:db/add -3 :user/ready false]
    [:db/add -3 :user/color "red"]
    [:db/add -3 :user/camera -4]
    [:db/add -3 :user/cameras -4]
    [:db/add -3 :user/type type]
    [:db/add -3 :panel/selected :tokens]
    [:db/add -4 :camera/scene -2]
    [:db/add -5 :db/ident :session]]))

(def context (uix/create-context))

(defui ^:private listeners []
  (let [write (idb/use-writer "images")]
    ;; Removes the given scene image and its thumbnail from the
    ;; IndexedDB images object store.
    (events/use-subscribe :scene-images/remove
      (uix/use-callback
       (fn [& hashes] (write :delete hashes)) [write]))

    ;; Removes the given token image and its thumbnail from the
    ;; IndexedDB images object store.
    (events/use-subscribe :token-images/remove
      (uix/use-callback
       (fn [& hashes] (write :delete hashes)) [write]))

    ;; Removes the given token images from the IndexedDB images
    ;; object store.
    (events/use-subscribe :token-images/remove-all
      (uix/use-callback
       (fn [hashes] (write :delete hashes)) [write]))))

(def ^:private ignored-attrs
  #{:user/type :user/ready :user/sharing? :session/status})

(defui ^:private persistence [{:keys [type]}]
  (let [conn  (uix/use-context context)
        read  (idb/use-reader "app")
        write (idb/use-writer "app")]
    ;; Persists the DataScript state to IndexedDB whenever changes
    ;; are made to it.
    (uix/use-effect
     (fn []
       (ds/listen! conn :marshaller
         (throttle
          (fn [{:keys [db-after]}]
            (if (:user/ready (ds/entity db-after [:db/ident :user]))
              (-> db-after
                  (ds/db-with [[:db/retract [:db/ident :session] :session/host]
                               [:db/retract [:db/ident :session] :session/conns]])
                  (ds/filter (fn [_ [_ attr _ _]] (not (contains? ignored-attrs attr))))
                  (ds/datoms :eavt)
                  (dt/write-transit-str)
                  (as-> marshalled #js {:release VERSION :updated (* -1 (.now js/Date)) :data marshalled})
                  (as-> record (write :put [record])))))
          600))
       (fn [] (ds/unlisten! conn :marshaller))) [conn write])

    ;; Reads existing state from IndexedDB, if it exists, and replaces
    ;; the DataScript state with it.
    (uix/use-effect
     (fn []
       (let [tx-data
             [[:db/add [:db/ident :user] :user/ready true]
              [:db/add [:db/ident :user] :user/type type]]]
         (.then (read VERSION)
                (fn [record]
                  (if (nil? record)
                    (ds/transact! conn tx-data)
                    (-> (.-data record)
                        (dt/read-transit-str)
                        (ds/conn-from-datoms schema)
                        (ds/db)
                        (ds/db-with tx-data)
                        (as-> data (ds/reset-conn! conn data)))))))) ^:lint/disable [])))

(defui provider
  "Provides a DataScript in-memory database to the application and causes
   re-renders when transactions are performed."
  [{:keys [children type] :or {type :host}}]
  (let [[conn] (uix/use-state (ds/conn-from-db (initial-data type)))]
    ($ context {:value conn}
      (if (or (= type :host) (= type :view))
        ($ persistence {:type type}))
      ($ listeners)
      children)))

(defn use-query
  ([pattern]
   (use-query pattern [:db/ident :user]))
  ([pattern entity-id]
   (let [conn                   (uix/use-context context)
         get-result             (uix/use-callback #(ds/pull @conn pattern entity-id) ^:lint/disable [])
         [listen-key]           (uix/use-state random-uuid)
         [prev-state set-state] (uix/use-state get-result)]
     (uix/use-effect
      (fn []
        (ds/listen! conn listen-key
          (fn []
            (let [next-state (get-result)]
              (if (not= prev-state next-state)
                (set-state next-state)))))
        (fn []
          (ds/unlisten! conn listen-key))) ^:lint/disable [prev-state])
     prev-state)))
