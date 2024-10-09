(ns ogres.app.provider.storage
  (:require [datascript.core :as ds]
            [datascript.transit :as dt]
            [goog.functions :refer [throttle]]
            [ogres.app.const :refer [VERSION]]
            [ogres.app.dom :refer [user-type]]
            [ogres.app.provider.events :as events]
            [ogres.app.provider.idb :as idb]
            [ogres.app.provider.state :as state :refer [use-query]]
            [uix.core :as uix :refer [defui $]]))

(def ^:private context (uix/create-context))

(def ^:private ignored-attrs
  #{:user/type :user/status :user/sharing? :session/state})

(defui ^:private marshal []
  (let [conn  (uix/use-context state/context)
        write (idb/use-writer "app")]
    (uix/use-effect
     (fn []
       (ds/listen! conn :marshaller
                   (throttle
                    (fn [{:keys [db-after]}]
                      (if (= (:user/status (ds/entity db-after [:db/ident :user])) :ready)
                        (-> db-after
                            (ds/db-with [[:db/retract [:db/ident :session] :session/host]
                                         [:db/retract [:db/ident :session] :session/conns]])
                            (ds/filter (fn [_ [_ attr _ _]] (not (contains? ignored-attrs attr))))
                            (ds/datoms :eavt)
                            (dt/write-transit-str)
                            (as-> marshalled #js {:release VERSION :updated (* -1 (.now js/Date)) :data marshalled})
                            (as-> record (write :put [record]))))) 200))
       (fn [] (ds/unlisten! conn :marshaller)))) []))

(defui ^:private unmarshal []
  (let [conn (uix/use-context state/context)
        read (idb/use-reader "app")
        tx-data
        [[:db/add [:db/ident :user] :user/status :ready]
         [:db/add [:db/ident :user] :user/type (user-type)]]]
    (uix/use-effect
     (fn []
       (-> (read VERSION)
           (.then (fn [record]
                    (if (nil? record)
                      (ds/transact! conn tx-data)
                      (->
                       (.-data record)
                       (dt/read-transit-str)
                       (ds/conn-from-datoms state/schema)
                       (ds/db)
                       (ds/db-with tx-data)
                       (as-> data (ds/reset-conn! conn data))))))
           (.catch (fn [] (ds/transact! conn tx-data)))))) []))

(defui ^:private delete-data []
  (events/use-subscribe :storage/reset
    (uix/use-callback
     (fn []
       (let [delete (.deleteDatabase js/indexedDB "ogres.app")
             reload (fn [] (.. js/window -location reload))]
         (.addEventListener delete "blocked" reload)
         (.addEventListener delete "success" reload))) [])))

(defui ^:private delete-images []
  (let [write         (idb/use-writer "images")
        on-delete     (uix/use-callback (fn [image thumb] (write :delete [image thumb])) [write])
        on-delete-all (uix/use-callback (fn [hashes]      (write :delete hashes))        [write])]
    (events/use-subscribe :scene-images/remove on-delete)
    (events/use-subscribe :token-images/remove on-delete)
    (events/use-subscribe :token-images/remove-all on-delete-all)))

(defui handlers
  "Registers event handlers related to IndexedDB, such as those involved in
   saving and loading the application state." []
  (let [{type :user/type} (use-query [:user/type])]
    (case type
      :host ($ :<>
              ($ unmarshal)
              ($ marshal)
              ($ delete-images)
              ($ delete-data))
      :view ($ :<> ($ unmarshal))
      :conn ($ :<> ($ delete-images)))))
