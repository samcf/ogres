(ns ogres.app.provider.dispatch
  (:require [datascript.core :as ds]
            [ogres.app.events :refer [event-tx-fn]]
            [ogres.app.provider.events :as events]
            [ogres.app.provider.state :as state]
            [uix.core :as uix :refer [defui $]]))

(def ^:private context (uix/create-context))

(defn ^:private tx-fn [data event args]
  (apply event-tx-fn data event args))

(defn ^:private use-dispatch-fn []
  (let [publish (events/use-publish)
        conn    (uix/use-context state/context)]
    (uix/use-callback
     (fn [topic & args]
       (apply publish topic args)
       (let [tx-data [[:db.fn/call tx-fn topic args]]
             report  (ds/transact! conn tx-data)]
         (if (seq (:tx-data report))
           (publish :tx/commit report)))) [conn publish])))

(defn use-dispatch []
  (uix/use-context context))

(defui provider [{:keys [children]}]
  (let [dispatch (use-dispatch-fn)]
    ($ context {:value dispatch}
      children)))
