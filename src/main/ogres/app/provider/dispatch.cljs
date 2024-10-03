(ns ogres.app.provider.dispatch
  (:require [datascript.core :as ds]
            [ogres.app.events :refer [event-tx-fn]]
            [ogres.app.provider.events :as events]
            [ogres.app.provider.state :as state]
            [uix.core :refer [$ create-context defui use-callback use-context]]))

(def ^:private context
  (create-context nil))

(defn ^:private tx-fn [data event args]
  (apply event-tx-fn data event args))

(defn ^:private use-dispatch-fn []
  (let [publish (events/use-publish)
        conn    (use-context state/context)]
    (use-callback
     (fn [topic & args]
       (apply publish topic args)
       (let [tx-data [[:db.fn/call tx-fn topic args]]
             report  (ds/transact! conn tx-data)]
         (if (seq (:tx-data report))
           (apply publish :tx/commit (list report))))) ^:lint/disable [publish])))

(defn use-dispatch []
  (use-context context))

(defui provider [{:keys [children]}]
  (let [dispatch (use-dispatch-fn)]
    ($ context {:value dispatch}
      children)))
