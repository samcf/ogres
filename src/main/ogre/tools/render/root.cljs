(ns ogre.tools.render.root
  (:require [uix.core.alpha :as uix]
            [datascript.core :as ds]
            [datascript.transit :as dt]
            [ogre.tools.render :refer [context]]
            [ogre.tools.render.layout :refer [layout]]
            [ogre.tools.query :as query]
            [ogre.tools.txs :refer [schema transact]]))

(def ^{:private true} pk 1)

(def boundary
  (uix/create-error-boundary
   {:error->state (fn [error] {:error error})
    :handle-catch (fn [error info])}
   (fn [state [props child]]
     (let [{:keys [error]} @state]
       (if error
         [:div {:style {:padding "32px" :max-width 640}}
          [:p "The application crashed! Try refreshing the page. If the problem
               persists, click the button below to delete your local data. This
               may resolve your issue."]
          [:button {:on-click (:on-reset props)} "Delete your local data"]]
         child)))))

(defn root [props]
  (let [data   (uix/state (:data props))
        loaded (uix/state false)
        store  (:store props)
        value  {:data      @data
                :store     store
                :workspace (query/workspace @data)
                :dispatch  (fn [event & args]
                             (let [tx (apply transact @data event args)]
                               (reset! data (ds/db-with @data tx))))}]

    (uix/effect!
     (fn []
       (-> (.table store "states")
           (.get pk)
           (.then
            (fn [record]
              (reset! loaded true)
              (when-not (nil? record)
                (-> (.-data record)
                    (dt/read-transit-str)
                    (ds/conn-from-datoms schema)
                    (as-> conn
                          (reset! data @conn))))))))
     [loaded])

    (uix/effect!
     (fn []
       (when loaded
         (-> (ds/filter @data (constantly true))
             (ds/datoms :eavt)
             (dt/write-transit-str)
             (as-> marshalled
                   (-> (.table store "states")
                       (.put  #js {:id pk :data marshalled}))))))
     [@data])

    (uix/context-provider
     [context value]
     [boundary
      {:on-reset
       (fn []
         (-> (.table store "states")
             (.delete pk)
             (.then
              (.reload (.-location js/window)))))}
      [layout]])))
