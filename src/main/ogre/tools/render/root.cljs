(ns ogre.tools.render.root
  (:require [uix.core.alpha :as uix]
            [datascript.core :as ds]
            [datascript.transit :as dt]
            [ogre.tools.render :refer [context]]
            [ogre.tools.render.layout :refer [layout]]
            [ogre.tools.query :as query]
            [ogre.tools.txs :refer [schema transact]]))

(def ^{:private true} pk 1)

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

    (uix/context-provider [context value] [layout])))
