(ns ogre.tools.render.root
  (:require [uix.core.alpha :as uix]
            [datascript.core :as ds]
            [ogre.tools.render :refer [context]]
            [ogre.tools.render.layout :refer [layout]]
            [ogre.tools.query :as query]
            [ogre.tools.txs :refer [transact]]))

(defn root [props]
  (let [data  (uix/state (:data props))
        value {:data      @data
               :store     (:store props)
               :workspace (query/workspace @data)
               :dispatch  (fn [event & args]
                            (let [tx (apply transact @data event args)]
                              (reset! data (ds/db-with @data tx))))}]
    (uix/context-provider [context value] [layout])))
