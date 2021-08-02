(ns ogre.tools.render.viewing
  (:require [uix.core.alpha :as uix]
            [ogre.tools.render :refer [context]]
            [ogre.tools.render.options :refer [options]]))

(defn viewing []
  (let [{:keys [data workspace dispatch]} (uix/context context)]
    (when-let [element (:workspace/viewing workspace)]
      [options {:element element :dispatch dispatch}])))
