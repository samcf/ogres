(ns ogre.tools.render.viewing
  (:require [rum.core :as rum]
            [ogre.tools.render :refer [context]]
            [ogre.tools.render.options :refer [options]]))

(rum/defc viewing [props & children]
  (rum/with-context [{:keys [data dispatch]} context]
    (let [{:keys [workspace]} props]
      [:div
       (for [element (:workspace/viewing workspace)]
         (rum/with-key (options {:element element :dispatch dispatch})
           (:db/id element)))])))
