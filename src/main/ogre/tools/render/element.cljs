(ns ogre.tools.render.element
  (:require [rum.core :as rum]))

(defn element [props & children]
  (let [{:keys [element dispatch]} props]
    (case (:element/type (:element props))
      :workspace
      [:g
       #_[:rect {:x 0 :y 0 :fill "url(#grid)" :width 640 :height 640}]
       #_[:path {:d "M 640.5 0 L 640.5 640.5 0 640.5" :fill "none" :stroke "black" :stroke-width 0.5}]
       (when-let [url (-> element :workspace/map :map/url)]
         [:image {:x 0 :y 0 :href url}])
       children]
      nil)))
