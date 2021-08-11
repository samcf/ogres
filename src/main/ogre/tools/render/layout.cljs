(ns ogre.tools.render.layout
  (:require [ogre.tools.render :refer [css context]]
            [ogre.tools.render.tokens :refer [tokens]]
            [ogre.tools.render.canvas :refer [canvas]]
            [ogre.tools.render.command :refer [command]]
            [ogre.tools.render.options :refer [options]]
            [ogre.tools.render.workspaces :refer [workspaces]]
            [ogre.tools.query :as query]
            [uix.core.alpha :as uix]))

(defn layout [props child]
  (let [{:keys [data]} (uix/context context)]
    [:div.layout
     [:div]
     [:div.layout-workspaces [workspaces]]
     [:div.layout-command [command]]
     [:div.layout-workspace
      [:div.layout-canvas [canvas]]
      [:div.layout-options [options]]
      [:div.layout-tokens [tokens]]]]))
