(ns ogre.tools.render.layout
  (:require [ogre.tools.render.tokens :refer [tokens]]
            [ogre.tools.render.canvas :refer [canvas]]
            [ogre.tools.render.command :refer [command]]
            [ogre.tools.render.options :refer [options]]
            [ogre.tools.render.workspaces :refer [workspaces]]
            [ogre.tools.state :refer [state]]
            [ogre.tools.query :as query]
            [uix.core.alpha :as uix]))

(defn layout []
  (let [{:keys [data]} (uix/context state)
        {:keys [viewer/host? viewer/loaded?]} (query/viewer data)]
    (cond
      (and loaded? host?)
      [:div.layout
       [:div.layout-workspaces [workspaces]]
       [:div.layout-workspace
        [:div.layout-canvas [canvas]]
        [:div.layout-overlay
         [:div.layout-command [command]]
         [:div.layout-options [options]]
         [:div.layout-tokens [tokens]]]]]

      (and loaded?)
      [:div.layout.layout--guest
       [:div.layout-workspace
        [:div.layout-canvas [canvas]]]]

      :else nil)))
