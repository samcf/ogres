(ns ogre.tools.render.layout
  (:require ogre.tools.form.core
            [ogre.tools.render :refer [css]]
            [ogre.tools.render.canvas :refer [canvas]]
            [ogre.tools.render.command :refer [command]]
            [ogre.tools.render.panel :refer [container]]
            [ogre.tools.state :refer [use-query]]
            [ogre.tools.render.tokens :refer [tokens]]
            [ogre.tools.render.workspaces :refer [workspaces]]))

(def attrs
  [:viewer/loaded?
   :viewer/host?
   :viewer/shortcuts?
   :viewer/tooltips?])

(defn layout []
  (let [[result] (use-query {:pull attrs})
        {:viewer/keys [loaded? host? shortcuts? tooltips?]} result
        classes
        {:global--host       host?
         :global--guest      (not host?)
         :global--shortcuts  shortcuts?
         :global--tooltips   tooltips?}]
    (if loaded?
      (if host?
        [:div.layout {:class (css classes)}
         [:div.layout-workspaces [workspaces]]
         [:div.layout-canvas [canvas]]
         [:div.layout-command [command]]
         [:div.layout-tokens [tokens]]
         [:div.layout-panel [container]]]
        [:div.layout {:class (css classes)}
         [:div.layout-canvas [canvas]]]))))
