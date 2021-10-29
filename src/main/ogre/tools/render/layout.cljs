(ns ogre.tools.render.layout
  (:require ogre.tools.form.core
            [ogre.tools.render :refer [css]]
            [ogre.tools.render.canvas :refer [canvas]]
            [ogre.tools.render.command :refer [command]]
            [ogre.tools.render.panel :refer [container]]
            [ogre.tools.state :refer [use-query]]
            [ogre.tools.render.tokens :refer [tokens]]
            [ogre.tools.render.controls :as controls]
            [ogre.tools.render.workspaces :refer [workspaces]]))

(def attrs
  [:root/loaded?
   :root/host?
   [:root/shortcuts? :default false]
   [:root/tooltips? :default false]])

(defn layout []
  (let [[result] (use-query {:pull attrs})
        {:root/keys [loaded? host? shortcuts? tooltips?]} result
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
         [:div.layout-controls [controls/container]]
         [:div.layout-panel [container]]]
        [:div.layout {:class (css classes)}
         [:div.layout-canvas [canvas]]]))))
