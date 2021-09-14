(ns ogre.tools.render.layout
  (:require [ogre.tools.query :as query]
            [ogre.tools.render :refer [css]]
            [ogre.tools.render.canvas :refer [canvas]]
            [ogre.tools.render.command :refer [command]]
            [ogre.tools.render.initiative :refer [initiative]]
            [ogre.tools.render.options :refer [options]]
            [ogre.tools.render.tokens :refer [tokens]]
            [ogre.tools.render.workspaces :refer [workspaces]]
            [ogre.tools.state :refer [state]]
            [uix.core.alpha :as uix]))

(defn layout []
  (let [context     (uix/context state)
        viewer      (query/viewer (:data context))
        initiative? (not (empty? (query/initiating (:data context))))
        class-names (css {:global--host       (:viewer/host? viewer)
                          :global--guest      (not (:viewer/host? viewer))
                          :global--shortcuts  (:viewer/shortcuts? viewer)
                          :global--tooltips   (:viewer/tooltips? viewer)
                          :global--initiative initiative?})]
    (if (:viewer/loaded? viewer)
      (if (:viewer/host? viewer)
        [:div.layout {:class class-names}
         [:div.layout-workspaces [workspaces]]
         [:div.layout-container
          [:div.layout-workspace
           [:div.layout-canvas [canvas]]
           [:div.layout-overlay
            [:div.layout-command [command]]
            [:div.layout-options [options]]
            [:div.layout-tokens [tokens]]]]
          (when initiative?
            [:div.layout-initiative [initiative]])]]
        [:div.layout {:class class-names}
         [:div.layout-workspace
          [:div.layout-canvas [canvas]]]]))))
