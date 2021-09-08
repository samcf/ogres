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
        initiative? (not (empty? (query/initiating (:data context))))]
    (cond
      (and (:viewer/loaded? viewer) (:viewer/host? viewer))
      [:div.layout
       {:class (css {:layout--initiative initiative?})}
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
      (and (:viewer/loaded? viewer) (not (:viewer/host? viewer)))
      [:div.layout.layout--guest
       [:div.layout-workspace
        [:div.layout-canvas [canvas]]]]
      :else nil)))
