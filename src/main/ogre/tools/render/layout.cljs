(ns ogre.tools.render.layout
  (:require [ogre.tools.render :refer [css context]]
            [ogre.tools.render.tokens :refer [tokens]]
            [ogre.tools.render.canvas :refer [canvas]]
            [ogre.tools.render.command :refer [command]]
            [ogre.tools.render.options :refer [options]]
            [ogre.tools.render.workspaces :refer [workspaces]]
            [ogre.tools.query :as query]
            [datascript.core :as ds]
            [uix.core.alpha :as uix]))

(defn layout []
  (let [context (uix/context context)
        viewer  (ds/entity (:data context) [:db/ident :viewer])]
    (when (:viewer/loaded? viewer)
      (if (and (:viewer/host? viewer))
        [:div.layout
         [:div]
         [:div.layout-workspaces [workspaces]]
         [:div.layout-command [command]]
         [:div.layout-workspace
          [:div.layout-canvas [canvas]]
          [:div.layout-options [options]]
          [:div.layout-tokens [tokens]]]]
        [:div.layout.layout--guest
         [:div.layout-workspace
          [:div.layout-canvas [canvas]]]]))))
