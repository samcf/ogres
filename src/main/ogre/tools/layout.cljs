(ns ogre.tools.layout
  (:require [ogre.tools.hooks :refer [use-query create-portal]]
            [ogre.tools.render.canvas :refer [canvas]]
            [ogre.tools.render.panel :refer [container]]
            [ogre.tools.render.tokens :refer [tokens]]
            [ogre.tools.render.toolbar :refer [toolbar]]
            [ogre.tools.render.workspaces :refer [workspaces]]))

(def ^{:private true} query
  [[:local/type :default :conn]
   [:local/loaded? :default false]
   [:local/shortcuts? :default true]
   [:local/tooltips? :default true]])

(defn layout []
  (let [result (use-query query)
        {:local/keys [loaded? type shortcuts? tooltips?]} result
        attrs {:data-view-type      (name type)
               :data-show-shortcuts shortcuts?
               :data-show-tooltips  tooltips?}]
    (if loaded?
      (if (= type :view)
        [:div.root.layout attrs [:div.layout-canvas [canvas]]]
        [:div.root.layout attrs
         (if (= type :host)
           [:div.layout-workspaces [workspaces]])
         [:div.layout-canvas [canvas]]
         [create-portal
          (fn [ref]
            [:div.layout-modal {:ref ref}]) :modal]
         [:div.layout-toolbar [toolbar]]
         [:div.layout-tokens [tokens]]
         [:div.layout-panel [container]]]))))
