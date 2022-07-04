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
  (let [{:local/keys [loaded? type shortcuts? tooltips?]} (use-query query)]
    [:div.root {:data-view-type      (name type)
                :data-show-shortcuts shortcuts?
                :data-show-tooltips  tooltips?}
     (if loaded?
       (if (= type :view)
         [:div.layout {:style {:visibility "hidden"}}
          [:div.layout-canvas [canvas]]]
         [:div.layout {:style {:visibility "hidden"}}
          (if (= type :host)
            [:div.layout-workspaces [workspaces]])
          [:div.layout-canvas [canvas]]
          [create-portal
           (fn [ref]
             [:div.layout-modal {:ref ref}]) :modal]
          [:div.layout-toolbar [toolbar]]
          [:div.layout-tokens [tokens]]
          [:div.layout-panel [container]]]))]))
