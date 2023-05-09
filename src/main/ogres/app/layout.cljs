(ns ogres.app.layout
  (:require [ogres.app.hooks :refer [use-query]]
            [ogres.app.render.canvas :refer [render-canvas]]
            [ogres.app.render.panel :refer [container]]
            [ogres.app.render.toolbar :refer [toolbar]]
            [ogres.app.render.workspaces :refer [workspaces]]
            [uix.core :refer [defui $]]))

(def ^{:private true} query
  [[:local/type :default :conn]
   [:local/loaded? :default false]
   [:local/shortcuts? :default true]
   [:local/tooltips? :default true]])

(defui layout []
  (let [{:local/keys [loaded? type shortcuts? tooltips?]} (use-query query)]
    ($ :div.root {:data-view-type      (name type)
                  :data-show-shortcuts shortcuts?
                  :data-show-tooltips  tooltips?}
      (if loaded?
        (if (= type :view)
          ($ :div.layout {:style {:visibility "hidden"}}
            ($ :div.layout-canvas ($ render-canvas)))
          ($ :div.layout {:style {:visibility "hidden"}}
            (if (= type :host)
              ($ :div.layout-workspaces ($ workspaces)))
            ($ :div.layout-canvas  ($ render-canvas))
            ($ :div.layout-toolbar ($ toolbar))
            ($ :div.layout-panel   ($ container))))))))
