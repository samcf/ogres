(ns ogres.app.layout
  (:require [ogres.app.hooks :refer [use-query]]
            [ogres.app.render.panel :refer [container]]
            [ogres.app.render.scene :refer [render-scene]]
            [ogres.app.render.scenes :refer [scenes]]
            [ogres.app.render.toolbar :refer [toolbar]]
            [ogres.app.render.join :refer [join]]
            [uix.core :refer [defui $]]))

(def ^:private query
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
            ($ :div.layout-scene ($ render-scene)))
          ($ :div.layout {:style {:visibility "hidden"}}
            (if (= type :host)
              ($ :div.layout-scenes ($ scenes)))
            ($ :div.layout-join    ($ join))
            ($ :div.layout-scene   ($ render-scene))
            ($ :div.layout-toolbar ($ toolbar))
            ($ :div.layout-panel   ($ container))))))))
