(ns ogres.app.core
  (:require [ogres.app.env              :refer [PATH]]
            [ogres.app.layout           :refer [layout]]
            [ogres.app.provider.events  :as events]
            [ogres.app.provider.portal  :as portal]
            [ogres.app.provider.state   :as state]
            [ogres.app.provider.storage :as storage]
            [ogres.app.provider.window  :as window]
            [ogres.app.render           :refer [error-boundary]]
            [ogres.app.session          :as session]
            [ogres.app.shortcut         :as shortcut]
            [react-helmet :refer [Helmet]]
            [uix.core :refer [defui $]]
            [uix.dom :refer [create-root render-root]]
            ["@dnd-kit/core" :refer [DndContext]
             :rename {DndContext dnd-context}]))

(defui app []
  ($ :<>
    ($ Helmet
      ($ :link {:rel "stylesheet" :href (str PATH "/reset.css")})
      ($ :link {:rel "stylesheet" :href (str PATH "/ogres.app.css")}))
    ($ events/provider
      ($ state/provider
        ($ storage/provider
          ($ portal/provider
            ($ :<>
              ($ storage/handlers)
              ($ window/provider)
              ($ shortcut/handlers)
              ($ session/handlers)
              ($ error-boundary
                ($ dnd-context
                  ($ layout))))))))))

(defn main []
  (let [elem (.querySelector js/document "#root")
        root (create-root elem)]
    (render-root ($ app) root)))
