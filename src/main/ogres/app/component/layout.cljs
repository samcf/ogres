(ns ogres.app.component.layout
  (:require [ogres.app.hooks :refer [use-query]]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.panel :refer [container]]
            [ogres.app.component.scene :refer [render-scene]]
            [ogres.app.component.scenes :refer [scenes]]
            [ogres.app.component.toolbar :refer [toolbar]]
            [uix.core :refer [defui $]]))

(def ^:private query
  [[:user/type :default :conn]
   [:user/status :default :none]
   [:panel/expanded :default true]])

(defui layout []
  (let [result (use-query query)
        {type :user/type
         status :user/status
         expanded :panel/expanded} result]
    (case [type status]
      [:host :ready]
      ($ :.layout
        {:style {:visibility "hidden"}
         :data-user (name type)
         :data-expanded expanded}
        ($ :.layout-scenes  ($ scenes))
        ($ :.layout-scene   ($ render-scene))
        ($ :.layout-toolbar ($ toolbar))
        ($ :.layout-panel   ($ container)))
      [:conn :ready]
      ($ :.layout
        {:style {:visibility "hidden"}
         :data-user (name type)
         :data-expanded expanded}
        ($ :.layout-scene   ($ render-scene))
        ($ :.layout-toolbar ($ toolbar))
        ($ :.layout-panel   ($ container)))
      [:conn :disconnected]
      ($ :.layout-error
        ($ :.layout-error-content
          ($ :div {:style {:margin-top 4 :color "hsl(6, 73%, 60%)"}}
            ($ icon {:name "exclamation-triangle-fill"}))
          ($ :div
            ($ :div {:style {:font-size 20 :line-height 1}}
              "Connection to the room could not be started or it was interrupted.")
            ($ :div {:style {:margin-top 16}}
              "Some possible reasons this might have happened:")
            ($ :ul {:style {:margin "0.25rem 1.2rem 1rem 1.2rem" :list-style-type "disc"}}
              ($ :li "The room you were in was closed by the host")
              ($ :li "The room was automatically closed due to inactivity")
              ($ :li "The room you tried to join does not exist")
              ($ :li "The host is using a map image that exceeds the server limit (10MB)")
              ($ :li "The server is undergoing maintenance"))
            ($ :div "Stay here and we'll keep trying to establish your connection every five seconds.")
            ($ :nav {:style {:margin-top 16 :color "var(--color-danger-500)"}}
              ($ :ul {:style {:display "flex" :gap 16}}
                ($ :li ($ :a {:href "/"} "Home"))
                ($ :li ($ :a {:href "https://github.com/samcf/ogres/wiki"} "Wiki"))
                ($ :li ($ :a {:href "https://github.com/samcf/ogres/discussions"} "Support")))))))
      [:view :ready]
      ($ :.layout
        {:style {:visibility "hidden"}
         :data-user (name type)
         :data-expanded expanded}
        ($ :.layout-scene ($ render-scene))) nil)))
