(ns ogres.app.component.layout
  (:require [ogres.app.hooks :as hooks]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.panel :as panel]
            [ogres.app.component.scene :refer [scene]]
            [ogres.app.component.scenes :refer [scenes]]
            [ogres.app.component.toolbar :refer [toolbar]]
            [ogres.app.component.players :refer [players]]
            [ogres.app.provider.window :as window]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [[:user/ready :default false]
   [:user/host :default true]
   [:panel/expanded :default true]
   [:session/status :default :none]])

(defui ^:memo layout []
  (let [result (hooks/use-query query)
        {host     :user/host
         ready    :user/ready
         status   :session/status
         expanded :panel/expanded} result
        node (uix/use-context window/context)]
    (cond (and host ready)
          ($ :.layout
            {:data-user "host" :data-expanded expanded}
            ($ :.layout-scenes  ($ scenes))
            ($ :.layout-scene {:ref node}
              ($ scene))
            ($ :.layout-toolbar ($ toolbar))
            ($ :.layout-players ($ players))
            ($ :.layout-panel   ($ panel/panel)))
          (and (not host) (= status :connected) ready)
          ($ :.layout
            {:data-user "conn" :data-expanded expanded}
            ($ :.layout-scene {:ref node}
              ($ scene))
            ($ :.layout-toolbar ($ toolbar))
            ($ :.layout-players ($ players))
            ($ :.layout-panel   ($ panel/panel)))
          (and (not host) (= status :disconnected))
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
                    ($ :li ($ :a {:href "https://github.com/samcf/ogres/discussions"} "Support"))))))))))
