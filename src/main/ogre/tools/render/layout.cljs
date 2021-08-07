(ns ogre.tools.render.layout
  (:require [spade.core :refer [defclass]]
            [ogre.tools.render :refer [css context]]
            [ogre.tools.render.tokens :refer [tokens]]
            [ogre.tools.render.canvas :refer [canvas]]
            [ogre.tools.render.command :refer [command]]
            [ogre.tools.render.options :refer [options]]
            [ogre.tools.render.workspaces :refer [workspaces]]
            [ogre.tools.query :as query]
            [uix.core.alpha :as uix]))

(defclass styles []
  {:display "grid"
   :grid-template-rows "minmax(36px, auto) 1fr"
   :grid-template-columns "0fr 1fr"
   :background-color "hsl(208deg 21% 20%)"
   :color "var(--theme-text)"
   :height "100%"}
  [:.command
   {:box-sizing "content-box"
    :padding "0 8px"}]
  [:.workspaces
   {:margin "8px 0 4px 0"}]
  [:.workspace :.canvas :.canvas>svg
   {:border-radius "6px"}]
  [:.workspace
   {:background-color "var(--theme-background-d)"
    :box-shadow "2px 2px 16px rgba(0, 0, 0, 0.65)"
    :margin "0 16px 16px 0"
    :position "relative"}]
  [:.canvas :.options :.tokens
   {:pointer-events "none" :position "absolute" :top 0 :right 0 :bottom 0 :left 0}]
  [:.options
   {:display "flex" :flex-direction "column"}]
  [:.tokens
   {:display "flex" :justify-content "flex-end"}])

(defn layout [props child]
  (let [{:keys [data]} (uix/context context)]
    [:div {:class (styles)}
     [:div ""]
     [:div.workspaces [workspaces]]
     [:div.command [command]]
     [:div.workspace
      [:div.canvas [canvas]]
      [:div.options [options]]
      [:div.tokens [tokens]]]]))
