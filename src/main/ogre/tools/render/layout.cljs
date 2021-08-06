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
  {:display "flex"
   :background-color "var(--theme-background-a)"
   :color "var(--theme-text)"
   :height "100%"}
  [:.command
   {:box-sizing "content-box"
    :padding "8px"
    :width "36px"}]
  [:.content
   {:display "flex" :flex-direction "column" :flex 1}]
  [:.workspaces
   {:padding "8px 0 0 8px"}]
  [:.workspace :.canvas :.canvas>svg
   {:border-radius "6px"}]
  [:.workspace
   {:background-color "var(--theme-background-d)"
    :border "1px solid transparent"
    :box-shadow "0 0 16px rgba(0, 0, 0, 0.65)"
    :flex "1"
    :margin "8px 16px 16px 8px"
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
     [:div.command [command]]
     [:div.content
      [:div.workspaces [workspaces]]
      [:div.workspace
       [:div.canvas [canvas]]
       [:div.options [options]]
       [:div.tokens [tokens]]]]]))
