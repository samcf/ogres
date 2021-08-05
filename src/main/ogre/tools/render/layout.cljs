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
   {:border-right "1px solid var(--color-primary-d)"
    :box-sizing "content-box"
    :padding "8px"
    :width "36px"}]
  [:.content
   {:display "flex" :flex-direction "column" :flex 1}]
  [:.workspaces
   {:border-bottom "1px solid var(--color-primary-d)" :padding "8px 0 0 8px"}]
  [:.workspace
   {:background-color "var(--theme-background-d)" :position "relative" :flex "1"}]
  [:.canvas :.options :.vignette :.tokens
   {:pointer-events "none" :position "absolute" :top 0 :right 0 :bottom 0 :left 0}]
  [:.vignette
   {:box-shadow "inset 0 0 32px rgba(0, 0, 0, 0.90)"}]
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
       [:div.vignette]
       [:div.options [options]]
       [:div.tokens [tokens]]]]]))
