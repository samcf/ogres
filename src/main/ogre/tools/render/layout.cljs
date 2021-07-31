(ns ogre.tools.render.layout
  (:require [rum.core :as rum]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [css context]]
            [ogre.tools.render.camera :refer [camera]]
            [ogre.tools.render.command :refer [command]]
            [ogre.tools.render.element :refer [element]]
            [ogre.tools.render.viewing :refer [viewing]]
            [ogre.tools.render.workspaces :refer [workspaces]]
            [ogre.tools.query :as query]))

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
  [:.canvas
   {:height "100%" :width "100%"}]
  [:.vignette
   {:box-shadow "inset 0 0 32px rgba(0, 0, 0, 0.90)"}]
  [:.viewing
   {:display "flex" :flex-direction "column"}]
  [:.vignette :.viewing
   {:pointer-events "none"
    :position "absolute"
    :top "0"
    :right "0"
    :bottom "0"
    :left "0"}])

(rum/defc layout [props & children]
  (rum/with-context [{:keys [data dispatch]} context]
    (let [viewer (query/viewer data)
          workspace (:viewer/workspace viewer)]
      [:div {:class (styles)}
       [:div.command (command {:workspace workspace})]
       [:div.content
        [:div.workspaces (workspaces {:workspace workspace})]
        [:div.workspace
         [:svg.canvas
          [:defs
           [:pattern {:id "grid" :width 64 :height 64 :patternUnits "userSpaceOnUse"}
            [:path {:d "M 64 0 L 0 0 0 64" :stroke "black" :stroke-width "1" :fill "none"}]]]
          (camera
           {:element workspace :dispatch dispatch}
           (element {:element workspace}))]
         [:div.vignette]
         [:div.viewing (viewing {:workspace workspace})]]]])))
