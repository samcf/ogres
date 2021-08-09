(ns ogre.tools.render.command
  (:require [uix.core.alpha :as uix]
            [datascript.core :as ds]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [context css]]
            [ogre.tools.render.icon :refer [icon]]))

(defclass styles []
  {:display "flex"
   :flex-direction "column"}
  [:button
   {:background-color "var(--theme-background-d)"
    :border "none"
    :border-radius "4px"
    :color "var(--theme-text)"
    :cursor "pointer"
    :height "36px"
    :padding-top "4px"
    :width "36px"}
   [:&:hover
    {:background-color "rgba(255, 255, 255, 0.15)"}]
   [:&.selected
    {:background-color "var(--color-primary-a)"}
    [:svg {:fill "black"}]
    [:div {:border-color "black"}]]]
  [:button+button :button+a :a+button
   {:margin-top "4px"}]
  [:div.grid
   {:border "1px dashed white"
    :height "16px"
    :margin "auto"
    :margin-top "-4px"
    :width "16px"}]
  [:hr
   {:width "80%"
    :border-width "1px 0 0 0"
    :border-color "rgba(255, 255, 255, 0.35)"}])

(defn command [props]
  (let [{:keys [workspace dispatch store]} (uix/context context)
        {:keys [canvas/mode grid/show]} workspace]
    [:div {:class (styles)}
     [:button {:type "button" :title "Select" :class (css {:selected (= mode :select)}) :on-click #(dispatch :canvas/select-mode)}
      [icon {:name :cursor :width 20 :height 20}]]
     [:button {:type "button" :title "Canvas Settings" :class (css {:selected (= mode :board)}) :on-click #(dispatch :canvas/toggle-canvas-options)}
      [icon {:name :image :width 18 :height 18}]]
     [:button {:type "button" :title "Grid Settings" :class (css {:selected (= mode :grid)}) :on-click #(dispatch :canvas/toggle-grid-options)}
      [icon {:name :grid :width 21 :height 21}]]
     [:button {:type "button" :title "Ruler" :class (css {:selected (= mode :ruler)}) :on-click #(dispatch :canvas/toggle-ruler)}
      [icon {:name :ruler :width 20 :height 20}]]
     [:hr]
     [:button {:type "button" :title "Toggle Grid" :class (css {:selected show}) :on-click #(dispatch :grid/toggle)}
      [:div.grid]]
     [:hr]
     [:button {:type "button" :title "Zoom In" :on-click #(dispatch :zoom/in)}
      [icon {:name :zoom-in :width 18 :height 18}]]
     [:button {:type "button" :title "Zoom Out" :on-click #(dispatch :zoom/out)}
      [icon {:name :zoom-out :width 18 :height 18}]]
     [:hr]
     [:a {:href "https://www.github.com/samcf/ogre.tools" :title "Project home" :target "_blank"}
      [:button {:type "button" :on-click #(dispatch nil)}
       [icon {:name :github :width 20 :height 20}]]]
     [:button
      {:type "button"
       :title "Reset Local Storage"
       :on-click
       (fn []
         (.delete store)
         (.reload (.-location js/window)))}
      [icon {:name :recycle :width 20 :height 20}]]]))
