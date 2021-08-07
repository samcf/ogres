(ns ogre.tools.render.command
  (:require [uix.core.alpha :as uix]
            [datascript.core :as ds]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [context]]
            [ogre.tools.render.icon :refer [icon]]))

(defclass styles []
  {:display "flex"
   :flex-direction "column"}
  [:button
   {:background-color "var(--theme-background-d)"
    :border "none"
    :border-radius "4px"
    :padding-top "4px"
    :color "var(--theme-text)"
    :cursor "pointer"
    :height "36px"
    :width "36px"}
   [:&:hover
    {:background-color "rgba(255, 255, 255, 0.15)"}]]
  [:button+button {:margin-top "4px"}]
  [:hr
   {:width "80%"
    :border-width "1px 0 0 0"
    :border-color "rgba(255, 255, 255, 0.35)"}])

(defn command [props]
  (let [{:keys [dispatch]} (uix/context context)]
    [:div {:class (styles)}
     [:button {:type "button" :on-click #(dispatch :workspace/set-select-mode)} [icon {:name :cursor :width 20 :height 20}]]
     [:button {:type "button" :on-click #(dispatch :workspace/toggle-board-options)} [icon {:name :image :width 18 :height 18}]]
     [:button {:type "button" :on-click #(dispatch :workspace/toggle-grid-options)} [icon {:name :grid :width 21 :height 21}]]
     [:hr]
     [:button {:type "button" :on-click #(dispatch :zoom/in)} [icon {:name :zoom-in :width 16 :height 16}]]
     [:button {:type "button" :on-click #(dispatch :zoom/out)} [icon {:name :zoom-out :width 16 :height 16}]]]))
