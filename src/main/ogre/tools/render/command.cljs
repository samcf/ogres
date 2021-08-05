(ns ogre.tools.render.command
  (:require [uix.core.alpha :as uix]
            [datascript.core :as ds]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [context]]))

(defclass styles []
  {:display "flex"
   :flex-direction "column"}
  [:button
   {:background-color "transparent"
    :border-radius "2px"
    :border "1px solid var(--color-primary-a)"
    :color "var(--theme-text)"
    :cursor "pointer"
    :height "36px"}]
  [:button+button {:margin-top "8px"}])

(defn command [props]
  (let [{:keys [dispatch]} (uix/context context)]
    [:div {:class (styles)}
     [:button
      {:type "button"
       :on-click #(dispatch :workspace/toggle-board-options)} "B"]
     [:button
      {:type "button"
       :on-click #(dispatch :workspace/toggle-grid-options)} "G"]]))
