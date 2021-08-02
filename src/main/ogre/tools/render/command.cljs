(ns ogre.tools.render.command
  (:require [datascript.core :as ds]
            [rum.core :as rum]
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

(rum/defc command [props & children]
  (rum/with-context [{:keys [data dispatch]} context]
    (let [{:keys [workspace]} props]
      [:div {:class (styles)}
       [:button
        {:type "button"
         :on-click #(dispatch :view/toggle)} "B"]])))
