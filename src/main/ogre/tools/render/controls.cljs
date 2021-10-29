(ns ogre.tools.render.controls
  (:require [ogre.tools.render.icon :refer [icon]]
            [ogre.tools.state :refer [use-query]]))

(def query
  {:pull
   [[:share/open? :default false]
    [:share/paused? :default false]]})

(defn container []
  (let [[data dispatch] (use-query query)
        {:share/keys [open? paused?]} data]
    (if open?
      [:div.controls
       [:div.controls-control
        {:css {:disabled (not paused?)} :on-click #(if paused? (dispatch :share/switch))}
        [icon {:name :play-fill :size 32}]]
       [:div.controls-control
        {:css {:disabled paused?} :on-click #(if (not paused?) (dispatch :share/switch))}
        [icon {:name :pause-fill :size 32}]]])))
