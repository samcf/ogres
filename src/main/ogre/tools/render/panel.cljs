(ns ogre.tools.render.panel
  (:require [ogre.tools.render :refer [button checkbox css]]
            [ogre.tools.form.render :refer [form]]
            [ogre.tools.query :as query]
            [ogre.tools.render.icon :refer [icon]]
            [ogre.tools.state :refer [state]]
            [uix.core.alpha :as uix]))

(def panels
  [[:canvas :images]
   [:initiative :hourglass-split]
   [:token :person-circle]
   [:shape :triangle]
   [:help :question-diamond]])

(defn container []
  (let [{:keys [dispatch workspace]} (uix/context state)
        {:keys [panel/current panel/collapsed?]} workspace]
    [:aside
     {:class
      (css {:panel true
            :panel--collapsed collapsed?
            :panel--expanded  (not collapsed?)})}
     [:nav.panel-tabs
      (for [[panel icon-name] panels]
        [:div
         {:key panel
          :class (css {:panel-tab true :selected (and (not collapsed?) (= panel current))})
          :on-click #(dispatch :interface/change-panel panel)}
         [icon {:name icon-name}]])
      [:div.panel-tab
       {:on-click #(dispatch :interface/toggle-panel)}
       [icon {:name (if collapsed? :chevron-double-left :chevron-double-right)}]]]
     (if (not collapsed?)
       (let [class (str "panel-content-" (name current))]
         [:div {:class (css "panel-content" class)}
          [form {:form current}]]))]))
