(ns ogre.tools.render.panel
  (:require [ogre.tools.render :refer [button checkbox]]
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
        {:keys [panel/curr panel/collapsed?]} workspace]
    [:aside.panel
     {:css {:panel--collapsed collapsed?
            :panel--expanded (not collapsed?)}}
     [:nav.panel-tabs
      (for [[panel icon-name] panels]
        [:div.panel-tab
         {:key panel
          :css {:selected (and (not collapsed?) (= panel curr))}
          :on-click #(dispatch :interface/change-panel panel)}
         [icon {:name icon-name}]])
      [:div.panel-tab
       {:on-click #(dispatch :interface/toggle-panel)}
       [icon
        {:name
         (if collapsed?
           :chevron-double-left
           :chevron-double-right)}]]]
     (if (not collapsed?)
       [:div.panel-content {:css (->> curr name (str "panel-content-"))}
        (let [component (form {:form curr})]
          [component])])]))
