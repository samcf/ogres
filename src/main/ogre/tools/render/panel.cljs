(ns ogre.tools.render.panel
  (:require [ogre.tools.render :refer [button checkbox css]]
            [ogre.tools.form.render :refer [form]]
            [ogre.tools.query :as query]
            [ogre.tools.render.icon :refer [icon]]
            [ogre.tools.state :refer [state]]
            [uix.core.alpha :as uix]))

(defn container []
  (let [{:keys [dispatch workspace]} (uix/context state)
        {:keys [panel/selected]} workspace]
    [:aside.panel
     [:nav.panel-tabs
      (for [[panel icon-name]
            [[:canvas :images]
             [:initiative :hourglass-split]
             [:token :person-circle]
             [:shape :triangle]
             [:help :question-diamond]]]
        [:div
         {:key panel
          :class (css {:panel-tab true :selected (= panel selected)})
          :on-click #(dispatch :interface/change-panel panel)}
         [icon {:name icon-name}]])
      [:div.panel-tab [icon {:name :chevron-double-right}]]]
     [:div {:class (css "panel-content" (str "panel-content-" (name selected)))}
      [form {:form selected}]]]))
