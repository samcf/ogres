(ns ogre.tools.render.panel
  (:require [ogre.tools.render :refer [icon]]
            [ogre.tools.form.render :refer [form]]
            [ogre.tools.state :refer [use-query]]))

(def ^{:private true} panel-icon
  {:canvas     "images"
   :initiative "hourglass-split"
   :session    "people-fill"
   :help       "question-diamond"
   :settings   "wrench-adjustable-circle"
   :data       "sd-card"})

(def ^{:private true} panel-forms
  {:host [{:form :canvas}
          {:form :session}
          {:form :initiative}
          {:form :settings}
          {:form :data}
          {:form :help}]
   :conn [{:form :session}
          {:form :initiative}
          {:form :settings}
          {:form :help}]})

(def ^{:private true} query
  [:local/type
   [:panel/current :default :canvas]
   [:panel/collapsed? :default false]])

(defn container []
  (let [[result dispatch] (use-query query)
        {:keys [local/type panel/current panel/collapsed?]} result
        forms    (panel-forms type)
        selected (or current (:form (first forms)))]
    [:section.panel
     {:css {:panel--collapsed collapsed? :panel--expanded (not collapsed?)}}
     [:nav.panel-tabs
      (for [{:keys [form]} forms]
        [:div.panel-tab
         {:key form
          :css {:selected (and (not collapsed?) (= form selected))}
          :on-click #(dispatch :interface/change-panel form)}
         [icon {:name (panel-icon form)}]])
      [:div.panel-tab
       {:on-click #(dispatch :interface/toggle-panel)}
       [icon {:name (if collapsed? "chevron-double-left" "chevron-double-right")}]]]
     (if (not collapsed?)
       [:div.panel-content {:css (str "panel-content-" (name selected))}
        [(form {:form selected})]])]))
