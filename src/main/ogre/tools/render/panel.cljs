(ns ogre.tools.render.panel
  (:require [ogre.tools.render :refer [icon]]
            [ogre.tools.form.render :refer [form]]
            [ogre.tools.state :refer [use-query]]))

(def panels
  [[:canvas "images"]
   [:initiative "hourglass-split"]
   [:help "question-diamond"]])

(def attrs
  [{:root/canvas
    [[:panel/curr :default :canvas]
     [:panel/collapsed? :default false]]}])

(defn container []
  (let [[result dispatch] (use-query {:pull attrs})
        {{:keys [panel/curr panel/collapsed?]} :root/canvas} result]
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
           "chevron-double-left"
           "chevron-double-right")}]]]
     (if (not collapsed?)
       [:div.panel-content {:css (->> curr name (str "panel-content-"))}
        (let [component (form {:form curr})]
          [component])])]))
