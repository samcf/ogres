(ns ogres.app.render.panel
  (:require [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render :refer [icon]]
            [ogres.app.form.render :refer [header form]]))

(def ^{:private true} panel-forms
  {:host [{:key :session    :label "Invite Friends" :figr "people-fill"}
          {:key :scenes     :label "Maps"           :figr "images"}
          {:key :tokens     :label "Tokens"         :figr "person-circle"}
          {:key :canvas     :label "Options"        :figr "wrench-adjustable-circle"}
          {:key :initiative :label "Initiative"     :figr "hourglass-split"}
          {:key :help       :label "Help"           :figr "question-diamond"}]
   :conn [{:key :session    :label "Online Session" :figr "people-fill"}
          {:key :initiative :label "Initiative"     :figr "hourglass-split"}
          {:key :help       :label "Help"           :figr "question-diamond"}]})

(def ^{:private true} query
  [[:local/type :default :conn]
   [:panel/expanded :default #{}]])

(defn container []
  (let [dispatch (use-dispatch)
        result (use-query query)
        {:keys [local/type panel/expanded]} result
        forms (panel-forms type)]
    [:section.panel
     [:div.panel-forms
      (for [{:keys [key label figr]} forms :let [expanded (contains? expanded key)]]
        [:div.panel-form
         {:key key
          :css {(str "panel-form-" (name key)) true
                "panel-form--expanded"         expanded
                "panel-form--collapsed"        (not expanded)}}
         [:div.panel-header
          {:on-click #(dispatch :interface/toggle-panel key)}
          [:<>
           [icon {:name figr :size 20}]
           [:div.panel-header-label label]
           [(header {:form key})]]]
         (if expanded
           [:div.panel-content [(form {:form key})]])])]]))
