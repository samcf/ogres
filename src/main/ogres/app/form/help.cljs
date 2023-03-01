(ns ogres.app.form.help
  (:require [ogres.app.env :refer [VERSION]]
            [ogres.app.form.render :refer [form]]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render :refer [checkbox]]))

(def ^{:private true} query
  [[:local/shortcuts? :default true]
   [:local/tooltips? :default true]])

(defn ^{:private true} help []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {:local/keys [shortcuts? tooltips?]} result]
    [:section
     [:fieldset.setting
      [:label "Show Shortcuts"]
      (for [display? [false true] :let [checked? (= display? shortcuts?)]]
        ^{:key display?}
        [checkbox
         {:checked checked?
          :on-change
          (fn []
            (if (not checked?)
              (dispatch :interface/toggle-shortcuts display?)))}
         (if display? "Yes" "No")])]
     [:fieldset.setting
      [:label "Show Tooltips"]
      (for [display? [false true] :let [checked? (= display? tooltips?)]]
        ^{:key display?}
        [checkbox
         {:checked checked?
          :on-change
          (fn []
            (if (not checked?)
              (dispatch :interface/toggle-tooltips display?)))}
         (if display? "Yes" "No")])]
     [:button.ogre-button {:on-click #(dispatch :storage/reset) :style {:margin-top 8}}
      "Restore Factory Defaults"]
     [:span "Current version: " [:strong VERSION]]]))

(defmethod form :help []
  help)
