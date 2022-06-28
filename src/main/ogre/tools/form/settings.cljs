(ns ogre.tools.form.settings
  (:require [ogre.tools.events :refer [use-dispatch]]
            [ogre.tools.form.render :refer [form]]
            [ogre.tools.render :refer [checkbox]]
            [ogre.tools.state :refer [use-query]]))

(def ^{:private true} query
  [[:local/shortcuts? :default true]
   [:local/tooltips? :default true]
   {:local/window
    [[:window/show-grid :default true]
     [:window/snap-grid :default true]]}])

(defn ^{:private true} settings-form []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {:local/keys [shortcuts? tooltips? window]} result
        {:window/keys [show-grid snap-grid]} window]
    [:section
     [:header "Settings"]
     [:fieldset.setting
      [:label "Show Grid"]
      (for [value [false true] :let [checked? (= show-grid value)]]
        ^{:key value}
        [checkbox
         {:checked checked?
          :on-change
          (fn []
            (if (not checked?)
              (dispatch :window/change-grid-show value)))}
         (if value "Yes" "No")])]
     [:fieldset.setting
      [:label "Align to Grid"]
      (for [value [false true] :let [checked? (= snap-grid value)]]
        ^{:key value}
        [checkbox
         {:checked checked?
          :on-change
          (fn []
            (if (not checked?)
              (dispatch :window/change-grid-snap value)))}
         (if value "Yes" "No")])]
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
         (if display? "Yes" "No")])]]))

(defmethod form :settings []
  settings-form)
