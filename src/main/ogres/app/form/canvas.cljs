(ns ogres.app.form.canvas
  (:require [clojure.string :refer [capitalize]]
            [ogres.app.form.render :refer [form]]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render :refer [checkbox icon]]))

(def ^{:private true} query
  [{:local/window
    [[:window/label :default ""]
     [:window/draw-mode :default :select]
     {:window/canvas
      [:entity/key
       [:canvas/dark-mode :default :light]
       [:canvas/grid-size :default 70]
       [:canvas/lighting :default :revealed]
       [:canvas/show-grid :default true]
       [:canvas/snap-grid :default false]
       [:canvas/timeofday :default :none]]}]}])

(defn ^{:private true} canvas []
  (let [dispatch    (use-dispatch)
        result      (use-query query [:db/ident :local])
        {window :local/window
         {canvas :window/canvas} :local/window} result]
    [:<>
     [:section
      [:fieldset.form-canvas-profile
       [:input
        {:type "text"
         :placeholder "New Canvas"
         :maxLength 36
         :spellCheck "false"
         :value (:window/label window)
         :on-change
         (fn [event]
           (let [value (.. event -target -value)]
             (dispatch :window/change-label value)))}]]]
     [:section
      [:fieldset.setting
       [:label "Show Grid"]
       (for [value [false true] :let [checked? (= (:canvas/show-grid canvas) value)]]
         ^{:key value}
         [checkbox
          {:checked checked?
           :on-change
           (fn []
             (if (not checked?)
               (dispatch :canvas/toggle-show-grid value)))}
          (if value "Yes" "No")])]
      [:fieldset.setting
       [:label "Align to Grid"]
       (for [value [false true] :let [checked? (= (:canvas/snap-grid canvas) value)]]
         ^{:key value}
         [checkbox
          {:checked checked?
           :on-change
           (fn []
             (if (not checked?)
               (dispatch :canvas/toggle-snap-grid value)))}
          (if value "Yes" "No")])]
      [:fieldset.setting
       [:label "Dark Mode"]
       (for [[value label] [[:light "Off"] [:dark "On"]]
             :let [checked? (= value (:canvas/dark-mode canvas))]]
         ^{:key value}
         [checkbox
          {:checked checked?
           :on-change
           (fn []
             (if (not checked?)
               (dispatch :canvas/toggle-dark-mode value)))} label])]
      [:fieldset.setting
       [:label "Lighting"]
       (for [value [:revealed :dimmed :hidden]
             :let [checked (= value (:canvas/lighting canvas))]]
         ^{:key value}
         [checkbox
          {:checked checked :on-change #(dispatch :canvas/change-lighting value)}
          (capitalize (name value))])]
      [:fieldset.setting
       [:label "Time of Day"]
       (for [value [:none :dusk :midnight] :let [checked? (= value (:canvas/timeofday canvas))]]
         ^{:key value}
         [checkbox
          {:checked checked? :on-change #(dispatch :canvas/change-time-of-day value)}
          (capitalize (name value))])]]
     [:section.form-canvas-grid
      [:fieldset.group
       [:input
        {:type "number"
         :placeholder "Grid size"
         :value (or (:canvas/grid-size canvas) 0)
         :on-change
         (fn [event]
           (dispatch :canvas/change-grid-size (.. event -target -value)))}]
       [checkbox
        {:checked (= (:window/draw-mode window) :grid)
         :on-change
         (fn [checked]
           (if checked
             (dispatch :window/change-mode :grid)
             (dispatch :window/change-mode :select)))}
        [icon {:name "crop" :size 16}]]]]]))

(defmethod form :canvas []
  canvas)
