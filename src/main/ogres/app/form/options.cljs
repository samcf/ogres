(ns ogres.app.form.options
  (:require [ogres.app.form.render :as render]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render :refer [icon]]))

(def ^:private options-lighting
  [[:revealed "Revealed" "eye-fill"]
   [:dimmed   "Obscured" "eye"]
   [:hidden   "Hidden"   "eye-slash-fill"]])

(def ^:private options-weather
  [[:none     "None"     "sun-fill"]
   [:dusk     "Dusk"     "cloud-sun-fill"]
   [:midnight "Midnight" "moon-stars-fill"]])

(def ^:private query
  [{:local/window
    [[:window/label :default ""]
     [:window/draw-mode :default :select]
     {:window/canvas
      [:entity/key
       [:canvas/dark-mode :default false]
       [:canvas/grid-size :default 70]
       [:canvas/lighting :default :revealed]
       [:canvas/show-grid :default true]
       [:canvas/snap-grid :default false]
       [:canvas/timeofday :default :none]
       {:canvas/image
        [:image/checksum]}]}]}])

(defn- form []
  (let [dispatch (use-dispatch)
        result   (use-query query [:db/ident :local])
        {window :local/window
         {canvas :window/canvas
          {{checksum :image/checksum} :canvas/image}
          :window/canvas}
         :local/window} result]
    [:section.options
     [:section
      [:fieldset
       [:input
        {:type "text"
         :placeholder "Name of this canvas..."
         :maxLength 36
         :spellCheck "false"
         :value (:window/label window)
         :on-change
         (fn [event]
           (let [value (.. event -target -value)]
             (dispatch :window/change-label value)))}]]]
     [:section.options-grid
      [:fieldset
       [:label "Show grid"]
       [:input
        {:id "show-grid"
         :type "checkbox"
         :checked (:canvas/show-grid canvas)
         :on-change
         (fn [event]
           (let [checked (.. event -target -checked)]
             (dispatch :canvas/toggle-show-grid checked)))}]
       [:label {:for "show-grid"}
        [icon {:name "grid-fill" :size 16}]]]
      [:fieldset
       [:label "Align to grid"]
       [:input
        {:id "align-grid"
         :type "checkbox"
         :checked (:canvas/snap-grid canvas)
         :on-change
         (fn [event]
           (let [checked (.. event -target -checked)]
             (dispatch :canvas/toggle-snap-grid checked)))}]
       [:label {:for "align-grid"}
        [icon {:name "bounding-box" :size 16}]]]
      [:fieldset
       [:label "Use dark grid"]
       [:input
        {:id "dark-mode"
         :type "checkbox"
         :checked (:canvas/dark-mode canvas)
         :on-change
         (fn [event]
           (let [checked (.. event -target -checked)]
             (dispatch :canvas/toggle-dark-mode checked)))}]
       [:label {:for "dark-mode"}
        [icon {:name "lightbulb-fill" :size 16}]]]
      [:fieldset
       [:label "Visibility"]
       [:div.options-group
        (for [[option label icon-name] options-lighting
              :let [key (str "lighting-" (name option))]]
          [:fieldset {:key key}
           [:input
            {:id key
             :type "radio"
             :checked (= (:canvas/lighting canvas) option)
             :disabled (not checksum)
             :on-change #(dispatch :canvas/change-lighting option)}]
           [:label {:for key :data-tooltip label}
            [icon {:name icon-name :size 18}]]])]]
      [:fieldset
       [:label "Time of Day"]
       [:div.options-group
        (for [[option label icon-name] options-weather
              :let [key (str "time-of-day-" (name option))]]
          [:fieldset {:key key}
           [:input
            {:id key
             :type "radio"
             :checked (= option (:canvas/timeofday canvas))
             :disabled (not checksum)
             :on-change #(dispatch :canvas/change-time-of-day option)}]
           [:label {:for key :data-tooltip label}
            [icon {:name icon-name :size 18}]]])]]
      [:fieldset
       [:label "Tile size (px)"]
       [:input
        {:type "number"
         :value (or (:canvas/grid-size canvas) 0)
         :placeholder "Grid size"
         :disabled (not checksum)
         :on-change #(dispatch :canvas/change-grid-size (.. %1 -target -value))}]]]]))

(defmethod render/form :canvas [] form)
