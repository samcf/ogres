(ns ogres.app.render.forms
  (:require [clojure.string :refer [capitalize]]
            [ogres.app.hooks :refer [use-dispatch]]
            [ogres.app.render :refer [icon]]
            [ogres.app.render.pattern :refer [pattern]]
            [uix.core :as uix :refer [defui $ use-effect use-ref use-state]]))

(defn token-size [x]
  (cond (<= x 3)  "Tiny"
        (<  x 5)  "Small"
        (<= x 5)  "Medium"
        (<= x 10) "Large"
        (<= x 15) "Huge"
        (>  x 15) "Gargantuan"
        :else     "Unknown"))

(def ^:private token-conditions
  [{:value :player        :icon "people-fill"}
   {:value :blinded       :icon "eye-slash-fill"}
   {:value :charmed       :icon "arrow-through-heart-fill"}
   {:value :defeaned      :icon "ear-fill"}
   {:value :exhausted     :icon "moon-stars-fill"}
   {:value :frightened    :icon "black-cat"}
   {:value :grappled      :icon "fist"}
   {:value :incapacitated :icon "emoji-dizzy"}
   {:value :invisible     :icon "incognito"}
   {:value :petrified     :icon "gem"}
   {:value :poisoned      :icon "droplet-fill"}
   {:value :prone         :icon "falling"}
   {:value :restrained    :icon "spiderweb"}
   {:value :stunned       :icon "stars"}
   {:value :unconscious   :icon "skull"}])

(def ^:private shape-colors
  [{:value "#ffeb3b" :label "Yellow"}
   {:value "#ff9800" :label "Orange"}
   {:value "#f44336" :label "Red"}
   {:value "#673ab7" :label "Purple"}
   {:value "#2196f3" :label "Blue"}
   {:value "#009688" :label "Green"}
   {:value "#8bc34a" :label "Light green"}
   {:value "#ffffff" :label "White"}
   {:value "#9e9e9e" :label "Gray"}
   {:value "#000000" :label "Black"}])

(def ^:private shape-patterns
  [{:value :solid   :label "None"}
   {:value :lines   :label "Lines"}
   {:value :circles :label "Circles"}
   {:value :crosses :label "Crosses"}
   {:value :caps    :label "Caps"}
   {:value :waves   :label "Waves"}])

(defn ^:private stop-propagation [event]
  (.stopPropagation event))

(defui ^:private context-menu
  [{:keys [render-toolbar children]}]
  (let [[selected set-selected] (use-state nil)
        props {:selected  selected
               :on-change (fn [form]
                            (if (= selected form)
                              (set-selected nil)
                              (set-selected form)))}]
    ($ :.context-menu {:on-pointer-down stop-propagation}
      ($ :.context-menu-toolbar
        (render-toolbar props))
      (if selected
        ($ :.context-menu-form
          {:class (str "context-menu-form-" (name selected))}
          (children props))))))

(defui ^:private checkbox
  [{:keys [checked children]}]
  (let [input (use-ref)
        indtr (= checked :indeterminate)]
    (use-effect
     (fn [] (set! (.-indeterminate @input) indtr)) [indtr])
    (children input)))

(defui ^:private token-form-label
  [{:keys [values on-change on-close]
    :or   {values    (constantly (list))
           on-change identity
           on-close  identity}}]
  (let [input-ref (use-ref)
        [input-val set-input-val]
        (use-state
         (fn []
           (let [vs (values :token/label)]
             (if (= (count vs) 1) (first vs) ""))))]
    (use-effect #(.select @input-ref) [])
    ($ :form
      {:on-submit
       (fn [event]
         (.preventDefault event)
         (on-change :token/change-label input-val)
         (on-close))}
      ($ :input.text.text-ghost
        {:type "text"
         :ref input-ref
         :value input-val
         :auto-focus true
         :placeholder "Change token label..."
         :on-change #(set-input-val (.. %1 -target -value))})
      ($ :button {:type "submit"}
        ($ icon {:name "check" :size 22})))))

(defui ^:private token-form-details
  [{:keys [on-change values]
    :or   {values (constantly (list)) on-change identity}}]
  (let [value-fn (fn [attr] (first (into (sorted-set-by >) (values attr))))]
    ($ :<>
      (let [value (value-fn :token/size)]
        ($ :<>
          ($ :label "Size")
          ($ :button
            {:type "button"
             :auto-focus true
             :on-click #(on-change :token/change-size (max (- value 5) 5))
             :aria-label "Decrease token size by 5 feet"}
            "-")
          ($ :data {:value value}
            (str value  "ft. " (token-size value)))
          ($ :button
            {:type "button"
             :on-click #(on-change :token/change-size (min (+ value 5) 50))
             :aria-label "Increase token size by 5 feet"} "+")))
      (let [value (value-fn :token/light)]
        ($ :<>
          ($ :label "Light")
          ($ :button
            {:type "button"
             :on-click #(on-change :token/change-light (max (- value 5) 0))
             :aria-label "Decrease light radius by 5 feet"}
            "-")
          ($ :data {:value value}
            (if (> value 0) (str value "ft. radius") "None"))
          ($ :button
            {:type "button"
             :on-click #(on-change :token/change-light (min (+ value 5) 120))
             :aria-label "Increase light radius by 5 feet"}
            "+")))
      (let [value (value-fn :aura/radius)]
        ($ :<>
          ($ :label "Aura")
          ($ :button
            {:type "button"
             :on-click #(on-change :token/change-aura (max (- value 5) 0))
             :aria-label "Decrease aura size by 5 feet"}
            "-")
          ($ :data {:value value}
            (if (> value 0) (str value "ft. radius") "None"))
          ($ :button
            {:type "button"
             :on-click #(on-change :token/change-aura (min (+ value 5) 120))
             :aria-label "Increase aura size by 5 feet"}
            "+"))))))

(defui ^:private token-form-conditions
  [props]
  (let [fqs (frequencies (reduce into [] ((:values props) :token/flags [])))
        ids ((:values props) :db/id)]
    (for [{value :value icon-name :icon} token-conditions
          :let [focus (= value (:value (first token-conditions)))
                state (cond (= (get fqs value 0) 0) false
                            (= (get fqs value 0) (count ids)) true
                            :else :indeterminate)]]
      ($ checkbox {:key value :checked state}
        (fn [input]
          ($ :label {:aria-label (name value) :data-tooltip (capitalize (name value))}
            ($ :input
              {:ref input
               :type "checkbox"
               :name (str "token-condition-" (name value))
               :checked (if (= state :indeterminate) false state)
               :auto-focus focus
               :on-change
               (fn [event]
                 (let [checked (.. event -target -checked)]
                   ((:on-change props) :token/change-flag value checked)))})
            ($ icon {:name icon-name :size 22})))))))

(defui token-context-menu [{:keys [tokens type]}]
  (let [dispatch (use-dispatch)
        idxs (map :db/id tokens)]
    ($ context-menu
      {:render-toolbar
       (fn [{:keys [selected on-change]}]
         ($ :<>
           (for [[form icon-name tooltip]
                 [[:label "fonts" "Label"]
                  [:details "sliders" "Options"]
                  [:conditions "arrow-through-heart-fill" "Conditions"]]]
             ($ :button
               {:key form
                :type "button"
                :data-selected (= selected form)
                :data-tooltip tooltip
                :on-click #(on-change form)}
               ($ icon {:name icon-name :size 22})))
           (if (= type :host)
             (let [on (every? (comp boolean :hidden :token/flags) tokens)]
               ($ :button
                 {:type "button"
                  :data-selected on
                  :data-tooltip (if on "Reveal" "Hide")
                  :on-click #(dispatch :token/change-flag idxs :hidden (not on))}
                 ($ icon {:name (if on "eye-slash-fill" "eye-fill") :size 22}))))
           (let [on (every? (comp vector? :scene/_initiative) tokens)]
             ($ :button
               {:type "button"
                :data-selected on
                :data-tooltip "Initiative"
                :on-click #(dispatch :initiative/toggle idxs (not on))}
               ($ icon {:name "hourglass-split" :size 22})))
           ($ :button
             {:type "button" :data-tooltip "Remove"
              :on-click #(dispatch :token/remove idxs)}
             ($ icon {:name "trash3-fill" :size 22}))))}
      (fn [{:keys [selected on-change]}]
        (let [props {:on-close  #(on-change nil)
                     :on-change #(apply dispatch %1 idxs %&)
                     :values    (fn vs
                                  ([f] (vs f #{}))
                                  ([f init] (into init (map f) tokens)))}]
          (case selected
            :label      ($ token-form-label props)
            :details    ($ token-form-details props)
            :conditions ($ token-form-conditions props)))))))

(defui ^:private shape-form-color
  [{:keys [on-change values]}]
  ($ :<>
    ($ :fieldset.fieldset
      ($ :legend "Opacity")
      ($ :label
        {:aria-label "Shape opacity"}
        ($ :input
          {:type "range" :name "shape-opacity" :min 0 :max 1 :step 0.10
           :value (first (values :shape/opacity))
           :auto-focus true
           :on-change
           (fn [event]
             (on-change :element/update :shape/opacity (.. event -target -value)))})))
    ($ :fieldset.fieldset
      ($ :legend "Color")
      ($ :.context-menu-form-colors
        (for [{:keys [value label]} shape-colors]
          ($ :label {:key value :aria-label label :style {:background-color value}}
            ($ :input
              {:type "radio"
               :name "shape-color"
               :value value
               :checked (= value (first (values :shape/color)))
               :on-change
               (fn [event]
                 (on-change :element/update :shape/color (.. event -target -value)))})))))))

(defui ^:private shape-form-pattern
  [{:keys [on-change values]}]
  (for [{:keys [value label]} shape-patterns
        :let [checked (= value (first (values :shape/pattern)))]]
    (let [id (str "template-pattern-" (name value))]
      ($ :label {:key value :aria-label label}
        ($ :input
          {:type "radio"
           :name "shape-pattern"
           :value value
           :checked checked
           :auto-focus checked
           :on-change
           (fn [event]
             (let [value (.. event -target -value)]
               (on-change :element/update :shape/pattern (keyword value))))})
        ($ :svg
          ($ :defs ($ pattern {:id id :name value}))
          ($ :rect {:x 0 :y 0 :width "100%" :height "100%" :fill (str "url(#" id ")")}))))))

(defui shape-context-menu [{:keys [data]}]
  (let [dispatch (use-dispatch)]
    ($ context-menu
      {:render-toolbar
       (fn [{:keys [selected on-change]}]
         ($ :<>
           ($ :button
             {:type "button"
              :data-selected (= selected :color)
              :data-tooltip "Color"
              :on-click #(on-change :color)}
             ($ icon {:name "palette-fill"}))
           ($ :button
             {:type "button"
              :data-selected (= selected :pattern)
              :data-tooltip "Pattern"
              :on-click #(on-change :pattern)}
             ($ icon {:name "paint-bucket"}))
           ($ :button
             {:type "button" :data-tooltip "Remove" :style {:margin-left "auto"}
              :on-click #(dispatch :element/remove [(:db/id data)])}
             ($ icon {:name "trash3-fill"}))))}
      (fn [{:keys [selected]}]
        (let [props {:values
                     (fn vs
                       ([f] (vs f #{}))
                       ([f init] (into init (map f) [data])))
                     :on-change
                     (fn [event & args]
                       (apply dispatch event [(:db/id data)] args))}]
          (case selected
            :color   ($ shape-form-color props)
            :pattern ($ shape-form-pattern props)))))))
