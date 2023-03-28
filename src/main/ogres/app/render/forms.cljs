(ns ogres.app.render.forms
  (:require [clojure.string :refer [capitalize]]
            [ogres.app.hooks :refer [use-dispatch]]
            [ogres.app.render :refer [icon]]
            [ogres.app.render.pattern :refer [pattern]]
            [uix.core.alpha :as uix]))

(def conditions
  [[:player "people-fill"]
   [:blinded "eye-slash-fill"]
   [:charmed "arrow-through-heart-fill"]
   [:exhausted "moon-stars-fill"]
   [:invisible "incognito"]
   [:grappled "fist"]
   [:prone "falling"]
   [:frightened "black-cat"]
   [:incapacitated "emoji-dizzy"]
   [:unconscious "skull"]])

(defn stop-propagation [event]
  (.stopPropagation event))

(defn compare-and-toggle
  ([prev next]
   (compare-and-toggle prev next nil))
  ([prev next else]
   (if (= prev next) else next)))

(defn context-menu [render-toolbar render-form]
  (let [selected (uix/state nil)
        callback (uix/callback #(swap! selected compare-and-toggle %1) [@selected])
        props    {:selected @selected :on-change callback}]
    [:div.context-menu {:on-mouse-down stop-propagation}
     [:div.context-menu-toolbar
      (render-toolbar props)]
     (if-let [selected @selected]
       [:div.context-menu-form
        {:css (str "context-menu-form-" (name selected))}
        (render-form props)])]))

(defn checkbox [{:keys [checked on-change]} render-fn]
  (let [input (uix/ref)
        indtr (= checked :indeterminate)
        key   (deref (uix/state (random-uuid)))]
    (uix/effect!
     (fn [] (set! (.-indeterminate @input) indtr)) [indtr])
    (render-fn
     [:input
      {:id key :type "checkbox" :ref input :checked (if indtr false checked)
       :on-change
       (fn [event]
         (on-change (.. event -target -checked)))}] key)))

(defmulti token-form :name)

(defmethod token-form :default [] nil)

(defmethod token-form :label [props]
  (let [input-ref (uix/ref)
        input-val (uix/state
                   (fn []
                     (let [vs ((:values props) :token/label)]
                       (if (= (count vs) 1) (first vs) ""))))]
    (uix/effect! #(.select @input-ref) [])
    [:form
     {:on-submit
      (fn [event]
        (.preventDefault event)
        ((:on-change props) :token/change-label @input-val)
        ((:on-close props)))}
     [:input
      {:type "text"
       :ref input-ref
       :value @input-val
       :auto-focus true
       :placeholder "Press 'Enter' to submit..."
       :on-change #(reset! input-val (.. %1 -target -value))}]]))

(defmethod token-form :details [props]
  (for [[label tx-name attr min def]
        [["Size" :token/change-size :token/size 5 5]
         ["Light" :token/change-light :token/light 0 15]
         ["Aura" :token/change-aura :aura/radius 0 0]]]
    (let [values ((:values props) attr)]
      [:div {:key label}
       [:legend label]
       [:span
        (cond
          (> (count values) 1) "Multiple..."
          (= (count values) 0) (str def "ft.")
          (= (first values) 0) "None"
          (= (count values) 1) (str (first values) "ft."))]
       [:button.button
        {:type "button"
         :on-click
         (fn []
           (let [next (if (> (count values) 1) min (max (- (first values) 5) min))]
             ((:on-change props) tx-name next)))} "-"]
       [:button.button
        {:type "button"
         :on-click
         (fn []
           (let [next (if (> (count values) 1) 5 (+ (first values) 5))]
             ((:on-change props) tx-name next)))} "+"]])))

(defmethod token-form :conditions [props]
  (let [fqs (frequencies (reduce into [] ((:values props) :token/flags [])))
        ids ((:values props) :entity/key)]
    (for [[flag icon-name] conditions]
      ^{:key flag}
      [checkbox
       {:checked
        (cond (= (get fqs flag 0) 0) false
              (= (get fqs flag 0) (count ids)) true
              :else :indeterminate)
        :on-change #((:on-change props) :token/change-flag flag %1)}
       (fn [input key]
         [:div input
          [:label {:for key :data-tooltip (capitalize (name flag))}
           [icon {:name icon-name :size 22}]]])])))

(defn token-context-menu [{:keys [tokens type]}]
  (let [dispatch (use-dispatch)
        keys     (map :entity/key tokens)]
    [context-menu
     (fn [{:keys [selected on-change]}]
       [:<>
        (for [[form icon-name tooltip]
              [[:label "fonts" "Label"]
               [:details "sliders" "Options"]
               [:conditions "flag-fill" "Conditions"]]]
          [:button
           {:key form :type "button" :data-tooltip tooltip
            :css {:selected (= selected form)}
            :on-click #(on-change form)}
           [icon {:name icon-name :size 22}]])
        (let [on (every? (comp boolean :hidden :token/flags) tokens)]
          [:button
           {:type "button" :css {:selected on} :data-tooltip (if on "Reveal" "Hide")
            :on-click #(dispatch :token/change-flag keys :hidden (not on))
            :disabled (= type :conn)}
           [icon {:name (if on "eye-slash-fill" "eye-fill") :size 22}]])
        (let [on (every? (comp vector? :canvas/_initiative) tokens)]
          [:button
           {:type "button" :css {:selected on} :data-tooltip "Initiative"
            :on-click #(dispatch :initiative/toggle keys (not on))}
           [icon {:name "hourglass-split" :size 22}]])
        [:button
         {:type "button" :data-tooltip "Remove"
          :on-click #(dispatch :element/remove keys)}
         [icon {:name "trash3-fill" :size 22}]]])
     (fn [{:keys [selected on-change]}]
       [token-form
        {:name      selected
         :upload?   (= type :host)
         :on-close  #(on-change nil)
         :on-change #(apply dispatch %1 keys %&)
         :values    (fn vs
                      ([f] (vs f #{}))
                      ([f init] (into init (map f) tokens)))}])]))

(defmulti shape-form :name)

(defmethod shape-form :default [] [])

(defmethod shape-form :color [{:keys [on-change values]}]
  [:fieldset
   [:input
    {:type "range" :min 0 :max 1 :step 0.10
     :value (first (values :shape/opacity))
     :on-change #(on-change :element/update :shape/opacity (.. %1 -target -value))}]
   [:div.context-menu-form-colors
    (for [color ["#ffeb3b" "#ff9800" "#f44336" "#673ab7" "#2196f3" "#009688" "#8bc34a" "#fff" "#9e9e9e" "#000"]]
      [:div
       {:key color :style {:background-color color}
        :on-click #(on-change :element/update :shape/color color)}])]])

(defmethod shape-form :pattern [{:keys [on-change]}]
  (for [pattern-name [:solid :lines :circles :crosses :caps :waves]]
    (let [id (str "template-pattern-" (name pattern-name))]
      [:svg {:key pattern-name :width "100%" :on-click #(on-change :element/update :shape/pattern pattern-name)}
       [:defs [pattern {:id id :name pattern-name}]]
       [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill (str "url(#" id ")")}]])))

(defn shape-context-menu [{:keys [shape]}]
  (let [dispatch (use-dispatch)]
    [context-menu
     (fn [{:keys [selected on-change]}]
       [:<>
        [:button
         {:type "button"
          :css {:selected (= selected :color)}
          :data-tooltip "Color"
          :on-click #(on-change :color)}
         [icon {:name "palette-fill"}]]
        [:button
         {:type "button"
          :css {:selected (= selected :pattern)}
          :data-tooltip "Pattern"
          :on-click #(on-change :pattern)}
         [icon {:name "paint-bucket"}]]
        [:button
         {:type "button" :data-tooltip "Remove" :style {:margin-left "auto"}
          :on-click #(dispatch :element/remove [(:entity/key shape)])}
         [icon {:name "trash3-fill"}]]])
     (fn [{:keys [selected]}]
       [shape-form
        {:name      selected
         :on-change #(apply dispatch %1 [(:entity/key shape)] %&)
         :values    (fn vs
                      ([f] (vs f #{}))
                      ([f init] (into init (map f) [shape])))}])]))
