(ns ogres.app.form.initiative
  (:require [clojure.string :refer [join capitalize blank?]]
            [ogres.app.form.render :refer [form]]
            [ogres.app.hooks :refer [use-dispatch use-image use-modal use-query]]
            [ogres.app.render :refer [icon]]
            [uix.core.alpha :as uix]))

(defn ^{:private true} visible? [flags]
  (or (contains? flags :player)
      (not (contains? flags :hidden))))

(defn ^{:private true} order [a b]
  (compare
   [(:initiative/roll b) (contains? (:token/flags a) :player) (:token/label a)]
   [(:initiative/roll a) (contains? (:token/flags b) :player) (:token/label b)]))

(defn ^{:private true} roll-form [{:keys [value on-change]}]
  (let [input (uix/ref) [editing? form] (use-modal)]
    [:div.initiant-roll
     [:div.initiant-roll-label
      {:on-click
       (fn []
         (swap! editing? not)
         (.requestAnimationFrame js/window (fn [] (.select @input))))}
      (or value "?")]
     (if @editing?
       [:form.initiant-form
        {:ref form
         :on-submit
         (fn [event]
           (.preventDefault event)
           (on-change (.-value @input))
           (swap! editing? not))}
        [:input
         {:type "number" :ref input :autoFocus true
          :placeholder "Roll" :default-value value}]
        [:button {:type "submit"} "✓"]])]))

(defn ^{:private true} health-form [{:keys [value on-change]}]
  (let [input (uix/ref) [editing? form] (use-modal)]
    [:div.initiant-health {:css {:active (or (number? value) @editing?)}}
     (if @editing?
       [:form.initiant-form
        {:ref form
         :on-submit
         (fn [event]
           (.preventDefault event)
           (on-change (fn [_ v] v) (.-value @input))
           (swap! editing? not))}
        [:input {:type "number" :ref input :autoFocus true :placeholder "HP"}]
        (for [[index [k f]] (map-indexed vector [["-" -] ["+" +] ["=" (fn [_ v] v)]])]
          [:button
           {:key index :type "button"
            :on-click
            (fn []
              (on-change f (.-value @input))
              (swap! editing? not))} k])])
     [:div.initiant-health-icon
      {:on-click #(swap! editing? not)}
      [icon {:name "suit-heart-fill" :size 38}]
      (if (number? value)
        [:div.initiant-health-label value])]]))

(defn ^{:private true} initiant [context entity]
  (let [dispatch                    (use-dispatch)
        {:keys [entity/key]}        entity
        {:token/keys [label flags]} entity
        {:initiative/keys [suffix]} entity
        url                         (use-image (-> entity :token/image :image/checksum))]
    [:li.initiant
     (if url
       [:div.initiant-image
        {:style {:background-image (str "url(" url ")")}
         :on-click #(dispatch :element/select key true)}]
       [:div.initiant-pattern
        {:on-click #(dispatch :element/select key true)}
        [icon {:name "person-circle" :size 36}]])
     [roll-form
      {:value (:initiative/roll entity)
       :on-change
       (fn [value]
         (dispatch :initiative/change-roll key value))}]
     (if suffix
       [:div.initiant-suffix (char (+ suffix 64))])
     [:div.initiant-info
      (if (not (blank? label))
        [:div.initiant-label label])
      [:div.initiant-flags
       (if (seq flags)
         [:em (join ", " (mapv (comp capitalize name) flags))])]]
     (if (or (= (:local/type context) :host)
             (contains? flags :player))
       [health-form
        {:value (:initiative/health entity)
         :on-change
         (fn [f v]
           (dispatch :initiative/change-health key f v))}])]))

(def ^{:private true} query
  [:local/type
   {:local/window
    [{:window/canvas
      [:entity/key
       {:canvas/initiative
        [:entity/key
         :token/label
         :token/flags
         :initiative/roll
         :initiative/suffix
         :initiative/health
         :window/_selected
         {:token/image [:image/checksum]}]}]}]}])

(defn ^{:private true} initiative []
  (let [dispatch   (use-dispatch)
        result     (use-query query)
        initiative (-> result :local/window :window/canvas :canvas/initiative)
        host?      (= (:local/type result) :host)]
    (if (seq initiative)
      [:div.initiative
       [:section
        [:fieldset.table
         [:button.ogre-button {:type "button" :on-click #(dispatch :initiative/roll-all)} "Roll"]
         [:button.ogre-button {:type "button" :on-click #(dispatch :initiative/reset-rolls)} "Reset"]
         [:button.ogre-button {:type "button" :on-click #(dispatch :initiative/leave)} "Leave"]]]
       [:section
        [:ol
         (for [entity (sort order initiative)
               :when (or host? (visible? (:token/flags entity)))]
           ^{:key (:entity/key entity)} [initiant result entity])]]]
      [:div.initiative
       [:section
        [:div.prompt
         [:br] "Begin initiative by selecting"
         [:br] "one or more tokens and clicking"
         [:br] "the hourglass icon."]]])))

(defmethod form :initiative [] initiative)
