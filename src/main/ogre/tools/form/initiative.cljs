(ns ogre.tools.form.initiative
  (:require [clojure.string :refer [join capitalize blank?]]
            [ogre.tools.form.render :refer [form]]
            [ogre.tools.render :refer [button icon use-image use-modal]]
            [ogre.tools.state :refer [use-query]]
            [uix.core.alpha :as uix]))

(defn order [a b]
  (compare
   [(:initiative/roll b) (contains? (:element/flags a) :player) (:element/name a)]
   [(:initiative/roll a) (contains? (:element/flags b) :player) (:element/name b)]))

(defn roll-form [{:keys [value on-change]}]
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

(defn health-form [{:keys [value on-change]}]
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

(defn initiant [{:keys [entity]}]
  (let [dispatch (use-query)
        {key   :entity/key
         name  :element/name
         sffx  :initiative/suffix
         flags :element/flags
         {checksum :image/checksum} :token/stamp} entity
        url (use-image checksum)]
    [:li.initiant
     (if checksum
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
     (if sffx
       [:div.initiant-suffix (char (+ sffx 64))])
     [:div.initiant-info
      (if (not (blank? name))
        [:div.initiant-label name])
      [:div.initiant-flags
       (if (seq flags)
         [:em (join ", " (mapv (comp capitalize clojure.core/name) flags))]
         [:em "No Conditions"])]]
     [health-form
      {:value (:initiative/health entity)
       :on-change
       (fn [f v]
         (dispatch :initiative/change-health key f v))}]]))

(def query
  [{:local/window
    [{:window/canvas
      [:entity/key
       {:canvas/initiative
        [:entity/key
         :element/name
         :element/flags
         :initiative/roll
         :initiative/suffix
         :initiative/health
         :window/_selected
         {:token/stamp [:image/checksum]}]}]}]}])

(defn initiative []
  (let [[result dispatch] (use-query query)
        initiative        (-> result :local/window :window/canvas :canvas/initiative)]
    (if (seq initiative)
      [:div.initiative
       [:section [:header "Initiative"]]
       [:section
        [:fieldset.table {:style {:padding "0 12px"}}
         [button {:on-click #(dispatch :initiative/roll-all)} "Roll"]
         [button {:on-click #(dispatch :initiative/reset-rolls)} "Reset"]
         [button {:on-click #(dispatch :initiative/leave)} "Leave"]]]
       [:section
        [:ol
         (for [entity (sort order initiative)]
           ^{:key (:entity/key entity)} [initiant {:entity entity}])]]]
      [:div.initiative
       [:section [:header "Initiative"]]
       [:section
        [:div.prompt
         [icon {:name "hourglass-split" :size 48}]
         [:br] "Begin initiative by selecting"
         [:br] "one or more tokens and clicking"
         [:br] "'Start Initiative'"]]])))

(defmethod form :initiative [] initiative)
