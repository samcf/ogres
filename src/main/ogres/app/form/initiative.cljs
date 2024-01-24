(ns ogres.app.form.initiative
  (:require [clojure.string :refer [join capitalize blank?]]
            [ogres.app.hooks :refer [use-dispatch use-image use-modal use-query]]
            [ogres.app.render :refer [icon]]
            [uix.core :refer [defui $ use-ref]]))

(def ^:private query-form
  [:local/type
   {:local/camera
    [{:camera/scene
      [:db/id
       :initiative/rounds
       :initiative/turn
       {:scene/initiative
        [:db/id
         :token/label
         :token/flags
         :initiative/roll
         :initiative/suffix
         :initiative/health
         :camera/_selected
         {:token/image [:image/checksum]}]}]}]}])

(def ^:private query-footer
  [{:local/camera
    [{:camera/scene
      [[:initiative/turns :default 0]
       [:initiative/rounds :default 0]
       :scene/initiative]}]}])

(defn ^:private initiative-order
  [a b]
  (let [f (juxt :initiative/roll :db/id)]
    (compare (f b) (f a))))

(defn ^:private format-time
  [seconds]
  (str (-> (mod seconds 3600) (/ 60) (js/Math.floor) (.toString) (.padStart 2 "0")) ":"
       (-> (mod seconds   60)        (js/Math.floor) (.toString) (.padStart 2 "0"))))

(defn ^:private visible?
  [flags]
  (or (contains? flags :player)
      (not (contains? flags :hidden))))

(defui ^:private form-dice
  [{:keys [value on-change]}]
  (let [[editing set-editing form] (use-modal)
        input (use-ref)]
    ($ :.initiative-token-roll
      {:data-present (some? value)}
      ($ :button.initiative-token-roll-label
        {:on-click
         (fn [event]
           (.stopPropagation event)
           (set-editing not)
           (.requestAnimationFrame
            js/window
            #(if-let [node (deref input)]
               (.select node))))}
        (or value "?"))
      (if editing
        ($ :form.initiative-token-form
          {:ref form
           :data-type "roll"
           :on-submit
           (fn [event]
             (.preventDefault event)
             (on-change (.-value (deref input)))
             (set-editing not))}
          ($ :input.text.text-ghost
            {:type "number"
             :ref input
             :auto-focus true
             :default-value value
             :placeholder "Initiative"
             :aria-label "Initiative roll"})
          ($ :button {:type "submit"} "✓"))))))

(defui ^:private form-hp
  [{:keys [value on-change]}]
  (let [[editing set-editing form] (use-modal)
        input (use-ref)]
    ($ :.initiative-token-health
      {:data-present (some? value)}
      ($ :.initiative-token-health-frame
        ($ icon {:name "heart-fill" :size 40}))
      ($ :button.initiative-token-health-label
        {:on-click (fn [event] (.stopPropagation event) (set-editing not))}
        (or value "HP"))
      (if editing
        ($ :form.initiative-token-form
          {:ref form
           :data-type "health"
           :on-submit
           (fn []
             (on-change (fn [_ v] v) (.-value @input))
             (set-editing not))}
          ($ :input.text.text-ghost
            {:type "number"
             :name "hitpoints"
             :ref input
             :auto-focus true
             :placeholder "Hitpoints"
             :aria-label "Hitpoints"})
          (for [[key label f] [["-" "Subtract from" -] ["+" "Add to" +] ["=" "Set as" (fn [_ v] v)]]]
            ($ :button
              {:key key :type "button" :aria-label label
               :on-click
               (fn []
                 (on-change f (.-value @input))
                 (set-editing not))} key)))))))

(defui ^:private token
  [{:keys [context entity]}]
  (let [dispatch (use-dispatch)
        {type      :local/type
         {{current :initiative/turn} :camera/scene} :local/camera} context
        {id        :db/id
         label     :token/label
         flags     :token/flags
         suffix    :initiative/suffix
         {checksum :image/checksum} :token/image} entity
        data-url (use-image checksum)]
    ($ :li.initiative-token
      {:data-current (= (:db/id current) (:db/id entity))}
      ($ form-dice
        {:value (:initiative/roll entity)
         :on-change
         (fn [value]
           (dispatch :initiative/change-roll id value))})
      ($ :.initiative-token-frame
        {:on-click #(dispatch :element/select id)
         :data-player (contains? flags :player)}
        (if data-url
          ($ :.initiative-token-image
            {:style {:background-image (str "url(" data-url ")")}})
          ($ :.initiative-token-pattern
            ($ icon {:name "dnd" :size 36}))))
      (if suffix
        ($ :.initiative-token-suffix (char (+ suffix 64))))
      ($ :.initiative-token-info
        (if (not (blank? label))
          ($ :.initiative-token-label label))
        ($ :.initiative-token-flags
          (if (seq flags)
            (join ", " (mapv (comp capitalize name) flags)))))
      (if (or (= type :host) (contains? flags :player))
        ($ form-hp
          {:value (:initiative/health entity)
           :on-change
           (fn [f v]
             (dispatch :initiative/change-health id f v))})))))

(defui form []
  (let [result (use-query query-form)
        {type :local/type
         {{tokens :scene/initiative
           rounds :initiative/rounds} :camera/scene}
         :local/camera} result]
    (cond (and (not (seq tokens)) (nil? rounds))
          ($ :.prompt "Begin initiative by selecting one or more tokens and clicking the hourglass icon.")

          (and (not (seq tokens)) (>= rounds 1))
          ($ :.prompt "Initiative is still running but there are no tokens participating.")

          (seq tokens)
          ($ :.form-initiative.initiative
            ($ :ol.initiative-list
              (for [entity (sort initiative-order tokens)
                    :when  (or (= type :host) (visible? (:token/flags entity)))]
                ($ token
                  {:key (:db/id entity)
                   :entity entity
                   :context result})))))))

(defui footer []
  (let [dispatch (use-dispatch)
        result   (use-query query-footer)
        {{{turns  :initiative/turns
           rounds :initiative/rounds
           tokens :scene/initiative}
          :camera/scene}
         :local/camera} result]
    ($ :<>
      ($ :button.button.button-neutral {:disabled true} "Round " rounds)
      ($ :button.button.button-neutral {:disabled true} "Time "
        ($ :span {:style {:text-transform "lowercase"}} (format-time (* turns 6))))
      ($ :button.button.button-primary
        {:disabled (empty? tokens) :on-click #(dispatch :initiative/next)}
        ($ icon {:name "play-fill" :size 16})
        (if (<= rounds 0) "Start" "Next"))
      ($ :button.button.button-neutral
        {:disabled (empty? tokens) :on-click #(dispatch :initiative/roll-all)}
        ($ icon {:name "dice-5-fill" :size 16}) "Randomize")
      ($ :button.button.button-neutral
        {:disabled (empty? tokens) :on-click #(dispatch :initiative/reset)}
        ($ icon {:name "arrow-counterclockwise" :size 16}) "Reset")
      ($ :button.button.button-danger
        {:disabled (empty? tokens) :on-click #(dispatch :initiative/leave)}
        ($ icon {:name "x-circle-fill" :size 16}) "Leave"))))
