(ns ogres.app.form.initiative
  (:require [clojure.string :refer [join capitalize blank?]]
            [ogres.app.hooks :refer [use-dispatch use-image use-modal use-query]]
            [ogres.app.render :refer [icon]]
            [uix.core :refer [defui $ use-ref]]))

(def ^:private query-form
  [:local/type
   {:local/camera
    [{:camera/scene
      [:db/key
       :initiative/rounds
       {:initiative/turn [:db/key]}
       {:scene/initiative
        [:db/key
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
       {:scene/initiative
        [:db/key]}]}]}])

(defn ^:private initiative-order
  [a b]
  (let [f (juxt :initiative/roll :db/key)]
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
  (let [input (use-ref) [editing set-editing form] (use-modal)]
    ($ :div.initiative-token-roll
      ($ :div.initiative-token-roll-label
        {:on-click
         (fn [event]
           (.stopPropagation event)
           (set-editing not)
           (.requestAnimationFrame js/window #(if-let [node (deref input)] (.select node))))}
        (or value "?"))
      (if editing
        ($ :form.initiative-token-form
          {:ref form
           :data-type "roll"
           :on-submit
           (fn [event]
             (.preventDefault event)
             (on-change (.-value @input))
             (set-editing not))}
          ($ :input
            {:type "number" :ref input :autoFocus true
             :placeholder "Roll" :default-value value})
          ($ :button {:type "submit"} "✓"))))))

(defui ^:private form-hp
  [{:keys [value on-change]}]
  (let [input (use-ref) [editing set-editing form] (use-modal)]
    ($ :div.initiative-token-health
      {:data-active (or editing (number? value))}
      ($ :.initiative-token-form-health-control
        {:on-click
         (fn [event]
           (.stopPropagation event)
           (set-editing not))}
        (or value "HP"))
      (if editing
        ($ :form.initiative-token-form
          {:ref form
           :data-type "health"
           :on-submit
           (fn [event]
             (.preventDefault event)
             (on-change (fn [_ v] v) (.-value @input))
             (set-editing not))}
          ($ :input {:type "number" :ref input :autoFocus true :placeholder "HP"})
          (for [[index [k f]] (map-indexed vector [["-" -] ["+" +] ["=" (fn [_ v] v)]])]
            ($ :button
              {:key index :type "button"
               :on-click
               (fn []
                 (on-change f (.-value @input))
                 (set-editing not))} k)))))))

(defui ^:private token
  [{:keys [context entity]}]
  (let [dispatch (use-dispatch)
        {type      :local/type
         {{current :initiative/turn} :camera/scene} :local/camera} context
        {key       :db/key
         label     :token/label
         flags     :token/flags
         suffix    :initiative/suffix
         {checksum :image/checksum} :token/image} entity
        data-url (use-image checksum)]
    ($ :li.initiative-token
      {:data-current (= (:db/key current) (:db/key entity))}
      (if data-url
        ($ :div.initiative-token-image
          {:style {:background-image (str "url(" data-url ")")}
           :on-click #(dispatch :element/select key true)})
        ($ :div.initiative-token-pattern
          {:on-click #(dispatch :element/select key true)}
          ($ icon {:name "dnd" :size 36})))
      ($ form-dice
        {:value (:initiative/roll entity)
         :on-change
         (fn [value]
           (dispatch :initiative/change-roll key value))})
      (if suffix
        ($ :div.initiative-token-suffix (char (+ suffix 64))))
      ($ :div.initiative-token-info
        (if (not (blank? label))
          ($ :div.initiative-token-label label))
        ($ :div.initiative-token-flags
          (if (seq flags)
            (join ", " (mapv (comp capitalize name) flags)))))
      (if (or (= type :host) (contains? flags :player))
        ($ form-hp
          {:value (:initiative/health entity)
           :on-change
           (fn [f v]
             (dispatch :initiative/change-health key f v))})))))

(defui form []
  (let [result (use-query query-form)
        {type :local/type
         {{tokens :scene/initiative
           rounds :initiative/rounds} :camera/scene}
         :local/camera} result]
    ($ :section.initiative
      (cond (and (not (seq tokens)) (nil? rounds))
            ($ :section
              ($ :div.prompt
                "Begin initiative by selecting"
                ($ :br) "one or more tokens and clicking"
                ($ :br) "the hourglass icon."))

            (and (not (seq tokens)) (>= rounds 1))
            ($ :div.prompt
              "Initiative is still running"
              ($ :br) "but there are no tokens participating.")

            (seq tokens)
            ($ :ol.initiative-list
              (for [entity (sort initiative-order tokens)
                    :when  (or (= type :host) (visible? (:token/flags entity)))]
                ($ token
                  {:key     (:db/key entity)
                   :context result
                   :entity  entity})))))))

(defui footer []
  (let [dispatch (use-dispatch)
        result   (use-query query-footer)
        {{{turns  :initiative/turns
           rounds :initiative/rounds
           tokens :scene/initiative}
          :camera/scene}
         :local/camera} result
        started (>= rounds 1)]
    ($ :<>
      ($ :button.button {:disabled true} "Round " rounds)
      ($ :button.button {:disabled true} "Time "
        ($ :span {:style {:text-transform "lowercase"}} (format-time (* turns 6))))
      ($ :button.button
        {:disabled (empty? tokens) :on-click #(dispatch :initiative/next)}
        ($ icon {:name "play-fill" :size 16})
        (if (<= rounds 0) "Start" "Next"))
      ($ :button.button
        {:disabled (empty? tokens) :on-click #(dispatch :initiative/roll-all)}
        ($ icon {:name "dice-5-fill" :size 16}) "Randomize")
      ($ :button.button
        {:disabled (empty? tokens) :on-click #(dispatch :initiative/reset)}
        ($ icon {:name "arrow-counterclockwise" :size 16}) "Reset")
      ($ :button.button
        {:disabled (not started) :on-click #(dispatch :initiative/leave)}
        ($ icon {:name "x-circle-fill" :size 16}) "Leave"))))
