(ns ogres.app.form.initiative
  (:require [clojure.string :refer [join capitalize blank?]]
            [ogres.app.hooks :refer [use-dispatch use-modal use-query]]
            [ogres.app.render :refer [icon image]]
            [uix.core :as uix :refer [defui $ use-ref]]))

(def ^:private query-form
  [:local/type
   {:local/camera
    [{:camera/scene
      [:db/id
       :initiative/rounds
       :initiative/turn
       :initiative/played
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
      [{:scene/initiative
        [:db/id :initiative/roll :token/flags]}
       [:initiative/rounds :default 0]
       :initiative/played]}]}])

(def ^:private npc-xf
  (comp (filter (comp (complement :player) :token/flags))
        (filter (comp nil? :initiative/roll))))

(defn ^:private initiative-order
  [a b]
  (let [f (juxt :initiative/roll :db/id)]
    (compare (f b) (f a))))

(defui ^:private form-dice
  [{:keys [value on-change]}]
  (let [[editing set-editing form] (use-modal)
        input (use-ref)]
    ($ :.initiative-token-roll
      {:data-present (some? value)}
      ($ :button.initiative-token-roll-control
        {:on-click
         (fn [event]
           (.stopPropagation event)
           (set-editing not)
           (.requestAnimationFrame
            js/window
            #(if-let [node (deref input)]
               (.select node))))}
        (or value \?))
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
          ($ :button {:type "submit"}
            ($ icon {:name "check"})))))))

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
        {type :local/type
         {{curr :initiative/turn
           went :initiative/played}
          :camera/scene} :local/camera} context
        {id        :db/id
         label     :token/label
         flags     :token/flags
         suffix    :initiative/suffix
         {checksum :image/checksum} :token/image} entity
        playing (= (:db/id curr) (:db/id entity))
        played  (boolean (some #{{:db/id id}} went))
        hidden  (and (= type :conn)
                     (contains? flags :hidden)
                     (not (contains? flags :player)))]
    ($ :li.initiative-token
      {:data-playing playing
       :data-played played
       :data-hidden hidden
       :data-type "token"}
      ($ :button.initiative-token-turn
        {:on-click
         (fn []
           (if played
             (dispatch :initiative/unmark id)
             (dispatch :initiative/mark id)))}
        ($ icon {:name "arrow-right-short"}))
      ($ form-dice
        {:value (:initiative/roll entity)
         :on-change
         (fn [value]
           (dispatch :initiative/change-roll id value))})
      ($ :.initiative-token-frame
        {:on-click #(dispatch :element/select id)
         :data-player (contains? flags :player)
         :data-hidden hidden}
        (cond hidden \?
              (some? checksum)
              ($ image {:checksum checksum}
                (fn [{:keys [data-url]}]
                  ($ :.initiative-token-image
                    {:style {:background-image (str "url(" data-url ")")}})))
              :else
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

(defui ^:private token-placeholder []
  ($ :li.initiative-token {:data-type "placeholder"}
    ($ :.initiative-token-turn
      ($ icon {:name "arrow-right-short"}))
    ($ :.initiative-token-roll
      ($ :.initiative-token-roll-control))
    ($ :.initiative-token-frame
      ($ :.initiative-token-pattern))
    ($ :.initiative-token-info)
    ($ :.initiative-token-health
      ($ :.initiative-token-health-frame
        ($ icon {:name "heart-fill" :size 40}))
      ($ :.initiative-token-health-label))))

(defui form []
  (let [dispatch (use-dispatch)
        result (use-query query-form)
        {{{tokens :scene/initiative
           rounds :initiative/rounds} :camera/scene}
         :local/camera} result]
    ($ :.initiative
      ($ :header
        ($ :h2 "Initiative")
        (if (>= rounds 1)
          ($ :h3 "Round " rounds)))
      (cond (and (not (seq tokens)) (nil? rounds))
            ($ :ol.initiative-list.initiative-list-placeholder
              (for [indx (range 6)]
                (if (= indx 1)
                  ($ :.initiative-prompt {:key indx :style {:text-align "center"}}
                    "Begin initiative by selecting one or more tokens and
                     clicking the hourglass button.")
                  ($ token-placeholder {:key indx}))))
            (and (not (seq tokens)) (>= rounds 1))
            ($ :.prompt
              ($ :br)
              "Initiative is still running but there are no tokens participating."
              ($ :br)
              ($ :br)
              ($ :button.button.button-neutral
                {:on-click #(dispatch :initiative/leave)} "Leave initiative"))
            (seq tokens)
            ($ :ol.initiative-list
              (for [entity (sort initiative-order tokens)]
                ($ token {:key (:db/id entity) :entity entity :context result})))))))

(defui footer []
  (let [dispatch (use-dispatch)
        result   (use-query query-footer)
        {{{rounds :initiative/rounds
           played :initiative/played
           tokens :scene/initiative}
          :camera/scene}
         :local/camera} result
        on-quit (uix/use-callback #(dispatch :initiative/leave) [dispatch])
        on-next (uix/use-callback #(dispatch :initiative/next) [dispatch])]
    ($ :<>
      ($ :button.button.button-neutral
        {:disabled (empty? tokens) :on-click on-quit} "Leave")
      ($ :button.button.button-neutral
        {:disabled (not (seq (sequence npc-xf tokens)))
         :on-click #(dispatch :initiative/roll-all)
         :style {:text-transform "none"}}
        ($ icon {:name "dice-5-fill" :size 16}) "ROLL NPCs")
      (cond (not (seq tokens))
            ($ :button.button.button-neutral
              {:disabled true} "Next")
            (<= rounds 0)
            ($ :button.button.button-primary
              {:on-click on-next} "Start")
            (= (count played) (count tokens))
            ($ :button.button.button-primary
              {:on-click on-next} "New round")
            :else
            ($ :button.button.button-neutral
              {:on-click on-next} "Next")))))
