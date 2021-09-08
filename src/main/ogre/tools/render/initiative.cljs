(ns ogre.tools.render.initiative
  (:require [clojure.string :refer [join capitalize blank?]]
            [datascript.core :as ds]
            [ogre.tools.query :as query]
            [ogre.tools.render.icon :refer [icon]]
            [ogre.tools.state :as state]
            [uix.core.alpha :as uix]))

(defn initiative-order [a b]
  (compare
   [(:initiative/roll a) (contains? (:element/flags b) :player) (:element/name b)]
   [(:initiative/roll b) (contains? (:element/flags a) :player) (:element/name a)]))

(defn button [attrs child]
  [:button.ogre-button (merge attrs {:type "button"}) child])

(def initial-state
  {:editing/roll? false :editing/life? false :input/roll nil})

(defn initiant [{:keys [element dispatch]}]
  (let [ident (:db/id element) state (uix/state initial-state)]

    (uix/effect!
     (fn [] (swap! state assoc :editing/roll? false))
     [(:initiative/roll element)])

    (cond
      (:editing/roll? @state)
      [:div.initiant.initiant-layout-roll {:key ident}
       [:div.initiant-roll
        {:on-click
         (fn []
           (dispatch :initiative/change-roll ident (:input/roll @state))
           (swap! state assoc :editing/roll? false))}
        "✓"]
       [:div.initiant-inc
        {:on-click #(swap! state update :input/roll inc)} "+"]
       [:div.initiant-dec
        {:on-click #(swap! state update :input/roll dec)} "-"]
       [:div.initiant-dice-input
        [:input
         {:type "number"
          :autoFocus true
          :placeholder "Initiative Roll"
          :value (or (:input/roll @state) "")
          :on-change #(swap! state assoc :input/roll (.. % -target -value))}]]
       [:div.initiant-dice
        {:on-click
         (fn []
           (swap! state assoc :input/roll (inc (rand-int 20))))}
        [icon {:name :dice-5 :width 32 :height 32}]]]

      (:editing/life? @state)
      nil

      :else
      [:div.initiant.initiant-layout-default {:key ident}
       [:div.initiant-roll
        {:on-click
         (fn []
           (swap!
            state merge
            {:editing/roll? true
             :input/roll (:initiative/roll element)}))}
        (let [roll (:initiative/roll element)]
          (if (blank? roll) "?" roll))]
       [:div.initiant-inc
        {:on-click #(dispatch :initiative/roll-inc ident)} "+"]
       [:div.initiant-dec
        {:on-click #(dispatch :initiative/roll-dec ident)} "-"]
       [:div.initiant-name
        (let [name (:element/name element)]
          (if (not (blank? name)) name [:em "Unknown"]))]
       [:div.initiant-cond
        (let [flags (:element/flags element)]
          (if (seq flags)
            (join ", " (map #(capitalize (name %)) flags))
            [:em "No Conditions"]))]])))

(defn initiative []
  (let [context  (uix/context state/state)]
    [:div.initiative
     [:section [:header "Initiative"]]
     [:section.initiative-actions
      [button {:on-click #((:dispatch context) :initiative/roll-all)} "Roll"]
      [button {:on-click #((:dispatch context) :initiative/reset-rolls)} "Reset"]
      [button {:on-click #((:dispatch context) :initiative/leave)} "Leave"]]
     (let [initiants (->> (query/initiating (:data context)) (sort initiative-order) (reverse))]
       [:section
        (for [element initiants :let [id (:db/id element)]]
          [initiant
           {:key id
            :element (into {:db/id id} (ds/touch element))
            :dispatch (:dispatch context)}])])]))
