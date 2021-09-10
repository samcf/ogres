(ns ogre.tools.render.initiative
  (:require [clojure.string :refer [join capitalize blank?]]
            [datascript.core :as ds]
            [ogre.tools.query :as query]
            [ogre.tools.render :refer [css]]
            [ogre.tools.render.icon :refer [icon]]
            [ogre.tools.state :as state]
            [uix.core.alpha :as uix]))

(defn label [{:keys [element/name initiative/suffix]}]
  (cond-> ""
    (string? name)   (str name)
    (blank? name)    (str "Unknown")
    (number? suffix) (str " (" (char (+ suffix 64)) ")")))

(defn initiative-order [a b]
  (compare
   [(:initiative/roll a) (contains? (:element/flags b) :player) (label b)]
   [(:initiative/roll b) (contains? (:element/flags a) :player) (label a)]))

(defn button [attrs child]
  [:button.ogre-button (merge {:type "button"} attrs) child])

(def initial-state
  {:editing/roll? false :editing/health? false :input/roll nil :input/health nil})

(defn initiant [{:keys [element dispatch selected]}]
  (let [ident (:db/id element) state (uix/state initial-state)]
    (uix/effect!
     (fn [] (swap! state assoc :editing/roll? false))
     [(:initiative/roll element)])

    (cond
      (:editing/roll? @state)
      [:div.initiant.initiant-layout-roll
       {:class (css {:selected selected})}
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

      (:editing/health? @state)
      [:div.initiant.initiant-layout-health
       {:class (css {:selected selected})}
       [:div.initiant-health-input
        [:input
         {:type "number"
          :value (or (:input/health @state) "")
          :autoFocus true
          :placeholder "Hitpoints"
          :on-change #(swap! state assoc :input/health (.. % -target -value))}]]
       (for [[icon-name class updator]
             [[:caret-down-fill "initiant-hp-take" -]
              [:caret-up-fill "initiant-hp-give" +]]]
         [:div
          {:key class
           :class class
           :on-click
           (fn []
             (dispatch :initiative/change-health ident updator (:input/health @state))
             (swap! state merge {:editing/health? false :input/health nil}))}
          [icon {:name icon-name :width 26 :height 26}]])
       [:div.initiant-hp
        {:on-click #(swap! state assoc :editing/health? false)} "×"]]

      :else
      [:div.initiant.initiant-layout-default
       {:class (css {:selected selected})}
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
        {:on-click #(dispatch :element/select ident)}
        [label element]]
       [:div.initiant-cond
        (let [flags (:element/flags element)]
          (if (seq flags)
            (join ", " (map #(capitalize (name %)) flags))
            [:em "No Conditions"]))]
       [:div.initiant-hp
        {:on-click #(swap! state assoc :editing/health? true)}
        (let [health (:initiative/health element)]
          (if (blank? health) "HP" health))]])))

(defn initiative []
  (let [{:keys [dispatch data workspace]} (uix/context state/state)]
    [:div.initiative
     [:section [:header "Initiative"]]
     [:section.initiative-actions
      [button {:on-click #(dispatch :initiative/roll-all)} "Roll"]
      [button {:on-click #(dispatch :initiative/reset-rolls)} "Reset"]
      [button {:on-click #(dispatch :initiative/leave)} "Leave"]]
     (let [initiants (->> (query/initiating data) (sort initiative-order) (reverse))]
       [:section
        (for [element initiants :let [id (:db/id element)]]
          [initiant
           {:key id
            :element (into {:db/id id} (ds/touch element))
            :selected (contains? (:canvas/selected workspace) element)
            :dispatch dispatch}])])]))
