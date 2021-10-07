(ns ogre.tools.render.command
  (:require [ogre.tools.render.icon :refer [icon]]
            [ogre.tools.state :refer [use-query]]))

(def shape->icon
  {:circle :circle
   :rect :square
   :cone :triangle
   :line :slash-lg})

(def shape->shortcut
  {:circle "1"
   :rect "2"
   :cone "3"
   :line "4"})

(defn commands [first & rest]
  [:div.command-set
   [:div.command-set-mark]
   [:div.command-set-first first]
   [:div.command-set-rest rest]])

(defn shortcut [key]
  [:div.commands-shortcut key])

(defn tooltip [message]
  [:div.commands-tooltip message])

(def attrs
  [[:share/open? :default false]
   [:share/paused? :default false]
   {:root/canvas
    [[:canvas/mode :default :select]
     [:canvas/theme :default :light]
     [:canvas/last-shape :default :circle]]}])

(defn command [props]
  (let [[result dispatch]                      (use-query {:pull attrs})
        {:keys [share/open? shape/paused?]}    result
        {:canvas/keys [mode theme last-shape]} (:root/canvas result)
        mode-attrs
        (fn [given]
          {:type "button"
           :key given
           :css {:selected (= given mode)}
           :on-click #(dispatch :canvas/toggle-mode given)})]
    [:div.commands
     [:button (mode-attrs :select)
      [icon {:name :cursor}]
      [shortcut "S"]
      [tooltip "Select"]]
     [:button (mode-attrs :ruler)
      [icon {:name :rulers}]
      [shortcut "R"]
      [tooltip "Ruler"]]
     [commands
      (let [last last-shape]
        [:button (mode-attrs last)
         [icon {:name (shape->icon last)}]
         [shortcut (shape->shortcut last)]])
      (for [shape [:circle :rect :cone :line]]
        [:button (mode-attrs shape)
         [icon {:name (shape->icon shape)}]
         [shortcut (shape->shortcut shape)]])]
     [commands
      [:button
       {:css {:active open?} :on-click #(dispatch :share/initiate)}
       [icon {:name :pip :size 22}]]
      [:button {:key :switch :disabled (not open?) :on-click #(dispatch :share/switch)}
       (if paused?
         [icon {:name :play-fill}]
         [icon {:name :pause-fill}])
       [shortcut "P"]
       [tooltip "Pause / resume"]]]]))
