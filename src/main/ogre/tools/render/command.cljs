(ns ogre.tools.render.command
  (:require [ogre.tools.query :as query]
            [ogre.tools.render :refer [css]]
            [ogre.tools.render.icon :refer [icon]]
            [ogre.tools.state :refer [state]]
            [uix.core.alpha :as uix]))

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

(defn command [props]
  (let [{:keys [dispatch data workspace]} (uix/context state)
        {:keys [canvas/mode canvas/theme canvas/last-shape]} workspace
        {:keys [share/open? share/paused?]} (query/viewer data)
        mode-attrs
        (fn [given]
          {:type "button"
           :key given
           :class (css {:selected (= given mode)})
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
      (let [last (or last-shape :circle)]
        [:button (mode-attrs last)
         [icon {:name (shape->icon last)}]
         [shortcut (shape->shortcut last)]])
      (for [shape [:circle :rect :cone :line]]
        [:button (mode-attrs shape)
         [icon {:name (shape->icon shape)}]
         [shortcut (shape->shortcut shape)]])]
     [commands
      [:button
       {:class (css {:active open?}) :on-click #(dispatch :share/initiate)}
       [icon {:name :pip :size 22}]]
      [:button {:key :switch :disabled (not open?) :on-click #(dispatch :share/switch)}
       (if paused?
         [icon {:name :play-fill}]
         [icon {:name :pause-fill}])
       [shortcut "P"]
       [tooltip "Pause / resume"]]]]))
