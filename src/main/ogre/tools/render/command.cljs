(ns ogre.tools.render.command
  (:require [ogre.tools.render :refer [icon]]
            [ogre.tools.state :refer [use-query]]))

(def shape->icon
  {:circle "circle"
   :rect "square"
   :cone "triangle"
   :line "slash-lg"})

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
   {:root/canvas
    [[:canvas/mode :default :select]
     [:canvas/last-shape :default :circle]]}])

(defn command [props]
  (let [[data dispatch] (use-query {:pull attrs})
        canvas          (:root/canvas data)
        mode-attrs
        (fn [given]
          {:type "button"
           :key given
           :css {:selected (= given (:canvas/mode canvas))}
           :on-click #(dispatch :canvas/toggle-mode given)})]
    [:div.commands
     [:button (mode-attrs :select)
      [icon {:name "cursor"}]
      [shortcut "S"]
      [tooltip "Select"]]
     [:button (mode-attrs :ruler)
      [icon {:name "rulers"}]
      [shortcut "R"]
      [tooltip "Ruler"]]
     [commands
      (let [last (:canvas/last-shape canvas)]
        [:button (mode-attrs last)
         [icon {:name (shape->icon last)}]
         [shortcut (shape->shortcut last)]])
      (for [shape [:circle :rect :cone :line]]
        [:button (mode-attrs shape)
         [icon {:name (shape->icon shape)}]
         [shortcut (shape->shortcut shape)]])]
     [:button
      {:css {:active (:share/open? data)} :on-click #(dispatch :share/initiate)}
      [icon {:name "pip" :size 22}]
      [shortcut "W"]
      [tooltip "Toggle Player Window"]]]))
