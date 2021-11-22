(ns ogre.tools.render.command
  (:require [ogre.tools.render :refer [icon listen!]]
            [ogre.tools.state :refer [use-query]]
            [uix.core.alpha :as uix]))

(defn shortcut [key]
  [:div.commands-shortcut key])

(defmulti tooltip :key)

(defmethod tooltip :zoom/reset []
  "Reset to 100% zoom.")

(defmethod tooltip :zoom/out []
  [:span "Use the " [:code "Mousewheel"] " or pinch the trackpad to zoom in and out."])

(defmethod tooltip :zoom/in []
  [:span "Use the " [:code "Mousewheel"] " or pinch the trackpad to zoom in and out."])

(defmethod tooltip :mode/select []
  [:span "Hold " [:code "Shift"] " and drag to select multiple tokens."])

(defmethod tooltip :mode/ruler []
  "Measure the distance between two points.")

(defmethod tooltip :mode/circle []
  "Draw a circle, starting from its center.")

(defmethod tooltip :mode/rect []
  "Draw a rectangle from one corner to the other.")

(defmethod tooltip :mode/cone []
  "Draw a cone whose length is equal to its width.")

(defmethod tooltip :mode/line []
  "Draw a line from one point to another.")

(defmethod tooltip :share/open []
  [:span "Open or close the player window. "
   [:a {:href "https://github.com/samcf/ogre.tools/wiki#player-window"
        :target "_blank"}
    "Learn more"] "."])

(defmethod tooltip :share/play []
  "Resumes updates to the player window.")

(defmethod tooltip :share/pause []
  "Pauses updates to the player window. Good time to setup an ambush!")

(defmethod tooltip :default [] nil)

(def query
  {:pull
   [[:root/tooltips? :default false]
    [:share/open? :default false]
    [:share/paused? :default false]
    {:root/canvas
     [[:canvas/mode :default :select]
      [:zoom/scale :default 1]]}]})

(defn command [props]
  (let [[data dispatch] (use-query query)
        container       (uix/ref)
        tooltip-key     (uix/state nil)
        canvas          (:root/canvas data)

        mode-attrs
        (fn [mode]
          {:type "button"
           :key mode
           :css {:selected (= mode (:canvas/mode canvas))}
           :on-click #(dispatch :canvas/toggle-mode mode)
           :on-mouse-enter #(reset! tooltip-key (keyword "mode" (name mode)))})

        element
        (if (and (not (nil? @tooltip-key))
                 (:root/tooltips? data))
          js/window nil)]

    (listen!
     (fn [event]
       (if (not (.contains @container (.-target event)))
         (reset! tooltip-key nil))) element "mouseover" [element])

    [:div.commands {:ref container}
     (if @tooltip-key
       [:div.commands-tooltip
        [tooltip {:key @tooltip-key}]])
     [:div.commands-groups
      [:div.commands-group
       [:button.command-zoom
        {:disabled (= (:zoom/scale canvas) 1)
         :on-click #(dispatch :zoom/reset)
         :on-mouse-enter #(reset! tooltip-key :zoom/reset)}
        (-> (:zoom/scale canvas) (* 100) (js/Math.trunc) (str "%"))]
       [:button
        {:disabled (= (:zoom/scale canvas) 0.15)
         :on-click #(dispatch :zoom/out)
         :on-mouse-enter #(reset! tooltip-key :zoom/out)}
        [icon {:name "zoom-out"}]]
       [:button
        {:disabled (= (:zoom/scale canvas) 4)
         :on-click #(dispatch :zoom/in)
         :on-mouse-enter #(reset! tooltip-key :zoom/in)}
        [icon {:name "zoom-in"}]]]
      [:div.commands-group
       [:button (mode-attrs :select) [icon {:name "cursor"}] [shortcut "S"]]
       [:button (mode-attrs :ruler) [icon {:name "rulers"}] [shortcut "R"]]
       [:button (mode-attrs :circle) [icon {:name "circle"}] [shortcut "1"]]
       [:button (mode-attrs :rect) [icon {:name "square"}] [shortcut "2"]]
       [:button (mode-attrs :cone) [icon {:name "triangle"}] [shortcut "3"]]
       [:button (mode-attrs :line) [icon {:name "slash-lg"}] [shortcut "4"]]]
      [:div.commands-group
       [:button
        {:css {:active (:share/open? data)}
         :on-click #(dispatch :share/initiate)
         :on-mouse-enter #(reset! tooltip-key :share/open)}
        [icon {:name "pip" :size 22}] [shortcut "W"]]
       [:button
        {:disabled (or (not (:share/open? data)) (not (:share/paused? data)))
         :on-click #(dispatch :share/switch)
         :on-mouse-enter #(reset! tooltip-key :share/play)}
        [icon {:name "play-fill" :size 22}]]
       [:button
        {:disabled (or (not (:share/open? data)) (:share/paused? data))
         :on-click #(dispatch :share/switch)
         :on-mouse-enter #(reset! tooltip-key :share/pause)}
        [icon {:name "pause-fill" :size 22}]]]]]))
