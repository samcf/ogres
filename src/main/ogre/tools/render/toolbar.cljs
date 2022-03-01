(ns ogre.tools.render.toolbar
  (:require [ogre.tools.render :refer [icon listen!]]
            [ogre.tools.state :refer [use-query]]
            [uix.core.alpha :as uix]))

(defn shortcut [key]
  [:div.toolbar-shortcut key])

(defn tooltip [key]
  (case key
    :zoom/reset  "Reset to 100% zoom."
    :zoom/out    [:span "Use the " [:code "Mousewheel"] " or pinch the trackpad to zoom in and out."]
    :zoom/in     [:span "Use the " [:code "Mousewheel"] " or pinch the trackpad to zoom in and out."]
    :mode/select [:span "Hold " [:code "Shift"] " and drag to select multiple tokens."]
    :mode/ruler  "Measure the distance between two points."
    :mode/circle "Draw a circle, starting from its center."
    :mode/rect   "Draw a rectangle from one corner to the other."
    :mode/cone   "Draw a cone whose length is equal to its width."
    :mode/line   "Draw a line from one point to another."
    :mode/poly   "Draw a polygon by clicking each point, close by clicking the start."
    :mode/fog    "Reveal or hide parts of the canvas."
    :share/open  [:span "Open or close the player window. "
                  [:a {:href "https://github.com/samcf/ogre.tools/wiki#player-window" :target "_blank"} "Learn more"] "."]
    :share/play  "Resumes updates to the player window."
    :share/pause "Pauses updates to the player window. Good time to setup an ambush!"
    :fog/hide    "Hide the entire canvas."
    :fog/show    "Reveal the entire canvas."
    ""))

(def query
  {:pull
   [[:root/tooltips? :default true]
    [:share/open? :default false]
    [:share/paused? :default false]
    {:root/canvas
     [[:canvas/mode :default :select]
      [:zoom/scale :default 1]]}]})

(defn toolbar []
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

        tooltip-fn
        (fn [key]
          (fn []
            (reset! tooltip-key key)))

        element
        (if (and (not (nil? @tooltip-key))
                 (:root/tooltips? data))
          js/window nil)]

    (listen!
     (fn [event]
       (if (not (.contains @container (.-target event)))
         (reset! tooltip-key nil))) element "mouseover" [element])

    [:div.toolbar {:ref container}
     (if @tooltip-key
       [:div.toolbar-tooltip
        [tooltip @tooltip-key]])
     [:div.toolbar-groups
      [:div.toolbar-group
       [:button.toolbar-zoom
        {:disabled (= (:zoom/scale canvas) 1)
         :on-click #(dispatch :zoom/reset)
         :on-mouse-enter (tooltip-fn :zoom/reset)}
        (-> (:zoom/scale canvas) (* 100) (js/Math.trunc) (str "%"))]
       [:button
        {:disabled (= (:zoom/scale canvas) 0.15)
         :on-click #(dispatch :zoom/out)
         :on-mouse-enter (tooltip-fn :zoom/out)}
        [icon {:name "zoom-out"}]]
       [:button
        {:disabled (= (:zoom/scale canvas) 4)
         :on-click #(dispatch :zoom/in)
         :on-mouse-enter (tooltip-fn :zoom/in)}
        [icon {:name "zoom-in"}]]]
      [:div.toolbar-group
       [:button (mode-attrs :select) [icon {:name "cursor"}] [shortcut "S"]]
       [:button (mode-attrs :ruler) [icon {:name "rulers"}] [shortcut "R"]]
       [:button (mode-attrs :circle) [icon {:name "circle"}] [shortcut "1"]]
       [:button (mode-attrs :rect) [icon {:name "square"}] [shortcut "2"]]
       [:button (mode-attrs :cone) [icon {:name "triangle"}] [shortcut "3"]]
       [:button (mode-attrs :poly) [icon {:name "star"}] [shortcut "4"]]
       [:button (mode-attrs :line) [icon {:name "slash-lg"}] [shortcut "5"]]]
      [:div.toolbar-group
       [:button {:on-mouse-enter (tooltip-fn :fog/hide)}
        [icon {:name "mask"}]]
       [:button {:on-mouse-enter (tooltip-fn :fog/show)}
        [icon {:name "eraser"}]]
       [:button (mode-attrs :fog)
        [icon {:name "star-half"}]
        [shortcut "F"]]]
      [:div.toolbar-group
       [:button
        {:css {:active (:share/open? data)}
         :on-click #(dispatch :share/initiate)
         :on-mouse-enter (tooltip-fn :share/open)}
        [icon {:name "pip" :size 22}] [shortcut "W"]]
       [:button
        {:disabled (or (not (:share/open? data)) (not (:share/paused? data)))
         :on-click #(dispatch :share/switch)
         :on-mouse-enter (tooltip-fn :share/play)}
        [icon {:name "play-fill" :size 22}]]
       [:button
        {:disabled (or (not (:share/open? data)) (:share/paused? data))
         :on-click #(dispatch :share/switch)
         :on-mouse-enter (tooltip-fn :share/pause)}
        [icon {:name "pause-fill" :size 22}]]]]]))
