(ns ogres.app.render.toolbar
  (:require [ogres.app.hooks :refer [use-listen use-dispatch use-query]]
            [ogres.app.render :refer [css icon]]
            [uix.core :refer [defui $ use-ref use-state]]))

(defui ^{:private true} shortcut [{:keys [name]}]
  ($ :div.toolbar-shortcut name))

(defui ^{:private true} tooltip [{:keys [tooltip]}]
  (case tooltip
    :zoom/reset  "Reset to 100% zoom."
    :zoom/out    ($ :span "Use the " ($ :code "Mousewheel") " or pinch the trackpad to zoom in and out.")
    :zoom/in     ($ :span "Use the " ($ :code "Mousewheel") " or pinch the trackpad to zoom in and out.")
    :mode/select ($ :span "Hold " ($ :code "Shift") " and drag to select multiple tokens.")
    :mode/ruler  "Measure the distance between two points."
    :mode/circle "Draw a circle, starting from its center."
    :mode/rect   "Draw a rectangle from one corner to the other."
    :mode/cone   "Draw a cone whose length is equal to its width."
    :mode/line   "Draw a line from one point to another."
    :mode/poly   "Draw any shape by clicking each point and closing it at the first point."
    :mode/mask   ($ :span "Create a new mask by drawing a polygon; hold " ($ :code "Shift") " to reveal a masked area.")
    :mode/mask-toggle "Toggle a mask on or off."
    :mode/mask-remove "Remove a mask."
    :mask/hide   "Remove all masks and then mask the entire scene."
    :mask/show   "Remove all masks and reveal the entire scene."
    :share/play  "Resumes updates to the player window."
    :share/pause "Pauses updates to the player window. Good time to setup an ambush!"
    :share/open  ($ :span "Open or close the player window. "
                   ($ :a {:href   "https://github.com/samcf/ogres.app/wiki#player-window"
                          :target "_blank"} "Learn more") ".")))

(def ^{:private true} query
  [[:local/type :default :conn]
   [:local/tooltips? :default true]
   [:local/sharing? :default false]
   [:local/paused? :default false]
   {:local/window
    [[:window/draw-mode :default :select]
     [:window/scale :default 1]]}])

(defui toolbar []
  (let [dispatch  (use-dispatch)
        data      (use-query query)
        container (use-ref)
        [tooltip-key set-tooltip-key] (use-state nil)

        {type      :local/type
         tooltips? :local/tooltips?
         sharing?  :local/sharing?
         paused?   :local/paused?
         {scale :window/scale
          mode  :window/draw-mode} :local/window} data

        conn? (= type :conn)

        mode-attrs
        (fn [value & attrs]
          (apply hash-map
                 :type           "button"
                 :key            value
                 :class          (css {:selected (= value mode)})
                 :on-click       #(dispatch :window/change-mode value)
                 :on-mouse-enter #(set-tooltip-key (keyword "mode" (name value)))
                 attrs))

        tooltip-fn
        (fn [key]
          (fn []
            (set-tooltip-key key)))

        element
        (if (and (not (nil? tooltip-key)) tooltips?)
          js/window nil)]

    (use-listen
     (fn [event]
       (if (not (.contains @container (.-target event)))
         (set-tooltip-key nil))) element "mouseover")

    ($ :div.toolbar {:ref container}
      (if tooltip-key
        ($ :div.toolbar-tooltip
          ($ tooltip {:tooltip tooltip-key})))
      ($ :div.toolbar-groups
        ($ :div.toolbar-group
          ($ :button.toolbar-zoom
            {:disabled (= scale 1)
             :on-click #(dispatch :zoom/reset)
             :on-mouse-enter (tooltip-fn :zoom/reset)}
            (-> scale (* 100) (js/Math.trunc) (str "%")))
          ($ :button
            {:disabled (= scale 0.15)
             :on-click #(dispatch :zoom/out)
             :on-mouse-enter (tooltip-fn :zoom/out)}
            ($ icon {:name "zoom-out"}))
          ($ :button
            {:disabled (= scale 4)
             :on-click #(dispatch :zoom/in)
             :on-mouse-enter (tooltip-fn :zoom/in)}
            ($ icon {:name "zoom-in"})))
        ($ :div.toolbar-group
          ($ :button (mode-attrs :select)
            ($ icon {:name "cursor-fill"})
            ($ shortcut {:name "S"}))
          ($ :button (mode-attrs :ruler)
            ($ icon {:name "rulers"})
            ($ shortcut {:name "R"}))
          ($ :button (mode-attrs :circle)
            ($ icon {:name "circle"})
            ($ shortcut {:name "1"}))
          ($ :button (mode-attrs :rect)
            ($ icon {:name "square"})
            ($ shortcut {:name "2"}))
          ($ :button (mode-attrs :cone)
            ($ icon {:name "triangle"})
            ($ shortcut {:name "3"}))
          ($ :button (mode-attrs :poly)
            ($ icon {:name "star"})
            ($ shortcut {:name "4"}))
          ($ :button (mode-attrs :line)
            ($ icon {:name "slash-lg"})
            ($ shortcut {:name "5"})))
        ($ :div.toolbar-group
          ($ :button (mode-attrs :mask :disabled conn?)
            ($ icon {:name "star-half"})
            ($ shortcut {:name "F"}))
          ($ :button (mode-attrs :mask-toggle :disabled conn?)
            ($ icon {:name "magic"})
            ($ shortcut {:name "T"}))
          ($ :button (mode-attrs :mask-remove :disabled conn?)
            ($ icon {:name "eraser-fill"})
            ($ shortcut {:name "X"}))
          ($ :button
            {:disabled conn?
             :on-click #(dispatch :mask/fill)
             :on-mouse-enter (tooltip-fn :mask/hide)}
            ($ icon {:name "eye-slash-fill"}))
          ($ :button
            {:disabled conn?
             :on-click #(dispatch :mask/clear)
             :on-mouse-enter (tooltip-fn :mask/show)}
            ($ icon {:name "eye-fill"})))
        ($ :div.toolbar-group
          ($ :button
            {:class (css {:active sharing?})
             :disabled conn?
             :on-click #(dispatch :share/initiate)
             :on-mouse-enter (tooltip-fn :share/open)}
            ($ icon {:name "pip" :size 22}))
          ($ :button
            {:disabled (or conn? (not sharing?) (not paused?))
             :on-click #(dispatch :share/switch)
             :on-mouse-enter (tooltip-fn :share/play)}
            ($ icon {:name "play-fill" :size 22}))
          ($ :button
            {:disabled (or conn? (not sharing?) paused?)
             :on-click #(dispatch :share/switch)
             :on-mouse-enter (tooltip-fn :share/pause)}
            ($ icon {:name "pause-fill" :size 22})))))))
