(ns ogres.app.render.toolbar
  (:require [ogres.app.hooks :refer [use-event-listener use-dispatch use-query]]
            [ogres.app.render :refer [css icon]]
            [ogres.app.util :refer [comp-fn]]
            [uix.core :refer [defui $ use-callback use-ref use-state]]))

(defui ^:private tooltip
  [{:keys [tooltip]}]
  (case tooltip
    :copy/cut    "Copy the selected tokens and remove them from the scene."
    :copy/copy   "Copy the selected tokens."
    :copy/paste  "Place copied tokens onto the scene."
    :zoom/reset  "Reset to 100% zoom."
    :zoom/out    ($ :span "Use the mousewheel or pinch the trackpad to zoom in and out.")
    :zoom/in     ($ :span "Use the mousewheel or pinch the trackpad to zoom in and out.")
    :mode/select ($ :span "Hold shift and drag to select multiple tokens.")
    :mode/ruler  "Measure the distance between two points."
    :mode/circle "Draw a circle starting from its center."
    :mode/rect   "Draw a rectangle from one corner to the other."
    :mode/cone   "Draw a cone whose length is equal to its width."
    :mode/line   "Draw a line from one point to another."
    :mode/poly   "Draw a polygon by clicking each point and closing it at the first point."
    :mode/mask   ($ :span "Create a new mask by drawing a polygon; hold shift to reveal a masked area.")
    :mode/mask-toggle "Toggle a mask on or off."
    :mode/mask-remove "Remove a mask."
    :mask/hide   "Remove all masks then mask the entire scene."
    :mask/show   "Remove all masks then reveal the entire scene."
    :scene/focus "Focus the current view, moving everyone to this scene and position."
    :share/play  "Resume updates to the player window."
    :share/pause "Pause updates to the player window."
    :share/open  ($ :span "Open the player window. "
                   ($ :a {:href   "https://github.com/samcf/ogres.app/wiki#player-window"
                          :target "_blank"} "Learn more") ".")))

(def ^:private query
  [:session/_host
   :local/clipboard
   [:local/type :default :conn]
   [:local/tooltips? :default true]
   [:local/sharing? :default false]
   [:local/paused? :default false]
   {:local/camera
    [{:camera/selected [:scene/_tokens]}
     [:camera/draw-mode :default :select]
     [:camera/scale :default 1]]}])

(defui toolbar []
  (let [dispatch  (use-dispatch)
        result    (use-query query)
        container (use-ref)
        [tooltip-key set-tooltip-key] (use-state nil)

        {type      :local/type
         tooltips? :local/tooltips?
         sharing?  :local/sharing?
         paused?   :local/paused?
         clipboard :local/clipboard
         {scale    :camera/scale
          mode     :camera/draw-mode
          selected :camera/selected} :local/camera} result

        conn? (= type :conn)

        mode-attrs
        (fn [value & attrs]
          (apply hash-map
                 :type           "button"
                 :key            value
                 :class          (css {:selected (= value mode)})
                 :on-click       #(dispatch :camera/change-mode value)
                 :on-mouse-enter #(set-tooltip-key (keyword "mode" (name value)))
                 attrs))

        tooltip-fn
        (fn [key]
          (fn []
            (set-tooltip-key key)))

        element
        (if (and (not (nil? tooltip-key)) tooltips?)
          js/window nil)

        copyable
        (some (comp-fn contains? identity :scene/_tokens) selected)]

    (use-event-listener element "mouseover"
      (use-callback
       (fn [event]
         (if (not (.contains @container (.-target event)))
           (set-tooltip-key nil))) []))

    ($ :.toolbar {:ref container}
      (if tooltip-key
        ($ :.toolbar-tooltip
          ($ tooltip {:tooltip tooltip-key})))
      ($ :.toolbar-groups
        ($ :button (mode-attrs :select)
          ($ icon {:name "cursor-fill"}))
        ($ :button
          {:type "button"
           :disabled (nil? copyable)
           :on-click #(dispatch :clipboard/copy true)
           :on-mouse-enter (tooltip-fn :copy/cut)}
          ($ icon {:name "scissors"}))
        ($ :button
          {:type "button"
           :disabled (nil? copyable)
           :on-click #(dispatch :clipboard/copy false)
           :on-mouse-enter (tooltip-fn :copy/copy)}
          ($ icon {:name "files"}))
        ($ :button
          {:type "button"
           :disabled (nil? clipboard)
           :on-click #(dispatch :clipboard/paste)
           :on-mouse-enter (tooltip-fn :copy/paste)}
          ($ icon {:name "clipboard2-plus"}))
        ($ :button (mode-attrs :ruler)
          ($ icon {:name "rulers"}))
        ($ :button (mode-attrs :circle)
          ($ icon {:name "circle"}))
        ($ :button (mode-attrs :rect)
          ($ icon {:name "square"}))
        ($ :button (mode-attrs :cone)
          ($ icon {:name "triangle"}))
        ($ :button (mode-attrs :poly)
          ($ icon {:name "star"}))
        ($ :button (mode-attrs :line)
          ($ icon {:name "slash-lg"}))
        ($ :button
          {:type "button"
           :class (css {:active sharing?})
           :disabled conn?
           :on-click #(dispatch :share/initiate)
           :on-mouse-enter (tooltip-fn :share/open)}
          ($ icon {:name "pip" :size 22}))
        ($ :button
          {:type "button"
           :disabled (or conn? (not sharing?) (not paused?))
           :on-click #(dispatch :share/switch)
           :on-mouse-enter (tooltip-fn :share/play)}
          ($ icon {:name "play-fill" :size 22}))
        ($ :button
          {:type "button"
           :disabled (or conn? (not sharing?) paused?)
           :on-click #(dispatch :share/switch)
           :on-mouse-enter (tooltip-fn :share/pause)}
          ($ icon {:name "pause-fill" :size 22}))
        ($ :button
          {:type "button"
           :disabled (= scale 0.15)
           :on-click #(dispatch :camera/zoom-out)
           :on-mouse-enter (tooltip-fn :zoom/out)}
          ($ icon {:name "zoom-out"}))
        ($ :button
          {:type "button"
           :disabled (= scale 1)
           :style {:grid-column "span 3"}
           :on-click #(dispatch :camera/zoom-reset)
           :on-mouse-enter (tooltip-fn :zoom/reset)}
          (-> scale (* 100) (js/Math.trunc) (str "%")))
        ($ :button
          {:type "button"
           :disabled (= scale 4)
           :on-click #(dispatch :camera/zoom-in)
           :on-mouse-enter (tooltip-fn :zoom/in)}
          ($ icon {:name "zoom-in"}))
        ($ :button (mode-attrs :mask :disabled conn?)
          ($ icon {:name "star-half"}))
        ($ :button (mode-attrs :mask-toggle :disabled conn?)
          ($ icon {:name "magic"}))
        ($ :button (mode-attrs :mask-remove :disabled conn?)
          ($ icon {:name "eraser-fill"}))
        ($ :button
          {:type "button"
           :disabled conn?
           :on-click #(dispatch :mask/fill)
           :on-mouse-enter (tooltip-fn :mask/hide)}
          ($ icon {:name "eye-slash-fill"}))
        ($ :button
          {:type "button"
           :disabled conn?
           :on-click #(dispatch :mask/clear)
           :on-mouse-enter (tooltip-fn :mask/show)}
          ($ icon {:name "eye-fill"}))
        ($ :button
          {:type "button"
           :disabled (not (and (= type :host) (not (nil? (:session/_host result)))))
           :on-click #(dispatch :session/focus)
           :on-mouse-enter (tooltip-fn :scene/focus)}
          ($ icon {:name "camera2" :size 22}))))))
