(ns ogres.app.render.toolbar
  (:require [ogres.app.hooks :refer [use-event-listener use-dispatch use-query]]
            [ogres.app.render :refer [icon]]
            [ogres.app.shortcut :refer [shortcuts]]
            [ogres.app.util :refer [comp-fn]]
            [uix.core :refer [defui $ use-callback use-ref use-state]]))

(def ^:private shortcut-keys
  (into {} (map (juxt :name :keys)) shortcuts))

(defui ^:private tooltip
  [{:keys [tooltip]}]
  (case tooltip
    :copy/cut    "Copy and remove the selected tokens."
    :copy/copy   "Copy the selected tokens."
    :copy/paste  "Paste copied tokens onto the scene."
    :zoom/reset  "Reset to 100% zoom."
    :zoom/zoom   ($ :span "Zoom in or out.")
    :mode/select ($ :span "Hold " ($ :code "Shift") " and drag to select multiple tokens.")
    :mode/ruler  "Measure the distance between two points."
    :mode/circle "Draw a circle starting from its center."
    :mode/rect   "Draw a rectangle from one corner to the other."
    :mode/cone   "Draw a cone whose length is equal to its width."
    :mode/line   "Draw a line from one point to another."
    :mode/poly   "Draw a polygon by clicking each point and closing it at the first point."
    :mode/mask   "Create a new mask by drawing a polygon."
    :mode/mask-toggle "Toggle a mask on or off."
    :mode/mask-remove "Remove a mask."
    :mode/grid   "Select a point on the scene to use as the grid origin."
    :mask/hide   "Mask the entire scene, preserving existing masks."
    :mask/show   "Reveal the entire scene, preserving existing masks."
    :scene/focus "Focus the current view, moving everyone to this scene and position."
    :share/open  "Open the player window."))

(def ^:private query
  [:session/_host
   :local/clipboard
   [:local/type :default :conn]
   [:local/tooltips? :default true]
   [:local/sharing? :default false]
   {:local/camera
    [{:camera/selected [:scene/_tokens]}
     [:camera/draw-mode :default :select]
     [:camera/scale :default 1]]}])

(defui toolbar []
  (let [dispatch  (use-dispatch)
        result    (use-query query)
        node      (use-ref)
        [tooltip-key set-tooltip-key] (use-state nil)

        {type      :local/type
         tooltips? :local/tooltips?
         sharing?  :local/sharing?
         {scale    :camera/scale
          mode     :camera/draw-mode
          selected :camera/selected} :local/camera} result

        mode-attrs
        (fn [value & attrs]
          (apply hash-map
                 :type           "button"
                 :key            value
                 :data-selected  (= value mode)
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
         (if (not (.contains @node (.-target event)))
           (set-tooltip-key nil))) []))

    ($ :.toolbar {:ref node}
      (if tooltip-key
        ($ :.toolbar-tooltip
          (if-let [shortcut (shortcut-keys tooltip-key)]
            ($ :.toolbar-shortcut
              (map (fn [s] ($ :code {:key (str s)} s)) shortcut)))
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
           :disabled (nil? (:local/clipboard result))
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
        ($ :button (mode-attrs :grid :disabled (= type :conn))
          ($ icon {:name "compass"}))
        ($ :button
          {:type "button"
           :disabled (not (and (= type :host) (not (nil? (:session/_host result)))))
           :on-click #(dispatch :session/focus)
           :on-mouse-enter (tooltip-fn :scene/focus)}
          ($ icon {:name "camera2" :size 22}))
        ($ :button
          {:type "button"
           :disabled (= scale 0.15)
           :on-click #(dispatch :camera/zoom-out)
           :on-mouse-enter (tooltip-fn :zoom/zoom)}
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
           :on-mouse-enter (tooltip-fn :zoom/zoom)}
          ($ icon {:name "zoom-in"}))
        ($ :button (mode-attrs :mask :disabled (= type :conn))
          ($ icon {:name "star-half"}))
        ($ :button (mode-attrs :mask-toggle :disabled (= type :conn))
          ($ icon {:name "magic"}))
        ($ :button (mode-attrs :mask-remove :disabled (= type :conn))
          ($ icon {:name "eraser-fill"}))
        ($ :button
          {:type "button"
           :disabled (= type :conn)
           :on-click #(dispatch :mask/fill)
           :on-mouse-enter (tooltip-fn :mask/hide)}
          ($ icon {:name "eye-slash-fill"}))
        ($ :button
          {:type "button"
           :disabled (= type :conn)
           :on-click #(dispatch :mask/clear)
           :on-mouse-enter (tooltip-fn :mask/show)}
          ($ icon {:name "eye-fill"}))
        ($ :button
          {:type "button"
           :disabled (= type :conn)
           :data-selected sharing?
           :on-click #(dispatch :share/initiate)
           :on-mouse-enter (tooltip-fn :share/open)}
          ($ icon {:name "pip" :size 22}))))))
