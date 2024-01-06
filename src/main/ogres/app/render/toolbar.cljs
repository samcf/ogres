(ns ogres.app.render.toolbar
  (:require [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render :refer [icon]]
            [ogres.app.shortcut :refer [shortcuts]]
            [ogres.app.util :refer [comp-fn]]
            [uix.core :refer [defui $ use-callback use-state]]))

(def ^:private shortcut-keys
  (into {} (map (juxt :name :keys)) shortcuts))

(defui ^:private tooltip
  [{:keys [tooltip]}]
  (case tooltip
    :copy/cut         "Copy and remove the selected tokens."
    :copy/copy        "Copy the selected tokens."
    :copy/paste       "Paste copied tokens onto the scene."
    :zoom/reset       "Reset to 100% zoom."
    :zoom/zoom        ($ :span "Zoom in or out.")
    :mode/select      ($ :span "Hold " ($ :code "Shift") " to select multiple tokens.")
    :mode/ruler       "Measure distance."
    :mode/circle      "Draw a circle."
    :mode/rect        "Draw a rectangle."
    :mode/cone        "Draw a cone."
    :mode/line        "Draw a line."
    :mode/poly        "Draw a polygon."
    :mode/mask        "Create a new mask."
    :mode/mask-toggle "Toggle a mask on and off."
    :mode/mask-remove "Remove a mask."
    :mode/grid        "Set the grid origin."
    :mask/hide        "Mask the entire scene."
    :mask/show        "Reveal the entire scene."
    :scene/focus      "Focus the current view."
    :share/open       "Open the player window."))

(def ^:private query
  [:session/_host
   :local/clipboard
   [:local/type :default :conn]
   [:local/sharing? :default false]
   {:local/camera
    [{:camera/selected [:scene/_tokens]}
     [:camera/draw-mode :default :select]
     [:camera/scale :default 1]]}])

(defui toolbar []
  (let [[tooltip-key set-tooltip-key] (use-state nil)
        dispatch   (use-dispatch)
        result     (use-query query)
        {type      :local/type
         sharing?  :local/sharing?
         {scale    :camera/scale
          mode     :camera/draw-mode
          selected :camera/selected} :local/camera} result
        tooltip-fn (use-callback (fn [key] (fn [] (set-tooltip-key key))) [])
        mode-attrs (use-callback
                    (fn [value & attrs]
                      (apply hash-map
                             :type             "button"
                             :key              value
                             :name             (name value)
                             :data-selected    (= value mode)
                             :on-click         (fn [] (dispatch :camera/change-mode value))
                             :on-pointer-enter (tooltip-fn (keyword "mode" (name value)))
                             attrs)) [dispatch tooltip-fn mode])
        copyable   (some (comp-fn contains? identity :scene/_tokens) selected)]
    ($ :.toolbar
      {:on-pointer-leave (tooltip-fn nil) :data-user type}
      (if tooltip-key
        ($ :.toolbar-tooltip
          (if-let [shortcut (shortcut-keys tooltip-key)]
            ($ :.toolbar-shortcut
              ($ :code (apply str (interpose "+" shortcut)))))
          ($ tooltip {:tooltip tooltip-key})))
      ($ :.toolbar-actions
        ($ :button (mode-attrs :select)
          ($ icon {:name "cursor-fill"}))
        ($ :button
          {:name "cut"
           :type "button"
           :disabled (nil? copyable)
           :on-click #(dispatch :clipboard/copy true)
           :on-pointer-enter (tooltip-fn :copy/cut)}
          ($ icon {:name "scissors"}))
        ($ :button
          {:name "copy"
           :type "button"
           :disabled (nil? copyable)
           :on-click #(dispatch :clipboard/copy false)
           :on-pointer-enter (tooltip-fn :copy/copy)}
          ($ icon {:name "files"}))
        ($ :button
          {:name "paste"
           :type "button"
           :disabled (nil? (:local/clipboard result))
           :on-click #(dispatch :clipboard/paste)
           :on-pointer-enter (tooltip-fn :copy/paste)}
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
          {:name "focus"
           :type "button"
           :disabled (not (and (= type :host) (some? (:session/_host result))))
           :on-click #(dispatch :session/focus)
           :on-pointer-enter (tooltip-fn :scene/focus)}
          ($ icon {:name "camera2" :size 22}))
        ($ :button
          {:name "zoom-out"
           :type "button"
           :disabled (= scale 0.15)
           :on-click #(dispatch :camera/zoom-out)
           :on-pointer-enter (tooltip-fn :zoom/zoom)}
          ($ icon {:name "zoom-out"}))
        ($ :button
          {:name "zoom-reset"
           :type "button"
           :disabled (= scale 1)
           :on-click #(dispatch :camera/zoom-reset)
           :on-pointer-enter (tooltip-fn :zoom/reset)}
          (-> scale (* 100) (js/Math.trunc) (str "%")))
        ($ :button
          {:name "zoom-in"
           :type "button"
           :disabled (= scale 4)
           :on-click #(dispatch :camera/zoom-in)
           :on-pointer-enter (tooltip-fn :zoom/zoom)}
          ($ icon {:name "zoom-in"}))
        ($ :button (mode-attrs :mask :disabled (= type :conn))
          ($ icon {:name "star-half"}))
        ($ :button (mode-attrs :mask-toggle :disabled (= type :conn))
          ($ icon {:name "magic"}))
        ($ :button (mode-attrs :mask-remove :disabled (= type :conn))
          ($ icon {:name "eraser-fill"}))
        ($ :button
          {:name "mask-hide"
           :type "button"
           :disabled (= type :conn)
           :on-click #(dispatch :mask/fill)
           :on-pointer-enter (tooltip-fn :mask/hide)}
          ($ icon {:name "eye-slash-fill"}))
        ($ :button
          {:name "mask-show"
           :type "button"
           :disabled (= type :conn)
           :on-click #(dispatch :mask/clear)
           :on-pointer-enter (tooltip-fn :mask/show)}
          ($ icon {:name "eye-fill"}))
        ($ :button
          {:name "player-window"
           :type "button"
           :disabled (= type :conn)
           :data-selected sharing?
           :on-click #(dispatch :share/initiate)
           :on-pointer-enter (tooltip-fn :share/open)}
          ($ icon {:name "pip" :size 22}))))))
