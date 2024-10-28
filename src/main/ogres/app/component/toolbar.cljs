(ns ogres.app.component.toolbar
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [ogres.app.provider.shortcut :refer [shortcuts]]
            [uix.core :as uix :refer [defui $]]))

(def ^:private shortcut-keys
  (into {} (map (juxt :name :keys)) shortcuts))

(def ^:private action-data
  {"copy-copy"    {:label "Copy the selected objects." :args [:clipboard/copy false]}
   "copy-cut"     {:label "Copy and remove the selected objects." :args [:clipboard/copy true]}
   "copy-paste"   {:label "Paste copied objects onto the scene." :args [:clipboard/paste]}
   "draw-circle"  {:label "Draw a circle." :args [:camera/change-mode :circle]}
   "draw-cone"    {:label "Draw a cone." :args [:camera/change-mode :cone]}
   "draw-line"    {:label "Draw a line." :args [:camera/change-mode :line]}
   "draw-poly"    {:label "Draw a polygon." :args [:camera/change-mode :poly]}
   "draw-rect"    {:label "Draw a rectangle." :args [:camera/change-mode :rect]}
   "mask-create"  {:label "Create a new mask." :args [:camera/change-mode :mask]}
   "mask-hide"    {:label "Mask the entire scene." :args [:scene/mask]}
   "mask-remove"  {:label "Remove a mask." :args [:camera/change-mode :mask-remove]}
   "mask-show"    {:label "Reveal the entire scene." :args [:scene/reveal]}
   "mask-toggle"  {:label "Toggle a mask on and off." :args [:camera/change-mode :mask-toggle]}
   "scene-focus"  {:label "Focus the current view." :args [:session/focus]}
   "scene-grid"   {:label "Grid alignment tool." :args [:camera/change-mode :grid]}
   "scene-ruler"  {:label "Measure distance." :args [:camera/change-mode :ruler]}
   "scene-select" {:label "Hold shift to select multiple tokens." :args [:camera/change-mode :select]}
   "scene-window" {:label "Open the player window." :args [:share/initiate]}
   "zoom-in"      {:label "Zoom in." :args [:camera/zoom-in]}
   "zoom-out"     {:label "Zoom out." :args [:camera/zoom-out]}
   "zoom-reset"   {:label "Reset to 100% zoom." :args [:camera/zoom-reset]}
   "note"         {:label "Create a note." :args [:camera/change-mode :note]}})

(defui ^:private action [props]
  ($ :button
    (merge
     {:type "button"}
     (dissoc props :children)
     {:aria-label (get-in action-data [(:name props) :label])})
    (:children props)))

(def ^:private query
  [:session/_host
   :user/clipboard
   [:user/type :default :conn]
   [:user/sharing? :default false]
   {:user/camera
    [:camera/selected
     [:camera/draw-mode :default :select]
     [:camera/scale :default 1]]}])

(defui toolbar []
  (let [[focused set-focused] (uix/use-state nil)
        dispatch  (hooks/use-dispatch)
        result    (hooks/use-query query)
        {type      :user/type
         share?    :user/sharing?
         {scale    :camera/scale
          mode     :camera/draw-mode
          selected :camera/selected} :user/camera} result
        on-focus (uix/use-callback
                  (fn [event]
                    (if-let [node (.. event -target (closest "button"))]
                      (set-focused (.-name node)))) [])
        on-click (uix/use-callback
                  (fn [event]
                    (let [node (.. event -target (closest "button"))]
                      (if (and (some? node) (not= (.getAttribute node "aria-disabled") "true"))
                        (apply dispatch (get-in action-data [(.-name node) :args]))))) [dispatch])]
    ($ :.toolbar
      {:data-user type :on-pointer-leave #(set-focused nil)}
      (if focused
        ($ :.toolbar-tooltip
          (if-let [shortcut (shortcut-keys focused)]
            ($ :.toolbar-shortcut
              ($ :code (apply str (interpose "+" shortcut)))))
          (get-in action-data [focused :label])))
      ($ :.toolbar-actions
        {:role "toolbar"
         :on-pointer-over on-focus
         :on-focus on-focus
         :on-blur  #(set-focused nil)
         :on-click on-click}
        ($ action {:name "scene-select" :aria-pressed (= mode :select)}
          ($ icon {:name "cursor-fill"}))
        ($ action {:name "copy-cut" :aria-disabled (nil? selected)}
          ($ icon {:name "scissors"}))
        ($ action {:name "copy-copy" :aria-disabled (nil? selected)}
          ($ icon {:name "files"}))
        ($ action {:name "copy-paste" :aria-disabled (nil? (:user/clipboard result))}
          ($ icon {:name "clipboard2-plus"}))
        ($ action {:name "scene-ruler" :aria-pressed (= mode :ruler)}
          ($ icon {:name "rulers"}))
        ($ action {:name "note" :aria-pressed (= mode :note)}
          ($ icon {:name "journal-bookmark-fill"}))
        ($ action {:name "draw-circle" :aria-pressed (= mode :circle)}
          ($ icon {:name "circle"}))
        ($ action {:name "draw-rect" :aria-pressed (= mode :rect)}
          ($ icon {:name "square"}))
        ($ action {:name "draw-cone" :aria-pressed (= mode :cone)}
          ($ icon {:name "triangle"}))
        ($ action {:name "draw-poly" :aria-pressed (= mode :poly)}
          ($ icon {:name "star"}))
        ($ action {:name "draw-line" :aria-pressed (= mode :line)}
          ($ icon {:name "slash-lg"}))
        ($ action {:name "scene-grid" :aria-pressed (= mode :grid)}
          ($ icon {:name "compass"}))
        ($ action {:name "scene-focus" :aria-disabled (not (some? (:session/_host result)))}
          ($ icon {:name "camera2" :size 22}))
        ($ action {:name "zoom-out" :aria-disabled (= scale 0.15)}
          ($ icon {:name "zoom-out"}))
        ($ action {:name "zoom-reset" :aria-disabled (= scale 1)}
          (-> scale (* 100) (js/Math.trunc) (str "%")))
        ($ action {:name "zoom-in" :aria-disabled (= scale 4)}
          ($ icon {:name "zoom-in"}))
        ($ action {:name "mask-create" :aria-pressed (= mode :mask)}
          ($ icon {:name "star-half"}))
        ($ action {:name "mask-toggle" :aria-pressed (= mode :mask-toggle)}
          ($ icon {:name "magic"}))
        ($ action {:name "mask-remove" :aria-pressed (= mode :mask-remove)}
          ($ icon {:name "eraser-fill"}))
        ($ action {:name "mask-show"}
          ($ icon {:name "eye-fill"}))
        ($ action {:name "mask-hide"}
          ($ icon {:name "eye-slash-fill"}))
        ($ action {:name "scene-window" :aria-pressed share?}
          ($ icon {:name "pip" :size 22}))))))
