(ns ogres.app.provider.shortcut
  (:require [ogres.app.hooks :as hooks]
            [uix.core :refer [defui]]))

(defn ^:private allowed? [event]
  (let [element (.-target event)]
    (not (instance? js/HTMLInputElement element))))

(def ^:private key->vector
  {"arrowleft"  [-1 0]
   "arrowup"    [0 -1]
   "arrowright" [1 0]
   "arrowdown"  [0 1]})

(def ^:private key->mode
  {\1 :circle
   \2 :rect
   \3 :cone
   \4 :poly
   \5 :line
   \f :mask
   \g :grid
   \r :ruler
   \s :select
   \t :mask-toggle
   \x :mask-remove})

(def shortcuts
  [{:name "scene-select"  :keys [\s]}
   {:name "select-many"   :keys [\s "Shift"]}
   {:name "select-clear"  :keys ["Escape"]}
   {:name "select-remove" :keys ["Delete"]}
   {:name "copy-cut"      :keys [\⌘ \x]}
   {:name "copy-copy"     :keys [\⌘ \c]}
   {:name "copy-paste"    :keys [\⌘ \v]}
   {:name "zoom-in"       :keys ["Mouse wheel"]}
   {:name "zoom-out"      :keys ["Mouse wheel"]}
   {:name "scene-ruler"   :keys [\r]}
   {:name "scene-grid"    :keys [\g]}
   {:name "draw-circle"   :keys [\1]}
   {:name "draw-rect"     :keys [\2]}
   {:name "draw-cone"     :keys [\3]}
   {:name "draw-poly"     :keys [\4]}
   {:name "draw-line"     :keys [\5]}
   {:name "mask-create"   :keys [\f]}
   {:name "mask-toggle"   :keys [\t]}
   {:name "mask-remove"   :keys [\x]}])

(defui listeners []
  (let [dispatch (hooks/use-dispatch)]

    ;; Zoom the camera in and out with the mousewheel or trackpad.
    (hooks/use-event-listener "wheel"
      (fn [event]
        (if (.closest (.-target event) ".scene")
          (let [ctrl (.-ctrlKey event)
                posx (.-clientX event)
                posy (.-clientY event)
                delt (.-deltaY event)]
            (.preventDefault event)
            (dispatch :camera/zoom-delta posx posy delt ctrl)))))

    ;; Zoom the camera in and out with keyboard.
    (hooks/use-shortcut [\= \-]
      (fn [data]
        (let [event (.-originalEvent data)]
          (if (and (allowed? event) (or (.-ctrlKey event) (.-metaKey event)))
            (do (.preventDefault data)
                (case (.-key data)
                  \= (dispatch :camera/zoom-in)
                  \- (dispatch :camera/zoom-out)))))))

    ;; Change draw mode.
    (hooks/use-shortcut [\1 \2 \3 \4 \5 \f \g \r \s \t \x]
      (fn [data]
        (let [event (.-originalEvent data)]
          (if (and (allowed? event) (not (or (.-metaKey event) (.-ctrlKey event))))
            (dispatch :camera/change-mode (key->mode (.-key data)))))))

    ;; Select a focused object.
    (hooks/use-shortcut [\ ]
      (fn [data]
        (let [shift (.. data -originalEvent -shiftKey)
              data  (.. js/document -activeElement -dataset)
              type  (.-type data)]
          (if (or (= type "shape") (= type "token"))
            (dispatch :objects/select (js/Number (.-id data)) shift)))))

    ;; Move tokens and pan camera around.
    (hooks/use-shortcut ["arrowleft" "arrowup" "arrowright" "arrowdown"]
      (fn [data]
        (if (allowed? (.-originalEvent data))
          (let [[dx dy] (key->vector (.-key data))
                attrs (.. js/document -activeElement -dataset)]
            (cond (.. data -originalEvent -altKey)
                  (dispatch :camera/translate (* dx 140) (* dy 140))
                  (= (.-type attrs) "scene")
                  (dispatch :camera/translate (* dx 140) (* dy 140))
                  (= (.-type attrs) "token")
                  (dispatch :objects/translate (js/Number (.-id attrs)) (* dx 70) (* dy 70))
                  (= (.-type attrs) "shape")
                  (dispatch :objects/translate (js/Number (.-id attrs)) (* dx 70) (* dy 70))
                  (= (.-activeElement js/document) (.-body js/document))
                  (dispatch :objects/translate-selected (* dx 70) (* dy 70)))))))

    ;; Cut, copy, and paste objects.
    (hooks/use-shortcut [\c \x \v]
      (fn [data]
        (let [event (.-originalEvent data)]
          (if (and (allowed? event) (or (.-ctrlKey event) (.-metaKey event)))
            (case (.-key data)
              \c (dispatch :clipboard/copy)
              \x (dispatch :clipboard/copy true)
              \v (dispatch :clipboard/paste))))))

    ;; Removes all object selections and reverts the draw mode to select.
    (hooks/use-shortcut ["escape"]
      (fn [data]
        (if (allowed? (.-originalEvent data))
          (dispatch :shortcut/escape))))

    ;; Removes selected or focused objects.
    (hooks/use-shortcut ["delete" "backspace"]
      (fn [data]
        (if (allowed? (.-originalEvent data))
          (let [attr (.. js/document -activeElement -dataset)
                type (.-type attr)]
            (if (or (= type "shape") (= type "token"))
              (dispatch :objects/remove [(js/Number (.-id attr))])
              (dispatch :selection/remove))))))))
