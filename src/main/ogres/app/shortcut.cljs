(ns ogres.app.shortcut
  (:require [ogres.app.hooks :refer [use-dispatch use-event-listener use-shortcut]]
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
  [{:name "scene-select"  :keys [\s]            :desc "Select: Pan, select, and move tokens"}
   {:name "select-many"   :keys [\s "Shift"]    :desc "Select: Hold to select multiple tokens"}
   {:name "select-clear"  :keys ["Escape"]      :desc "Select: Clear selection"}
   {:name "select-remove" :keys ["Delete"]      :desc "Select: Remove selected tokens"}
   {:name "copy-cut"      :keys [\⌘ \x]         :desc "Copy: Copy and remove the selected tokens"}
   {:name "copy-copy"     :keys [\⌘ \c]         :desc "Copy: Copy the selected tokens"}
   {:name "copy-paste"    :keys [\⌘ \v]         :desc "Copy: Paste the copied tokens onto the scene"}
   {:name "zoom-in"       :keys ["Mouse wheel"] :desc "Zoom: Zoom in"}
   {:name "zoom-out"      :keys ["Mouse wheel"] :desc "Zoom: Zoom out"}
   {:name "scene-ruler"   :keys [\r]            :desc "Mode: Ruler tool"}
   {:name "scene-grid"    :keys [\g]            :desc "Mode: Grid alignment tool"}
   {:name "draw-circle"   :keys [\1]            :desc "Mode: Draw a circle"}
   {:name "draw-rect"     :keys [\2]            :desc "Mode: Draw a rectangle"}
   {:name "draw-cone"     :keys [\3]            :desc "Mode: Draw a cone"}
   {:name "draw-poly"     :keys [\4]            :desc "Mode: Draw a polygon"}
   {:name "draw-line"     :keys [\5]            :desc "Mode: Draw a line"}
   {:name "mask-create"   :keys [\f]            :desc "Mode: Draw a fog shape"}
   {:name "mask-toggle"   :keys [\t]            :desc "Mode: Reveal or obscure a fog shape"}
   {:name "mask-remove"   :keys [\x]            :desc "Mode: Remove a fog shape"}])

(defui handlers []
  (let [dispatch (use-dispatch)]

    ;; Zoom the camera in and out with the mousewheel or trackpad.
    (use-event-listener "wheel"
      (fn [event]
        (if (.closest (.-target event) ".scene")
          (let [ctrl (.-ctrlKey event)
                posx (.-clientX event)
                posy (.-clientY event)
                delt (.-deltaY event)]
            (.preventDefault event)
            (dispatch :camera/zoom-delta posx posy delt ctrl)))))

    ;; Zoom the camera in and out with keyboard.
    (use-shortcut [\= \-]
      (fn [data]
        (let [event (.-originalEvent data)]
          (if (and (allowed? event) (or (.-ctrlKey event) (.-metaKey event)))
            (do (.preventDefault data)
                (case (.-key data)
                  \= (dispatch :camera/zoom-in)
                  \- (dispatch :camera/zoom-out)))))))

    ;; Change draw mode.
    (use-shortcut [\1 \2 \3 \4 \5 \f \g \r \s \t \x]
      (fn [data]
        (let [event (.-originalEvent data)]
          (if (and (allowed? event) (not (or (.-metaKey event) (.-ctrlKey event))))
            (dispatch :camera/change-mode (key->mode (.-key data)))))))

    ;; Select a focused token.
    (use-shortcut [\ ]
      (fn [data]
        (let [shift (.. data -originalEvent -shiftKey)
              data  (.. js/document -activeElement -dataset)]
          (if (= (.-type data) "token")
            (dispatch :element/select (js/Number (.-id data)) shift)))))

    ;; Move tokens and pan camera around.
    (use-shortcut ["arrowleft" "arrowup" "arrowright" "arrowdown"]
      (fn [data]
        (if (allowed? (.-originalEvent data))
          (let [[dx dy] (key->vector (.-key data))
                attrs (.. js/document -activeElement -dataset)]
            (cond (.. data -originalEvent -altKey)
                  (dispatch :camera/translate (* dx 140) (* dy 140))
                  (= (.-type attrs) "scene")
                  (dispatch :camera/translate (* dx 140) (* dy 140))
                  (= (.-type attrs) "token")
                  (dispatch :token/translate (js/Number (.-id attrs)) (* dx 70) (* dy 70))
                  (= (.-activeElement js/document) (.-body js/document))
                  (dispatch :token/translate-selected (* dx 70) (* dy 70)))))))

    ;; Cut, copy, and paste tokens.
    (use-shortcut [\c \x \v]
      (fn [data]
        (let [event (.-originalEvent data)]
          (if (and (allowed? event) (or (.-ctrlKey event) (.-metaKey event)))
            (case (.-key data)
              \c (dispatch :clipboard/copy)
              \x (dispatch :clipboard/copy true)
              \v (dispatch :clipboard/paste))))))

    ;; Removes all token selections and revert the draw mode to select.
    (use-shortcut ["escape"]
      (fn [data]
        (if (allowed? (.-originalEvent data))
          (dispatch :shortcut/escape))))

    ;; Removes selected or focused tokens.
    (use-shortcut ["delete" "backspace"]
      (fn [data]
        (if (allowed? (.-originalEvent data))
          (let [attrs (.. js/document -activeElement -dataset)]
            (if (= (.-type attrs) "token")
              (dispatch :token/remove [(js/Number (.-id attrs))])
              (dispatch :selection/remove))))))))
