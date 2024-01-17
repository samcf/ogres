(ns ogres.app.shortcut
  (:require [datascript.core :refer [pull]]
            [ogres.app.hooks :refer [use-event-listener use-dispatch]]
            [ogres.app.provider.state :refer [context]]
            [uix.core :refer [defui use-callback use-context]]))

(defn ^:private linear [dx dy rx ry]
  (fn [n] (+ (* (/ (- n dx) (- dy dx)) (- ry rx)) rx)))

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

(def ^:private handler
  {["keydown" "Shift"]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :local/modifier-start :shift)))

   ["keyup" "Shift"]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :local/modifier-release)))

   ["keydown" "Escape"]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :shortcut/escape)))

   ["keydown" "Delete"]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :selection/remove)))

   ["keydown" "Backspace"]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :selection/remove)))

   ["keydown" \s]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :camera/change-mode :select)))

   ["keydown" \r]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :camera/change-mode :ruler)))

   ["keydown" \1]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :camera/change-mode :circle)))

   ["keydown" \2]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :camera/change-mode :rect)))

   ["keydown" \3]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :camera/change-mode :cone)))

   ["keydown" \4]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :camera/change-mode :poly)))

   ["keydown" \5]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :camera/change-mode :line)))

   ["keydown" \g]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :camera/change-mode :grid)))

   ["keydown" \f]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :camera/change-mode :mask)))

   ["keydown" \t]
   (fn [[_ dispatch] event]
     (if (not (.-metaKey event))
       (dispatch :camera/change-mode :mask-toggle)))

   ["keydown" \x]
   (fn [[_ dispatch] event]
     (if (.-metaKey event)
       (dispatch :clipboard/copy true)
       (dispatch :camera/change-mode :mask-remove)))

   ["keydown" \c]
   (fn [[_ dispatch] event]
     (if (.-metaKey event)
       (dispatch :clipboard/copy false)))

   ["keydown" \v]
   (fn [[_ dispatch] event]
     (if (.-metaKey event)
       (dispatch :clipboard/paste)))

   ["wheel"]
   (fn [[conn dispatch] event]
     (if (.. event -target (closest "svg.scene"))
       (let [select [[:bounds/self :default [0 0 0 0]]]
             result (pull @conn select [:db/ident :local])
             {[ox oy _ _] :bounds/self} result
             cx (- (.-clientX event) ox)
             cy (- (.-clientY event) oy)
             dy (.-deltaY event)
             dt (linear -400 400 -0.50 0.50)]
         (if (.-ctrlKey event)
           (do (.preventDefault event)
               (dispatch :camera/zoom-delta (dt (* -1 8 dy)) cx cy))
           (dispatch :camera/zoom-delta (dt (* -1 2 dy)) cx cy)))))})

(defn ^:private event-key [type event]
  (case type
    "keydown" [type (.-key event)]
    "keyup"   [type (.-key event)]
    "wheel"   [type]))

(defn ^:private allow-event? [event]
  (let [target (.-target event)]
    (not (or (.-repeat event)
             (and (not= (.-type event) "wheel") (.-ctrlKey event))
             (and (not= (.-key event) "Shift") (.-shiftKey event))
             (and (instance? js/HTMLInputElement target)
                  (or (= (.-type target) "text")
                      (= (.-type target) "number")))))))

(defui handlers []
  (let [dispatch (use-dispatch)
        conn     (use-context context)]
    (use-event-listener "keyup"
      (use-callback
       (fn [event]
         (if (allow-event? event)
           (if-let [f (handler (event-key "keyup" event))]
             (f [conn dispatch] event)))) [conn dispatch]))
    (use-event-listener "keydown"
      (use-callback
       (fn [event]
         (if (allow-event? event)
           (if-let [f (handler (event-key "keydown" event))]
             (f [conn dispatch] event)))) [conn dispatch]))
    (use-event-listener "wheel"
      (use-callback
       (fn [event]
         (if (allow-event? event)
           (if-let [f (handler (event-key "wheel" event))]
             (f [conn dispatch] event)))) [conn dispatch]))))
