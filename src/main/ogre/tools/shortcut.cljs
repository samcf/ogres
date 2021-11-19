(ns ogre.tools.shortcut
  (:require [datascript.core :refer [pull]]
            [ogre.tools.state :refer [state]]
            [ogre.tools.render :refer [listen!]]
            [uix.core.alpha :as uix]))

(defn linear [dx dy rx ry]
  (fn [n] (+ (* (/ (- n dx) (- dy dx)) (- ry rx)) rx)))

(def events
  {["keydown" "Shift"]
   (fn [[_ dispatch]]
     (dispatch :canvas/modifier-start :shift))

   ["keyup" "Shift"]
   (fn [[_ dispatch]]
     (dispatch :canvas/modifier-release))

   ["keydown" "Escape"]
   (fn [[_ dispatch]]
     (dispatch :selection/clear))

   ["keydown" "Delete"]
   (fn [[_ dispatch]]
     (dispatch :selection/remove))

   ["keydown" "Backspace"]
   (fn [[_ dispatch]]
     (dispatch :selection/remove))

   ["keydown" \s]
   (fn [[_ dispatch]]
     (dispatch :canvas/toggle-mode :select))

   ["keydown" \g]
   (fn [[_ dispatch]]
     (dispatch :canvas/toggle-mode :grid))

   ["keydown" \r]
   (fn [[_ dispatch]]
     (dispatch :canvas/toggle-mode :ruler))

   ["keydown" \1]
   (fn [[_ dispatch]]
     (dispatch :canvas/toggle-mode :circle))

   ["keydown" \2]
   (fn [[_ dispatch]]
     (dispatch :canvas/toggle-mode :rect))

   ["keydown" \3]
   (fn [[_ dispatch]]
     (dispatch :canvas/toggle-mode :cone))

   ["keydown" \4]
   (fn [[_ dispatch]]
     (dispatch :canvas/toggle-mode :line))

   ["keydown" \v]
   (fn [[_ dispatch]]
     (dispatch :share/initiate))

   ["keydown" \p]
   (fn [[conn dispatch]]
     (let [{:keys [share/open?]} (pull @conn [:share/open?] [:db/ident :root])]
       (if open?
         (dispatch :share/switch))))
   
   ["keydown" \w]
   (fn [[_ dispatch]]
     (dispatch :share/initiate))

   ["keydown" \ ]
   (fn [[_ dispatch]]
     (dispatch :interface/toggle-panel))

   ["wheel"]
   (fn [[conn dispatch] event]
     (if (.. event -target (closest "svg.canvas"))
       (let [{[ox oy _ _] :bounds/self} (pull @conn [:bounds/self] [:db/ident :root])
             cx (- (.-clientX event) ox)
             cy (- (.-clientY event) oy)
             dy (.-deltaY event)
             dt (linear -400 400 -0.50 0.50)]
         (if (.-ctrlKey event)
           (do (.preventDefault event)
               (dispatch :zoom/change (dt (* 8 dy)) cx cy))
           (dispatch :zoom/change (* -1 (dt (* 2 dy))) cx cy)))))})

(defn event-key [type event]
  (case type
    "keydown" [type (.-key event)]
    "keyup"   [type (.-key event)]
    "wheel"   [type]))

(defn allow-event? [event]
  (let [target (.-target event)]
    (not (or (.-repeat event)
             (.-metaKey event)
             (and (not= (.-type event) "wheel") (.-ctrlKey event))
             (and (not= (.-key event) "Shift") (.-shiftKey event))
             (and (instance? js/HTMLInputElement target)
                  (or (= (.-type target) "text")
                      (= (.-type target) "number")))))))

(defn handlers []
  (let [context (uix/context state)]
    (doseq [type ["keydown" "keyup" "wheel"]]
      (listen!
       (fn [event]
         (if (allow-event? event)
           (if-let [f (events (event-key type event))]
             (f context event)))) type []))))
