(ns ogre.tools.shortcut
  (:require [datascript.core :refer [pull]]
            [ogre.tools.state :refer [state]]
            [ogre.tools.window :refer [listen!]]
            [uix.core.alpha :as uix]))

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
             mx (.-clientX event)
             my (.-clientY event)
             x (- mx ox)
             y (- my oy)]
         (if (pos? (.-deltaY event))
           (dispatch :zoom/out x y)
           (dispatch :zoom/in x y)))))})

(defn event-key [type event]
  (case type
    "keydown" [type (.-key event)]
    "keyup"   [type (.-key event)]
    "wheel"   [type]))

(defn allow-event? [event]
  (let [target (.-target event)]
    (not (or (.-repeat event)
             (.-metaKey event)
             (.-ctrlKey event)
             (and (not= (.-key event) "Shift") (.-shiftKey event))
             (and (instance? js/HTMLInputElement target)
                  (or (= (.-type target) "text")
                      (= (.-type target) "number")))))))

(defn handlers []
  (let [context (uix/context state)]
    (doseq [type ["keydown" "keyup" "wheel"]]
      (listen!
       (fn [event]
         (when (allow-event? event)
           (when-let [f (events (event-key type event))]
             (f context event)))) type [context]))))
