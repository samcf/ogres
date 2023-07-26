(ns ogres.app.shortcut
  (:require [datascript.core :refer [pull]]
            [ogres.app.hooks :refer [use-event-listener use-dispatch]]
            [ogres.app.provider.state :refer [context]]
            [uix.core :refer [defui use-callback use-context]]))

(defn linear [dx dy rx ry]
  (fn [n] (+ (* (/ (- n dx) (- dy dx)) (- ry rx)) rx)))

(def shortcuts
  {["keydown" "Shift"]
   (fn [[_ dispatch]]
     (dispatch :window/modifier-start :shift))

   ["keyup" "Shift"]
   (fn [[_ dispatch]]
     (dispatch :window/modifier-release))

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
     (dispatch :window/change-mode :select))

   ["keydown" \r]
   (fn [[_ dispatch]]
     (dispatch :window/change-mode :ruler))

   ["keydown" \1]
   (fn [[_ dispatch]]
     (dispatch :window/change-mode :circle))

   ["keydown" \2]
   (fn [[_ dispatch]]
     (dispatch :window/change-mode :rect))

   ["keydown" \3]
   (fn [[_ dispatch]]
     (dispatch :window/change-mode :cone))

   ["keydown" \4]
   (fn [[_ dispatch]]
     (dispatch :window/change-mode :poly))

   ["keydown" \5]
   (fn [[_ dispatch]]
     (dispatch :window/change-mode :line))

   ["keydown" \f]
   (fn [[_ dispatch]]
     (dispatch :window/change-mode :mask))

   ["keydown" \t]
   (fn [[_ dispatch]]
     (dispatch :window/change-mode :mask-toggle))

   ["keydown" \x]
   (fn [[_ dispatch]]
     (dispatch :window/change-mode :mask-remove))

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
               (dispatch :zoom/delta (dt (* -1 8 dy)) cx cy))
           (dispatch :zoom/delta (dt (* -1 2 dy)) cx cy)))))})

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

(defui handlers []
  (let [dispatch (use-dispatch)
        conn     (use-context context)]
    (use-event-listener "keyup"
      (use-callback
       (fn [event]
         (if (allow-event? event)
           (if-let [f (shortcuts (event-key "keyup" event))]
             (f [conn dispatch] event)))) [conn dispatch]))
    (use-event-listener "keydown"
      (use-callback
       (fn [event]
         (if (allow-event? event)
           (if-let [f (shortcuts (event-key "keydown" event))]
             (f [conn dispatch] event)))) [conn dispatch]))
    (use-event-listener "wheel"
      (use-callback
       (fn [event]
         (if (allow-event? event)
           (if-let [f (shortcuts (event-key "wheel" event))]
             (f [conn dispatch] event)))) [conn dispatch]))))
