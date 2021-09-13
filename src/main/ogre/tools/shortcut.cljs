(ns ogre.tools.shortcut
  (:require [ogre.tools.query :as query]
            [ogre.tools.state :refer [state]]
            [ogre.tools.window :refer [listen!]]
            [uix.core.alpha :as uix]))

(def events
  {["keydown" "Shift"]
   (fn [{:keys [workspace dispatch]}]
     (dispatch :canvas/modifier-start :shift))

   ["keyup" "Shift"]
   (fn [{:keys [dispatch]}]
     (dispatch :canvas/modifier-release))

   ["keydown" "Escape"]
   (fn [{:keys [dispatch]}]
     (dispatch :selection/clear))

   ["keydown" "Delete"]
   (fn [{:keys [dispatch]}]
     (dispatch :selection/remove))

   ["keydown" \s]
   (fn [{:keys [dispatch]}]
     (dispatch :canvas/toggle-mode :select))

   ["keydown" \w]
   (fn [{:keys [dispatch]}]
     (dispatch :canvas/toggle-mode :canvas))

   ["keydown" \g]
   (fn [{:keys [dispatch]}]
     (dispatch :canvas/toggle-mode :grid))

   ["keydown" \r]
   (fn [{:keys [dispatch]}]
     (dispatch :canvas/toggle-mode :ruler))

   ["keydown" \1]
   (fn [{:keys [dispatch]}]
     (dispatch :canvas/toggle-mode :circle))

   ["keydown" \2]
   (fn [{:keys [dispatch]}]
     (dispatch :canvas/toggle-mode :rect))

   ["keydown" \3]
   (fn [{:keys [dispatch]}]
     (dispatch :canvas/toggle-mode :cone))

   ["keydown" \4]
   (fn [{:keys [dispatch]}]
     (dispatch :canvas/toggle-mode :line))

   ["keydown" \v]
   (fn [{:keys [dispatch]}]
     (dispatch :share/initiate))

   ["keydown" \p]
   (fn [{:keys [dispatch data]}]
     (when (:share/open? (query/viewer data))
       (dispatch :share/switch)))

   ["wheel"]
   (fn [{:keys [data dispatch]} event]
     (when (.. event -target (closest "svg.canvas"))
       (let [{[ox oy _ _] :bounds/self} (query/viewer data)
             [mx my] [(.-clientX event) (.-clientY event)]
             [x y] [(- mx ox) (- my oy)]]
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
