(ns ogres.app.provider.window
  (:require [goog.functions :refer [throttle]]
            [ogres.app.hooks :as hooks]
            [ogres.app.vec :as vec]
            [uix.core :as uix :refer [defui $]]))

(defui ^:private bounds
  "Registers event handlers to watch for changes in the scene dimensions in
   order to put those dimensions in the application state."
  [{:keys [target]}]
  (let [dispatch (hooks/use-dispatch)
        handler  (uix/use-memo
                  (fn []
                    (throttle
                     (fn []
                       (if-let [element (.. target -document (querySelector ".layout-scene"))]
                         (let [rect (vec/DOMRect->Segment (.getBoundingClientRect element))]
                           (dispatch :bounds/change rect)))) 100)) [dispatch target])
        [observer] (uix/use-state (js/ResizeObserver. handler))
        [scene set-scene] (uix/use-state nil)]
    (hooks/use-event-listener "resize" handler)
    (uix/use-effect
     (fn []
       ((fn f []
          (let [element (.querySelector (.-document target) ".layout-scene")]
            (if element
              (set-scene element)
              (.requestAnimationFrame js/window f)))))))
    (uix/use-effect
     (fn []
       (when scene
         (.observe observer scene)
         (fn [] (.unobserve observer scene)))) ^:lint/disable [scene])))

(defui listeners
  "Provides a reference to the view window, if any, and registers several
   event handlers needed for them."
  []
  (let [{:keys [user/ready]} (hooks/use-query [:user/ready])]
    (if ready
      ($ bounds {:target js/window}))))
