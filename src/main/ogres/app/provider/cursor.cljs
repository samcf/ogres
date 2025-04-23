(ns ogres.app.provider.cursor
  (:require [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui]]
            [ogres.app.vec :as vec]))

(def ^:private handler-query
  [[:bounds/self :default vec/zero-segment]
   {:user/camera
    [[:camera/scale :default 1]
     [:camera/point :default [0 0]]]}])

(defui listeners []
  (let [publish (hooks/use-publish)
        result  (hooks/use-query handler-query)
        {bounds :bounds/self
         {[cx cy] :camera/point
          scale :camera/scale} :user/camera} result]
    (hooks/use-event-listener js/window "pointermove"
      (uix/use-callback
       (fn [event]
         (if-let [element (.. event -target (closest "#scene-drag"))]
           (let [data (.-dataset element)]
             (if (and (some? data) (= (.-dragging data) "false"))
               (let [dx (.-clientX event)
                     dy (.-clientY event)
                     mx (int (+ (/ (- dx (.-x (.-a bounds))) scale) cx))
                     my (int (+ (/ (- dy (.-y (.-a bounds))) scale) cy))]
                 (publish :cursor/move mx my))))))
       [publish cx cy bounds scale]))))
