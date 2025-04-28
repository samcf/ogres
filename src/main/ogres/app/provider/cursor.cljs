(ns ogres.app.provider.cursor
  (:require [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui]]
            [ogres.app.vec :as vec :refer [Vec2]]))

(def ^:private handler-query
  [[:user/bounds :default vec/zero-segment]
   {:user/camera
    [[:camera/scale :default 1]
     [:camera/point :default vec/zero]]}])

(defui listeners []
  (let [publish (hooks/use-publish)
        result  (hooks/use-query handler-query)
        {bounds :user/bounds
         {point :camera/point
          scale :camera/scale} :user/camera} result]
    (hooks/use-event-listener js/window "pointermove"
      (uix/use-callback
       (fn [event]
         (if-let [element (.. event -target (closest "#scene-drag"))]
           (let [data (.-dataset element)]
             (if (and (some? data) (= (.-dragging data) "false"))
               (let [dx (.-clientX event)
                     dy (.-clientY event)
                     mv (vec/add (vec/div (vec/sub (Vec2. dx dy) (.-a bounds)) scale) point)]
                 (publish :cursor/move (.-x mv) (.-y mv)))))))
       [publish point bounds scale]))))
