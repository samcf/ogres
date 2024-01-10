(ns ogres.app.provider.cursor
  (:require [ogres.app.hooks :refer [use-publish use-query use-event-listener]]
            [uix.core :as uix :refer [use-callback defui]]))

(def ^:private handler-query
  [[:bounds/self :default [0 0 0 0]]
   {:local/camera
    [[:camera/scale :default 1]
     [:camera/point :default [0 0]]]}])

(defui handlers []
  (let [publish (use-publish)
        result  (use-query handler-query)
        {[sx sy] :bounds/self
         {[cx cy] :camera/point
          scale :camera/scale} :local/camera} result]
    (use-event-listener js/window "pointermove"
      (use-callback
       (fn [event]
         (if-let [element (.. event -target (closest "#scene-drag"))]
           (let [data (.-dataset element)]
             (if (and (some? data) (= (.-dragging data) "false"))
               (let [dx (.-clientX event)
                     dy (.-clientY event)
                     mx (int (+ (/ (- dx sx) scale) cx))
                     my (int (+ (/ (- dy sy) scale) cy))]
                 (publish {:topic :cursor/move :args [mx my]}))))))
       [publish cx cy sx sy scale]))))
