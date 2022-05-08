(ns ogre.tools.render.tokens
  (:require [datascript.core :refer [squuid]]
            [ogre.tools.state :refer [use-query]]
            [react-draggable]))

(defn round [x n]
  (* (js/Math.round (/ x n)) n))

(def attrs
  {:pull
   [:bounds/self
    {:root/canvas
     [[:zoom/scale :default 1]
      [:grid/align :default false]
      [:grid/size :default 70]
      [:pos/vec :default [0 0]]]}]})

(defn tokens []
  (let [[result dispatch] (use-query attrs)
        {[ox oy] :bounds/self
         {[tx ty] :pos/vec
          scale :zoom/scale
          align :grid/align
          size :grid/size} :root/canvas} result]
    [:<>
     [:svg [:circle {:cx 32 :cy 32 :r 33 :fill "url(#token-stamp-default)"}]]
     [:> react-draggable
      {:position #js {:x 0 :y 0}
       :onStop
       (fn [event data]
         (let [r (.getBoundingClientRect (.-node data))
               w (.-width r)
               h (.-height r)
               x (-> (.-x r) (+ (* (/ 2) w)) (- ox) (* (/ scale)) (- tx))
               y (-> (.-y r) (+ (* (/ 2) h)) (- oy) (* (/ scale)) (- ty))
               x (if (not= align (.-metaKey event)) (round x (/ size 2)) x)
               y (if (not= align (.-metaKey event)) (round y (/ size 2)) y)]
           (dispatch :token/create (squuid) x y)))}
      [:svg [:circle {:cx 32 :cy 32 :r 33 :fill "url(#token-stamp-default)"}]]]]))
