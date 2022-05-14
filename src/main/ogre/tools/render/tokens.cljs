(ns ogre.tools.render.tokens
  (:require [ogre.tools.state :refer [use-query]]
            [react-draggable]))

(defn round [x n]
  (* (js/Math.round (/ x n)) n))

(def query
  [:bounds/self
   {:local/window
    [[:window/scale :default 1]
     [:window/snap-grid :default false]
     [:window/vec :default [0 0]]
     {:window/canvas
      [[:grid/size :default 70]]}]}])

(defn tokens []
  (let [[result dispatch] (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :window/vec
          scale   :window/scale
          align   :window/snap-grid
          {size :grid/size} :window/canvas} :local/window} result]
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
           (dispatch :token/create x y)))}
      [:svg [:circle {:cx 32 :cy 32 :r 33 :fill "url(#token-stamp-default)"}]]]]))
