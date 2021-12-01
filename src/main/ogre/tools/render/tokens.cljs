(ns ogre.tools.render.tokens
  (:require [ogre.tools.state :refer [use-query]]
            [react-draggable :as draggable]))

(def attrs
  [:bounds/self
   {:root/canvas
    [[:zoom/scale :default 1]
     [:pos/vec :default [0 0]]]}])

(defn tokens [props]
  (let [[result dispatch] (use-query {:pull attrs})
        {[bx by _ _] :bounds/self
         {s :zoom/scale [tx ty] :pos/vec} :root/canvas} result]
    [:<>
     [:svg [:circle {:cx 32 :cy 32 :r 33 :fill "url(#token-stamp-default)"}]]
     [:> draggable
      {:position #js {:x 0 :y 0}
       :onStop
       (fn [_ data]
         (let [el (.getBoundingClientRect (.-node data))
               nx (.-x el)
               ny (.-y el)
               nw (/ (.-width el) 2)
               nh (/ (.-height el) 2)
               dx (-> nx (+ nw) (- bx) (/ s) (- tx))
               dy (-> ny (+ nh) (- by) (/ s) (- ty))]
           (dispatch :token/create [dx dy])))}
      [:svg [:circle {:cx 32 :cy 32 :r 33 :fill "url(#token-stamp-default)"}]]]]))
