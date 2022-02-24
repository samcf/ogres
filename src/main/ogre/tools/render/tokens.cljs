(ns ogre.tools.render.tokens
  (:require [ogre.tools.state :refer [use-query]]
            [ogre.tools.vec :as vec]
            [react-draggable]))

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
        {offset :bounds/self
         {trans :pos/vec
          scale :zoom/scale
          align :grid/align
          size :grid/size} :root/canvas} result]
    [:<>
     [:svg [:circle {:cx 32 :cy 32 :r 33 :fill "url(#token-stamp-default)"}]]
     [:> react-draggable
      {:position #js {:x 0 :y 0}
       :onStop
       (fn [event data]
         (let [el (.getBoundingClientRect (.-node data))
               dim (vec/s (/ 2) [(.-width el) (.-height el)])
               dst [(.-x el) (.-y el)]
               dst (vec/+ dst dim)
               dst (vec/- dst offset)
               dst (vec/s (/ scale) dst)
               dst (vec/- dst trans)
               dst (if (not= align (.-metaKey event))
                     (vec/r (/ size 2) dst) dst)]
           (dispatch :token/create dst)))}
      [:svg [:circle {:cx 32 :cy 32 :r 33 :fill "url(#token-stamp-default)"}]]]]))
