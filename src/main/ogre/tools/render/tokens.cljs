(ns ogre.tools.render.tokens
  (:require [ogre.tools.state :refer [use-query]]
            [react-draggable :as draggable]))

(def attrs
  [:bounds/self
   {:root/tokens [:db/id]}
   {:root/canvas [:zoom/scale :pos/vec]}])

(defn tokens [props]
  (let [[result dispatch] (use-query {:pull attrs})
        {tokens :root/tokens
         [px py _ _] :bounds/self
         {scale :zoom/scale [tx ty] :pos/vec} :root/canvas} result]
    [:div.tokens
     (for [{:keys [db/id]} tokens]
       [:div.tokens-token {:key id}
        [:> draggable
         {:position #js {:x 0 :y 0}
          :onStop
          (fn [event data]
            (let [node (.getBoundingClientRect (.-node data))
                  [cx cy cw ch]
                  [(.-x node)
                   (.-y node)
                   (.-width node)
                   (.-height node)]]
              (dispatch
               :token/create id
               [(- (/ (- (+ cx (/ cw 2)) px) scale) tx)
                (- (/ (- (+ cy (/ ch 2)) py) scale) ty)])))}
         [:svg.draggable [:circle {:cx 36 :cy 36 :r 36 :fill "black"}]]]
        [:svg.copy [:circle {:cx 36 :cy 36 :r 36 :fill "black"}]]])]))
