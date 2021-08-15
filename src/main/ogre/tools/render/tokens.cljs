(ns ogre.tools.render.tokens
  (:require [uix.core.alpha :as uix]
            [react-draggable :as draggable]
            [ogre.tools.render :refer [context]]
            [ogre.tools.query :as query]))

(defn tokens [props]
  (let [{:keys [data workspace dispatch]} (uix/context context)
        {scale :zoom/scale [tx ty] :pos/vec} workspace]
    [:div.tokens
     (for [token (query/templates data) :let [{:keys [db/id]} token]]
       [:div.tokens-token {:key id}
        [:> draggable
         {:position #js {:x 0 :y 0}
          :onStop
          (fn [event data]
            (let [parent        (.getBoundingClientRect (.querySelector js/document ".layout-canvas"))
                  node          (.getBoundingClientRect (.-node data))
                  [px py]       [(.-x parent) (.-y parent)]
                  [cx cy cw ch] [(.-x node) (.-y node) (.-width node) (.-height node)]]
              (dispatch
               :token/create id
               [(- (/ (- (+ cx (/ cw 2)) px) scale) tx)
                (- (/ (- (+ cy (/ ch 2)) py) scale) ty)])))}
         [:svg.draggable [:circle {:cx 36 :cy 36 :r 36 :fill "black"}]]]
        [:svg.copy [:circle {:cx 36 :cy 36 :r 36 :fill "black"}]]])]))
