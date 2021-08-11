(ns ogre.tools.render.tokens
  (:require [uix.core.alpha :as uix]
            [react-draggable :as draggable]
            [ogre.tools.render :refer [context]]
            [ogre.tools.query :as query]))

(defn tokens [props]
  (let [{:keys [data workspace dispatch]} (uix/context context)
        {:keys [zoom/scale]} workspace]
    [:div.tokens
     (for [token (query/templates data) :let [{:keys [db/id]} token]]
       [:div.tokens-token {:key id}
        [:> draggable
         {:position #js {:x 0 :y 0}
          :onStop
          (fn [event data]
            (let [parent (.getBoundingClientRect (.querySelector js/document ".layout-canvas"))
                  node   (.getBoundingClientRect (.-node data))]
              (dispatch
               :token/create id
               (- (+ (.-x node) (/ (.-width node) 2)) (.-x parent))
               (- (+ (.-y node) (/ (.-height node) 2)) (.-y parent)))))}
         [:svg.draggable [:circle {:cx 36 :cy 36 :r 36 :fill "black"}]]]
        [:svg.copy [:circle {:cx 36 :cy 36 :r 36 :fill "black"}]]])]))
