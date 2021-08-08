(ns ogre.tools.render.tokens
  (:require [uix.core.alpha :as uix]
            [react-draggable :as draggable]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [context]]
            [ogre.tools.query :as query]))

(defclass styles []
  {:display "flex"
   :pointer-events "all"
   :flex-direction "column"
   :margin "24px 16px"}
  [:.token
   {:position "relative" :margin-bottom "8px"}
   [:svg {:width "72px" :height "72px"}]
   [:svg.draggable
    {:position "absolute"}]])

(defn tokens [props]
  (let [{:keys [data workspace dispatch]} (uix/context context)
        {:keys [zoom/scale]} workspace]
    [:div {:class (styles)}
     (for [token (query/templates data) :let [{:keys [db/id]} token]]
       [:div.token {:key id}
        [:> draggable
         {:position #js {:x 0 :y 0}
          :onStop
          (fn [event data]
            (let [parent (.getBoundingClientRect (.querySelector js/document ".canvas"))
                  node   (.getBoundingClientRect (.-node data))]
              (dispatch
               :token/create id
               (- (+ (.-x node) (/ (.-width node) 2)) (.-x parent))
               (- (+ (.-y node) (/ (.-height node) 2)) (.-y parent)))))}
         [:svg.draggable [:circle {:cx 36 :cy 36 :r 36 :fill "black"}]]]
        [:svg.copy [:circle {:cx 36 :cy 36 :r 36 :fill "black"}]]])]))
