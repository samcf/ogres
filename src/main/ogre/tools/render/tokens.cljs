(ns ogre.tools.render.tokens
  (:require [react-draggable :as draggable]
            [rum.core :as rum]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [context]]
            [ogre.tools.query :as query]))

(defclass styles []
  {:display        "flex"
   :pointer-events "all"
   :flex-direction "column"
   :margin         "24px 16px"}
  [:.token
   {:position      "relative"
    :margin-bottom "8px"}
   [:svg {:width  "72px"
          :height "72px"}]
   [:svg.draggable
    {:position "absolute"}]])

(rum/defc tokens [{:keys [workspace]}]
  (rum/with-context [{:keys [data dispatch]} context]
    [:div {:class (styles)}
     (for [token (query/tokens data) :let [{:keys [db/id]} token]]
       [:div.token {:key id}
        [:> draggable
         {:position #js {:x 0 :y 0}
          :onStop
          (fn [event data]
            (let [parent (.getBoundingClientRect (.querySelector js/document ".canvas"))
                  node   (.getBoundingClientRect (.-node data))]
              (dispatch :token/create
                        (:db/id workspace)
                        {:element/type :token}
                        (- (+ (.-x node) (/ (.-width node) 2)) (.-x parent))
                        (- (+ (.-y node) (/ (.-height node) 2)) (.-y parent)))))}
         [:svg.draggable [:circle {:cx 36 :cy 36 :r 36 :fill "black"}]]]
        [:svg.copy [:circle {:cx 36 :cy 36 :r 36 :fill "black"}]]])]))
