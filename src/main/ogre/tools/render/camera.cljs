(ns ogre.tools.render.camera
  (:require [rum.core :as rum]
            [react-draggable :as draggable]))

(rum/defc camera [props children]
  (let [{:keys [element dispatch]} props
        {:keys [position/x position/y]} element]
    [:> draggable
     {:position #js {:x 0 :y 0}
      :onStop (fn [event data]
                (let [ox (.-x data) oy (.-y data)]
                  (dispatch :camera/translate (:db/id element) (+ ox x) (+ oy y))))}
     [:g
      [:g {:transform (str "translate(" x ", " y ")")} children]
      [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]]]))
