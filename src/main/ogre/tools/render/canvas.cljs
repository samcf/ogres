(ns ogre.tools.render.canvas
  (:require [uix.core.alpha :as uix]
            [react-draggable :as draggable]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [context css handler use-image]]))

(defn distance [[ax ay] [bx by]]
  (js/Math.sqrt
   (+ (js/Math.pow (- bx ax) 2)
      (js/Math.pow (- by ay) 2))))

(defclass styles []
  {:pointer-events "all" :height "100%" :width "100%"})

(defclass element-styles []
  {:pointer-events "all"}
  [:&.token>circle
   {:filter "drop-shadow(1px 1px 2px rgba(0, 0, 0, .6))"}]
  [:&.token.active>circle
   {:stroke "#ffeb3b"
    :stroke-width "1.5px"
    :stroke-dasharray "3px"
    :stroke-linecap "round"}])

(defn board [{:keys [image]}]
  (let [url (use-image (:image/checksum image))]
    (when (string? url)
      [:image {:x 0 :y 0 :href url}])))

(defn canvas [props]
  (let [{:keys [workspace dispatch]}    (uix/context context)
        {:keys [position/x position/y]} workspace]
    [:svg {:class (styles)}
     [:> draggable
      {:position #js {:x 0 :y 0}
       :onStart
       (fn []
         (dispatch :view/clear))

       :onStop
       (fn [event data]
         (let [ox (.-x data) oy (.-y data)]
           (dispatch :camera/translate (:db/id workspace) (+ ox x) (+ oy y))))}
      [:g
       [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]
       [:g {:transform (str "translate(" x ", " y ")")}
        (let [{:keys [workspace/map]} workspace]
          [board {:key (:image/checksum map) :image map}])
        (for [element (:workspace/elements workspace)]
          (case (:element/type element)
            :token
            (let [{:keys [position/x position/y]} element]
              [:> draggable
               {:key      (:db/id element)
                :position #js {:x x :y y}
                :onStart  (handler)
                :onStop   (handler
                           (fn [event data]
                             (let [dist (distance [x y] [(.-x data) (.-y data)])]
                               (if (= dist 0)
                                 (dispatch :view/toggle (:db/id element))
                                 (dispatch :token/translate (:db/id element) (.-x data) (.-y data))))))}
               [:g {:class (css (element-styles) "token" {:active (= element (:workspace/viewing workspace))})}
                [:circle {:cx 0 :cy 0 :r 36 :fill "black"}]
                (when-let [name (:element/name element)]
                  [:text {:x 0 :y 54 :text-anchor "middle" :fill "white"} name])]])
            nil))]]]]))
