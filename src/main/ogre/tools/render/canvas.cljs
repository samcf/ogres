(ns ogre.tools.render.canvas
  (:require [rum.core :as rum]
            [react-draggable :as draggable]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [context css]]))

(defclass element-styles []
  {:pointer-events "all"}
  [:&.token>circle
   {:filter "drop-shadow(1px 1px 2px rgba(0, 0, 0, .6))"}])

(defn handler
  ([] (fn [event]
        (.preventDefault event)
        (.stopPropagation event)))
  ([f] (fn [event & args]
         (.preventDefault event)
         (.stopPropagation event)
         (apply f event args))))

(rum/defc canvas [props]
  (rum/with-context [{:keys [dispatch]} context]
    (let [{:keys [workspace]} props
          {:keys [position/x position/y]} workspace]
      [:> draggable
       {:position #js {:x 0 :y 0}
        :onStop
        (fn [event data]
          (let [ox (.-x data) oy (.-y data)]
            (dispatch :camera/translate (:db/id workspace) (+ ox x) (+ oy y))))}
       [:g
        [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]
        [:g {:transform (str "translate(" x ", " y ")")}
         (when-let [url (-> workspace :workspace/map :map/url)]
           [:image {:x 0 :y 0 :href url}])
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
                              (dispatch :token/translate (:db/id element) (.-x data) (.-y data))))}
                [:g {:class (css (element-styles) "token")}
                 [:circle {:cx 0 :cy 0 :r 36 :fill "purple"}]]])
             nil))]]])))
