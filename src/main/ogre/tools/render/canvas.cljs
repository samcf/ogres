(ns ogre.tools.render.canvas
  (:require [clojure.string :as string]
            [uix.core.alpha :as uix]
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

(defn grid [props]
  (let [{:keys [workspace]} (uix/context context)
        {:keys [grid/size grid/origin]} workspace
        [ox oy] origin]
    (when (> size 0)
      [:<>
       [:defs
        [:pattern {:id "grid" :width size :height size :patternUnits "userSpaceOnUse"}
         [:path
          {:d (string/join " " ["M" size 0 "L" 0 0 0 size])
           :stroke "white"
           :stroke-width "1"
           :stroke-dasharray "2px"
           :fill "none"}]]]
       [:g
        [:circle {:cx ox :cy oy :r 12 :stroke "gold" :fill "transparent"}]
        [:rect {:x ox :y oy :width (str (* size 10) "px") :height (str (* size 10) "px") :fill "url(#grid)"}]]])))

(defn grid-draw [props]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [workspace/mode]}     workspace
        canvas (.getBoundingClientRect (js/document.querySelector ".canvas"))
        points (uix/state nil)]
    [:g
     [:> draggable
      {:position #js {:x 0 :y 0}
       :disabled (not (= mode :grid))
       :on-start
       (handler
        (fn [event data]
          (let [mx (- (.-clientX event) (.-x canvas))
                my (- (.-clientY event) (.-y canvas))]
            (reset! points [mx my mx my]))))

       :on-drag
       (handler
        (fn [event data]
          (swap!
           points
           (fn [[ax ay bx by]]
             (let [d (.-x data)]
               [ax ay (+ ax d) (+ ay d)])))))

       :on-stop
       (handler
        (fn [event data]
          (let [[ax ay bx by] @points]
            (reset! points nil)
            (let [size (js/Math.abs (- bx ax))]
              (when (> size 0)
                (dispatch :grid/draw ax ay size))))))}
      [:g [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]]]

     (when (seq @points)
       (let [[ax ay bx by] @points]
         [:path {:d (string/join " " ["M" ax ay "H" bx "V" by "H" ax "Z"]) :stroke "red" :fill "transparent"}]))]))

(defn canvas [props]
  (let [{:keys [workspace dispatch]}    (uix/context context)
        {:keys [position/x position/y grid/size]} workspace]
    [:svg {:class (styles)}
     [:> draggable
      {:position #js {:x 0 :y 0}
       :disabled (= (:workspace/mode workspace) :grid)
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
        [grid]
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
               [:g {:class (css (element-styles) "token" {:active (= element (:workspace/selected workspace))})}
                [:circle {:cx 0 :cy 0 :r (- (/ size 2) 4) :fill "black"}]
                (when-let [name (:element/name element)]
                  [:text {:x 0 :y 54 :text-anchor "middle" :fill "white"} name])]])
            nil))]
       (when (= (:workspace/mode workspace) :grid)
         [grid-draw])]]]))
