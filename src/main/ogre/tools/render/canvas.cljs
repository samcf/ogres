(ns ogre.tools.render.canvas
  (:require [clojure.string :as string]
            [uix.core.alpha :as uix]
            [react-draggable :as draggable]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [context css handler use-image]]
            [ogre.tools.query :as query]))

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
  (let [{{:keys [lighting/level]} :workspace} (uix/context context)
        url (use-image (:image/checksum image))
        attrs {:bright {:clip-path (when-not (= level :bright) "url(#clip-bright-light)")}
               :dim    {:clip-path (when (= level :dark) "url(#clip-dim-light)")
                        :style     {:filter "saturate(20%) brightness(50%)"}}}]
    (when (string? url)
      [:<>
       (for [type [:dim :bright]]
         [:image (merge {:x 0 :y 0 :href url} (attrs type) {:key type})])])))

(defn grid [{:keys [canvas]}]
  (let [{:keys [workspace]}    (uix/context context)
        {camera-x :position/x} workspace
        {camera-y :position/y} workspace
        {grid-size :grid/size} workspace
        {[ox oy] :grid/origin} workspace
        dimensions (uix/state [0 0])
        [w h]      @dimensions]

    (uix/effect!
     (fn []
       (when @canvas
         (let [bounding (.getBoundingClientRect @canvas)]
           (reset! dimensions [(.-width bounding) (.-height bounding)]))))
     [(nil? @canvas)])

    (let [[sx sy ax ay bx] [(- (* w -2) camera-x) (- (* h -2) camera-y)
                            (- (* w  2) camera-x) (- (* h  2) camera-y) (- (* w -2) camera-x)]]
      [:<>
       [:defs
        [:pattern {:id "grid" :width grid-size :height grid-size :patternUnits "userSpaceOnUse"}
         [:path
          {:d (string/join " " ["M" 0 0 "H" grid-size "V" grid-size])
           :stroke "rgba(255, 255, 255, 0.40)"
           :stroke-width "1"
           :stroke-dasharray "2px"
           :fill "none"}]]]
       [:circle {:cx ox :cy oy :r 12 :stroke "gold" :fill "transparent"}]
       [:path {:d (string/join " " ["M" sx sy "H" ax "V" ay "H" bx "Z"]) :fill "url(#grid)"}]])))

(defn grid-draw [{:keys [canvas]}]
  (let [{:keys [workspace dispatch]}        (uix/context context)
        {:keys [workspace/mode zoom/scale]} workspace
        canvas (.getBoundingClientRect      @canvas)
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
                (dispatch :grid/draw ax ay (/ size scale)))))))}
      [:g [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]]]

     (when (seq @points)
       (let [[ax ay bx by] @points]
         [:path {:d (string/join " " ["M" ax ay "H" bx "V" by "H" ax "Z"]) :stroke "gold" :fill "transparent"}]))]))

(defn tokens [props]
  (let [context                (uix/context context)
        {workspace  :workspace} context
        {data            :data} context
        {dispatch    :dispatch} context
        {level :lighting/level} workspace
        {grid-size  :grid/size} workspace
        {scale     :zoom/scale} workspace
        elements                (query/tokens data)]
    [:<>
     [:defs
      (when (= level :dark)
        [:clipPath {:id "clip-dim-light"}
         (for [token elements :let [{x :position/x} token {y :position/y} token]]
           [:circle {:key (:db/id token) :cx x :cy y :r (+ (* grid-size 8) (/ grid-size 2))}])])
      (when-not (= level :bright)
        [:clipPath {:id "clip-bright-light"}
         (for [token elements :let [{x :position/x} token {y :position/y} token]]
           [:circle {:key (:db/id token) :cx x :cy y :r (+ (* grid-size 4) (/ grid-size 2))}])])]
     (for [token elements :let [{x :position/x} token {y :position/y} token]]
       [:> draggable
        {:key      (:db/id token)
         :position #js {:x x :y y}
         :scale    scale
         :onStart  (handler)
         :onStop   (handler
                    (fn [_ data]
                      (let [dist (distance [x y] [(.-x data) (.-y data)])]
                        (if (= dist 0)
                          (dispatch :view/toggle (:db/id token))
                          (dispatch :token/translate (:db/id token) (.-x data) (.-y data))))))}
        [:g {:class (css (element-styles) "token" {:active (= token (:workspace/selected workspace))})}
         [:circle {:cx 0 :cy 0 :r (max (- (/ grid-size 2) 4) 8) :fill "black"}]
         (when-let [name (:element/name token)]
           [:text {:x 0 :y (+ (/ grid-size 2) 16) :text-anchor "middle" :fill "white"} name])]])]))

(defn canvas [props]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {grid-show :grid/show} workspace
        {camera-x :position/x} workspace
        {camera-y :position/y} workspace
        {mode :workspace/mode} workspace
        {image :workspace/map} workspace
        {scale    :zoom/scale} workspace
        node (uix/ref nil)]
    [:svg {:ref node :class (styles)}
     [:> draggable
      {:position #js {:x 0 :y 0}
       :disabled (= mode :grid)
       :onStart  (fn [] (dispatch :view/clear))
       :onStop
       (fn [event data]
         (let [ox (.-x data) oy (.-y data)]
           (dispatch :camera/translate
                     (+ (/ ox scale) camera-x)
                     (+ (/ oy scale) camera-y))))}
      [:g

       ;; Render an element that guarantees that the entire canvas may be
       ;; dragged from anywhere on the element.
       [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]

       [:g {:transform (str "scale(" scale ") translate(" camera-x ", " camera-y ")")}

        ;; Render the selected board map.
        [board {:key (:image/checksum image) :image image}]

        ;; Render the playing grid when it is enabled or the current mode is
        ;; :grid.
        (when (or (= mode :grid) grid-show)
          [grid {:canvas node}])

        ;; Render the various elements of the board such as tokens, geometry,
        ;; lighting, etc.
        [tokens]]

       ;; Render the drawable grid component.
       (when (= mode :grid)
         [grid-draw {:canvas node}])]]]))
