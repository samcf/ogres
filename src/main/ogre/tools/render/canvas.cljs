(ns ogre.tools.render.canvas
  (:require [clojure.string :as string]
            [uix.core.alpha :as uix]
            [react-draggable :as draggable]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [context css handler use-image]]
            [ogre.tools.query :as query]))

(defn ft->px [feet size]
  (-> (/ feet 5) (* size)))

(defn distance [[ax ay] [bx by]]
  (js/Math.sqrt
   (+ (js/Math.pow (- bx ax) 2)
      (js/Math.pow (- by ay) 2))))

(defclass styles []
  {:pointer-events "all" :height "100%" :width "100%"})

(defclass element-styles []
  {:pointer-events "all"}
  [:&.token>circle.token
   {:filter "drop-shadow(1px 1px 8px rgba(0, 0, 0, .8))"
    :stroke "black"}]
  [:&.token.active>circle.token
   {:stroke "#ffeb3b"
    :stroke-width "1.5px"
    :stroke-dasharray "3px"
    :stroke-linecap "round"}]
  [:&.token>circle.aura
   {:fill "none"
    :pointer-events "none"
    :stroke "white"
    :stroke-width "1px"
    :stroke-dasharray "12px"
    :stroke-linecap "round"}]
  [:&.token>text.aura
   {:fill "white"
    :pointer-events "none"}])

(defn board [{:keys [image]}]
  (let [{{:keys [canvas/lighting]} :workspace} (uix/context context)
        url (use-image (:image/checksum image))
        attrs {:bright {:clip-path (when-not (= lighting :bright) "url(#clip-bright-light)")}
               :dim    {:clip-path (when (= lighting :dark) "url(#clip-dim-light)")
                        :style     {:filter "saturate(20%) brightness(50%)"}}}]
    (when (string? url)
      [:<>
       (for [type [:dim :bright]]
         [:image (merge {:x 0 :y 0 :href url} (attrs type) {:key type})])])))

(defn grid [{:keys [canvas]}]
  (let [{{[cx cy] :pos/vec [ox oy] :grid/origin size :grid/size} :workspace} (uix/context context)
        dimensions (uix/state [0 0])
        [w h]      @dimensions]

    (uix/effect!
     (fn []
       (when @canvas
         (let [bounding (.getBoundingClientRect @canvas)]
           (reset! dimensions [(.-width bounding) (.-height bounding)]))))
     [(nil? @canvas)])

    (let [[sx sy ax ay bx]
          [(- (* w -2) cx)
           (- (* h -2) cy)
           (- (* w  2) cx)
           (- (* h  2) cy)
           (- (* w -2) cx)]]
      [:<>
       [:defs
        [:pattern {:id "grid" :width size :height size :patternUnits "userSpaceOnUse"}
         [:path
          {:d (string/join " " ["M" 0 0 "H" size "V" size])
           :stroke "rgba(255, 255, 255, 0.40)"
           :stroke-width "1"
           :stroke-dasharray "2px"
           :fill "none"}]]]
       [:path {:d (string/join " " ["M" sx sy "H" ax "V" ay "H" bx "Z"]) :fill "url(#grid)"}]])))

(defn grid-draw [{:keys [canvas]}]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [canvas/mode zoom/scale]} workspace
        canvas (.getBoundingClientRect @canvas)
        points (uix/state nil)]
    [:<>
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
         [:<>
          [:path {:d (string/join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])
                  :fill "transparent" :stroke "white" :stroke-dasharray "3px"}]
          [:text {:x bx :y ay :fill "white"}
           (-> (- (/ bx scale) (/ ax scale))
               (js/Math.abs)
               (js/Math.round)
               (str "px"))]]))]))

(defn tokens [props]
  (let [{:keys [data workspace dispatch]} (uix/context context)
        {:keys [canvas/lighting grid/size zoom/scale]} workspace
        elements (query/tokens data)]
    [:<>
     [:defs
      (when (= lighting :dark)
        [:clipPath {:id "clip-dim-light"}
         (for [token elements :let [{[x y] :pos/vec [br dr] :token/light} token]]
           [:circle {:key (:db/id token) :cx x :cy y :r (+ (ft->px br size)
                                                           (ft->px dr size)
                                                           (/ size 2))}])])

      (when-not (= lighting :bright)
        [:clipPath {:id "clip-bright-light"}
         (for [token elements :let [{[x y] :pos/vec [r _] :token/light} token]]
           [:circle {:key (:db/id token) :cx x :cy y :r (+ (ft->px r size) (/ size 2))}])])]

     (for [token elements :let [{[x y] :pos/vec} token]]
       [:> draggable
        {:key      (:db/id token)
         :position #js {:x x :y y}
         :scale    scale
         :on-start (handler)
         :on-stop
         (handler
          (fn [_ data]
            (let [dist (distance [x y] [(.-x data) (.-y data)])]
              (if (= dist 0)
                (dispatch :view/toggle (:db/id token))
                (dispatch :token/translate (:db/id token) (.-x data) (.-y data))))))}
        [:g {:class (css (element-styles) "token" {:active (= token (:canvas/selected workspace))})}

         (let [{label :element/name {token-size :size} :token/size} token
               radius (/ (ft->px token-size size) 2)]
           [:<>
            [:circle.token {:cx 0 :cy 0 :r (max (- radius 4) 8) :fill "#172125"}]
            (when (seq label)
              [:text.token {:x 0 :y (+ radius 16) :text-anchor "middle" :fill "white"} label])])

         (let [{:keys [aura/radius aura/label]} token
               length  (-> (ft->px radius size) (+ (/ size 2)))
               [cx cy] [(* (js/Math.cos 0.75) length)
                        (* (js/Math.sin 0.75) length)]]
           [:<>
            (when (> radius 0)
              [:circle.aura {:cx 0 :cy 0 :r length}])
            (when (and (> radius 0) (seq label))
              [:text.aura {:x (+ cx 8) :y (+ cy 8)} label])])]])]))

(defn canvas [props]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [pos/vec grid/show canvas/mode canvas/map zoom/scale]} workspace
        [cx cy] vec
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
                     (+ (/ ox scale) cx)
                     (+ (/ oy scale) cy))))}
      [:g

       ;; Render an element that guarantees that the entire canvas may be
       ;; dragged from anywhere on the element.
       [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]

       [:g {:transform (str "scale(" scale ") translate(" cx ", " cy ")")}

        ;; Render the selected board map.
        [board {:key (:image/checksum map) :image map}]

        ;; Render the playing grid when it is enabled or the current mode is
        ;; :grid.
        (when (or (= mode :grid) show)
          [grid {:canvas node}])

        ;; Render the various elements of the board such as tokens, geometry,
        ;; lighting, etc.
        [tokens]]

       ;; Render the drawable grid component.
       (when (and (not (nil? @node)) (= mode :grid))
         [grid-draw {:canvas node}])]]]))
