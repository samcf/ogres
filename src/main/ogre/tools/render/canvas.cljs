(ns ogre.tools.render.canvas
  (:require [clojure.string :as string]
            [datascript.core :as ds]
            [uix.core.alpha :as uix]
            [react-draggable :as draggable]
            [ogre.tools.render :refer [context css handler use-dimensions use-image]]
            [ogre.tools.render.pattern :refer [pattern]]
            [ogre.tools.query :as query]))

(defn ft->px [ft size]
  (-> (/ ft 5) (* size)))

(defn px->ft [px size]
  (js/Math.round (* (/ px size) 5)))

(defn euclidean [[ax ay] [bx by]]
  (js/Math.hypot (- bx ax) (- by ay)))

(defn chebyshev [[ax ay] [bx by]]
  (max (js/Math.abs (- ax bx))
       (js/Math.abs (- ay by))))

(defn cone-points [[ax ay] [bx by]]
  (let [alt (js/Math.hypot (- bx ax) (- by ay))
        hyp (js/Math.hypot alt (/ alt 2))
        rad (js/Math.atan2 (- by ay) (- bx ax))]
    [ax
     ay
     (+ ax (* hyp (js/Math.cos (+ rad 0.46))))
     (+ ay (* hyp (js/Math.sin (+ rad 0.46))))
     (+ ax (* hyp (js/Math.cos (- rad 0.46))))
     (+ ay (* hyp (js/Math.sin (- rad 0.46))))]))

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

(defn grid [{[_ _ w h] :dimensions}]
  (let [{{[cx cy] :pos/vec size :grid/size} :workspace} (uix/context context)]
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

(defmulti shape (fn [props] (:shape/kind (:element props))))

(defmethod shape :circle [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity shape/pattern]} element
        [[ax ay] [bx by]] vecs]
    [:circle
     (merge
      attrs
      {:cx ax
       :cy ay
       :r (chebyshev [ax ay] [bx by])
       :fill-opacity opacity
       :stroke color})]))

(defmethod shape :rect [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [[ax ay] [bx by]] vecs]
    [:path
     (merge attrs {:d (string/join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])
                   :fill-opacity opacity :stroke color})]))

(defmethod shape :line [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [[ax ay] [bx by]] vecs]
    [:line {:x1 ax :y1 ay :x2 bx :y2 by :stroke color :stroke-width 4 :stroke-linecap "round"}]))

(defmethod shape :cone [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [[ax ay] [bx by]] vecs]
    [:polygon
     (merge
      attrs
      {:points (string/join " " (cone-points [ax ay] [bx by]))
       :fill-opacity opacity
       :stroke color})]))

(defn shapes [props]
  (let [{:keys [data workspace dispatch]} (uix/context context)
        {:keys [zoom/scale canvas/selected]} workspace
        elements (query/elements data :shape)]
    (for [element elements :let [[x y] (:pos/vec element)]]
      [:> draggable
       {:key (:db/id element)
        :position #js {:x x :y y}
        :scale scale
        :on-start
        (fn [event data]
          (.stopPropagation event))
        :on-stop
        (fn [_ data]
          (let [dist (euclidean [x y] [(.-x data) (.-y data)])]
            (if (> dist 0)
              (dispatch :shape/translate (:db/id element) [(.-x data) (.-y data)])
              (dispatch :element/select (:db/id element)))))}
       (let [{patt :shape/pattern color :shape/color kind :shape/kind} element
             id (ds/squuid)]
         [:g
          {:class
           (css "canvas-shape"
                (str "canvas-shape-" (name kind))
                {"selected" (= element selected)})}
          [:defs [pattern {:id id :name patt :color color}]]
          [shape {:element (into {} (ds/touch element))
                  :attrs   {:fill (str "url(#" id ")")}}]])])))

(defn tokens [props]
  (let [{:keys [data workspace dispatch]} (uix/context context)
        {:keys [canvas/lighting grid/size zoom/scale]} workspace
        elements (query/elements data :token)]
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
            (let [dist (euclidean [x y] [(.-x data) (.-y data)])]
              (if (= dist 0)
                (dispatch :element/select (:db/id token))
                (dispatch :token/translate (:db/id token) (.-x data) (.-y data))))))}
        [:g.canvas-token {:class (css {:selected (= token (:canvas/selected workspace))})}

         (let [{label :element/name {token-size :size} :token/size} token
               radius (/ (ft->px token-size size) 2)]
           [:g.canvas-token-shape
            [:circle {:cx 0 :cy 0 :r (max (- radius 4) 8) :fill "#172125"}]
            (when (seq label)
              [:text {:x 0 :y (+ radius 16) :text-anchor "middle" :fill "white"} label])])

         (let [{:keys [aura/radius aura/label]} token
               length  (-> (ft->px radius size) (+ (/ size 2)))
               [cx cy] [(* (js/Math.cos 0.75) length)
                        (* (js/Math.sin 0.75) length)]]
           [:g.canvas-token-aura
            (when (> radius 0)
              [:circle {:cx 0 :cy 0 :r length}])
            (when (and (> radius 0) (seq label))
              [:text {:x (+ cx 8) :y (+ cy 8)} label])])]])]))

(defn drawable [props render-fn]
  (let [{:keys [dimensions on-release]} props
        points (uix/state nil)
        [x y w h] dimensions]
    [:<>
     [:> draggable
      {:position #js {:x 0 :y 0}
       :on-start
       (fn [event _]
         (.stopPropagation event)
         (let [x (- (.-clientX event) x)
               y (- (.-clientY event) y)]
           (reset! points [x y x y])))
       :on-drag
       (fn [_ data]
         (swap! points
                (fn [[ax ay bx by]]
                  [ax ay (+ ax (.-x data)) (+ ay (.-y data))])))
       :on-stop
       (fn []
         (on-release @points)
         (reset! points nil))}
      [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]]
     (when (seq @points)
       (render-fn @points))]))

(defmulti draw :mode)

(defmethod draw :grid [props]
  (let [{:keys [dimensions]} props
        {:keys [workspace dispatch]} (uix/context context)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:dimensions dimensions
      :on-release
      (fn [[ax ay bx by]]
        (let [size (js/Math.abs (- bx ax))]
          (when (> size 0)
            (dispatch :grid/draw ax ay size))))}
     (fn [[ax ay bx by]]
       (let [m (min (- bx ax) (- by ay))]
         [:g
          [:path {:d (string/join " " ["M" ax ay "h" m "v" m "H" ax "Z"])
                  :fill "transparent" :stroke "white" :stroke-dasharray "3px"}]
          [:text {:x bx :y ay :fill "white"}
           (-> (/ (- bx ax) scale)
               (js/Math.abs)
               (js/Math.round)
               (str "px"))]]))]))

(defmethod draw :ruler [props]
  (let [{:keys [dimensions]} props
        {:keys [workspace]} (uix/context context)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:dimensions dimensions
      :on-release identity}
     (fn [[ax ay bx by]]
       [:g
        [:line {:x1 ax :y1 ay :x2 bx :y2 by :stroke "white" :stroke-dasharray "12px"}]
        [:text {:x (- bx 48) :y (- by 8) :fill "white"}
         (str (px->ft (chebyshev [ax ay] [bx by]) (* size scale)) "ft.")]])]))

(defmethod draw :circle [props]
  (let [{:keys [dimensions]} props
        {:keys [workspace dispatch]} (uix/context context)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:dimensions dimensions
      :on-release
      (fn [points]
        (let [[ax ay bx by] (mapv #(/ % scale) points)
              [cx cy] (:pos/vec workspace)
              src [(- ax cx) (- ay cy)]
              dst [(- bx cx) (- by cy)]]
          (dispatch :shape/create :circle src dst)))}
     (fn [[ax ay bx by]]
       (let [radius (chebyshev [ax ay] [bx by])]
         [:g
          [:circle {:cx ax :cy ay :r radius :fill "transparent" :stroke "white"}]
          [:text {:x ax :y ay :fill "white"}
           (str (px->ft radius (* size scale)) "ft. radius")]]))]))

(defmethod draw :rect [props]
  (let [{:keys [dimensions]} props
        {:keys [workspace dispatch]} (uix/context context)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:dimensions dimensions
      :on-release
      (fn [points]
        (let [[ax ay bx by] (mapv #(/ % scale) points)
              [cx cy] (:pos/vec workspace)
              src [(- ax cx) (- ay cy)]
              dst [(- bx cx) (- by cy)]]
          (dispatch :shape/create :rect src dst)))}
     (fn [[ax ay bx by]]
       [:g
        [:path {:d (string/join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])
                :fill "transparent" :stroke "white"}]
        [:text {:x (+ ax 8) :y (- ay 8) :fill "white"}
         (let [[w h] [(px->ft (js/Math.abs (- bx ax)) (* size scale))
                      (px->ft (js/Math.abs (- by ay)) (* size scale))]]
           (str w "ft. x " h "ft."))]])]))

(defmethod draw :line [props]
  (let [{:keys [dimensions]} props
        {:keys [workspace dispatch]} (uix/context context)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:dimensions dimensions
      :on-release
      (fn [points]
        (let [[ax ay bx by] (mapv #(/ % scale) points)
              [cx cy] (:pos/vec workspace)
              src [(- ax cx) (- ay cy)]
              dst [(- bx cx) (- by cy)]]
          (dispatch :shape/create :line src dst)))}
     (fn [[ax ay bx by]]
       [:g [:line {:x1 ax :y1 ay :x2 bx :y2 by
                   :stroke "white" :stroke-width 4 :stroke-linecap "round"}]
        [:text {:x (+ ax 8) :y (- ay 8) :fill "white"}
         (str (px->ft (chebyshev [ax ay] [bx by]) (* size scale)) "ft.")]])]))

(defmethod draw :cone [props]
  (let [{:keys [dimensions]} props
        {:keys [workspace dispatch]} (uix/context context)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:dimensions dimensions
      :on-release
      (fn [points]
        (let [[ax ay bx by] (mapv #(/ % scale) points)
              [cx cy] (:pos/vec workspace)
              src [(- ax cx) (- ay cy)]
              dst [(- bx cx) (- by cy)]]
          (dispatch :shape/create :cone src dst)))}
     (fn [[ax ay bx by]]
       [:g
        [:polygon {:points (string/join " " (cone-points [ax ay] [bx by])) :fill "transparent" :stroke "white"}]
        [:text {:x (+ bx 16) :y (+ by 16) :fill "white"}
         (str (px->ft (js/Math.hypot (- bx ax) (- by ay))
                      (* size scale)) "ft.")]])]))

(defmethod draw :default [] nil)

(defn canvas [props]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [pos/vec grid/show canvas/mode canvas/map zoom/scale]} workspace
        [tx ty] vec
        [ref dimensions] (use-dimensions)]
    [:svg.canvas {:ref ref}
     [:> draggable
      {:position #js {:x 0 :y 0}
       :on-start (fn [] (dispatch :view/clear))
       :on-stop
       (fn [event data]
         (let [ox (.-x data) oy (.-y data)]
           (dispatch :camera/translate (+ (/ ox scale) tx) (+ (/ oy scale) ty))))}
      [:g
       [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]
       [:g {:transform (str "scale(" scale ") translate(" tx ", " ty ")")}
        [board {:key (:image/checksum map) :image map}]
        (when (or (= mode :grid) show)
          [grid {:dimensions dimensions}])
        [shapes]
        [tokens]]

       [draw {:mode mode :dimensions dimensions}]]]]))
