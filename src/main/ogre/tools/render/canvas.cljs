(ns ogre.tools.render.canvas
  (:require [clojure.string :as string]
            [datascript.core :as ds]
            [uix.core.alpha :as uix]
            [react-draggable :as draggable]
            [ogre.tools.render :refer [context css use-image]]
            [ogre.tools.render.pattern :refer [pattern]]
            [ogre.tools.query :as query]))

(defn ft->px [ft size]
  (-> (/ ft 5) (* size)))

(defn px->ft [px size]
  (js/Math.round (* (/ px size) 5)))

(defn euclidean [ax ay bx by]
  (js/Math.hypot (- bx ax) (- by ay)))

(defn chebyshev [ax ay bx by]
  (max (js/Math.abs (- ax bx))
       (js/Math.abs (- ay by))))

(defn cone-points [ax ay bx by]
  (let [alt (js/Math.hypot (- bx ax) (- by ay))
        hyp (js/Math.hypot alt (/ alt 2))
        rad (js/Math.atan2 (- by ay) (- bx ax))]
    [ax
     ay
     (+ ax (* hyp (js/Math.cos (+ rad 0.46))))
     (+ ay (* hyp (js/Math.sin (+ rad 0.46))))
     (+ ax (* hyp (js/Math.cos (- rad 0.46))))
     (+ ay (* hyp (js/Math.sin (- rad 0.46))))]))

(defn text [attrs child]
  [:<>
   [:text.canvas-text-outline (merge attrs) child]
   [:text.canvas-text attrs child]])

(defn visible? [token]
  (let [{:keys [element/flags]} token]
    (or (nil? flags) (flags :player) (not (some flags [:hidden :invisible])))))

(defn board [{:keys [image]}]
  (let [{:keys [data]} (uix/context context)
        {:keys [viewer/host? viewer/workspace]} (query/viewer data)
        {:keys [canvas/lighting grid/size zoom/scale]} workspace
        tokens (query/elements data :token)
        url (use-image (:image/checksum image))]
    (when (string? url)
      [:g.canvas-image
       (when (not (= lighting :bright))
         [:defs
          [:clipPath {:id "clip-light-bright"}
           (for [token tokens :let [{[x y] :pos/vec [r _] :token/light} token]
                 :when (and (> r 0) (or host? (visible? token)))]
             [:circle {:key (:db/id token) :cx x :cy y :r (+ (ft->px r size) (/ size 2))}])]
          (when (= lighting :dark)
            [:clipPath {:id "clip-light-dim"}
             (for [token tokens :let [{[x y] :pos/vec [br dr] :token/light} token]
                   :when (and (or (> br 0) (> dr 0)) (or host? (visible? token)))]
               [:circle {:key (:db/id token) :cx x :cy y :r (+ (ft->px br size) (ft->px dr size) (/ size 2))}])])])
       (when (and (= lighting :dark) host?)
         [:image {:x 0 :y 0 :href url :style {:filter "saturate(0%) brightness(20%)"}}])
       (when (not (= lighting :bright))
         [:image {:x 0 :y 0 :href url :clip-path (when (= lighting :dark) "url(#clip-light-dim)") :style {:filter "saturate(20%) brightness(50%)"}}])
       [:image {:x 0 :y 0 :href url :clip-path (when (not (= lighting :bright)) "url(#clip-light-bright)")}]])))

(defn mask []
  (let [{:keys [data workspace]} (uix/context context)
        {:keys [viewer/host?]} (query/viewer data)
        {:keys [canvas/lighting canvas/map]} workspace
        {:keys [image/width image/height]} map]
    (when (and (not host?) (= lighting :dark))
      [:g.canvas-mask
       [:defs
        [:mask {:id "mask-light-all"}
         [:rect {:x 0 :y 0 :width width :height height :fill "white"}]
         [:rect {:x 0 :y 0 :width width :height height :clip-path "url(#clip-light-dim)" :fill "black"}]
         [:rect {:x 0 :y 0 :width width :height height :clip-path "url(#clip-light-bright)" :fill "black"}]]]
       [:rect {:x 0 :y 0 :width width :height height :mask "url(#mask-light-all)"}]])))

(defn grid []
  (let [{:keys [data workspace]} (uix/context context)
        {:keys [canvas/mode grid/show]} workspace]
    (when (or (= mode :grid) show)
      (let [{[_ _ w h] :bounds/self} (query/viewer data)
            {[cx cy] :pos/vec size :grid/size} workspace
            [sx sy ax ay bx]
            [(- (* w -2) cx)
             (- (* h -2) cy)
             (- (* w  2) cx)
             (- (* h  2) cy)
             (- (* w -2) cx)]]
        [:g {:class "canvas-grid"}
         [:defs
          [:pattern {:id "grid" :width size :height size :patternUnits "userSpaceOnUse"}
           [:path
            {:d (string/join " " ["M" 0 0 "H" size "V" size])}]]]
         [:path {:d (string/join " " ["M" sx sy "H" ax "V" ay "H" bx "Z"]) :fill "url(#grid)"}]]))))

(defmulti shape (fn [props] (:shape/kind (:element props))))

(defmethod shape :circle [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity shape/pattern]} element
        [ax ay bx by] vecs]
    [:circle
     (merge
      attrs
      {:cx ax
       :cy ay
       :r (chebyshev ax ay bx by)
       :fill-opacity opacity
       :stroke color})]))

(defmethod shape :rect [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [ax ay bx by] vecs]
    [:path
     (merge
      attrs
      {:d (string/join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])
       :fill-opacity opacity :stroke color})]))

(defmethod shape :line [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [ax ay bx by] vecs]
    [:line {:x1 ax :y1 ay :x2 bx :y2 by :stroke color :stroke-width 4 :stroke-linecap "round"}]))

(defmethod shape :cone [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element]
    [:polygon
     (merge
      attrs
      {:points (string/join " " (apply cone-points vecs))
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
          (let [dist (euclidean x y (.-x data) (.-y data))]
            (if (> dist 0)
              (dispatch :shape/translate (:db/id element) (.-x data) (.-y data))
              (dispatch :element/select (:db/id element)))))}
       (let [{patt :shape/pattern color :shape/color kind :shape/kind} element
             id (ds/squuid)]
         [:g
          {:class (css "canvas-shape" (str "canvas-shape-" (name kind)) {"selected" (= element selected)})}
          [:defs [pattern {:id id :name patt :color color}]]
          [shape {:element (into {} (ds/touch element)) :attrs {:fill (str "url(#" id ")")}}]])])))

(defn tokens [props]
  (let [{:keys [data workspace dispatch]} (uix/context context)
        {:keys [viewer/host?]} (query/viewer data)
        {:keys [grid/size zoom/scale canvas/map]} workspace
        elements (query/elements data :token)]
    (for [token elements :let [{[x y] :pos/vec} token] :when (or host? (visible? token))]
      [:> draggable
       {:key      (:db/id token)
        :position #js {:x x :y y}
        :scale    scale
        :on-start (fn [event] (.stopPropagation event))
        :on-stop
        (fn [event data]
          (.stopPropagation event)
          (let [dist (euclidean x y (.-x data) (.-y data))]
            (if (= dist 0)
              (dispatch :element/select (:db/id token))
              (dispatch :token/translate (:db/id token) (.-x data) (.-y data)))))}
       [:g.canvas-token
        {:class (css {:selected (= token (:canvas/selected workspace))}
                     (mapv #(str "flag--" (name %)) (:element/flags token)))}
        (let [{label :element/name {token-size :size} :token/size} token
              radius (/ (ft->px token-size size) 2)]
          [:g.canvas-token-shape
           [:circle {:cx 0 :cy 0 :r (max (- radius 4) 8) :fill "#172125"}]
           (when (seq label)
             [text {:x 0 :y (+ radius 16) :text-anchor "middle" :fill "white"} label])])
        (let [{:keys [aura/radius aura/label]} token
              length  (-> (ft->px radius size) (+ (/ size 2)))
              [cx cy] [(* (js/Math.cos 0.75) length)
                       (* (js/Math.sin 0.75) length)]]
          [:g.canvas-token-aura
           (when (> radius 0)
             [:circle {:cx 0 :cy 0 :r length}])
           (when (and (> radius 0) (seq label))
             [text {:x (+ cx 8) :y (+ cy 8)} label])])]])))

(defn drawable [{:keys [on-release]} render-fn]
  (let [{:keys [data]} (uix/context context)
        {[x y w h] :bounds/self} (query/viewer data)
        points (uix/state nil)]
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
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:on-release
      (fn [[ax ay bx by]]
        (let [size (js/Math.abs (- bx ax))]
          (when (> size 0)
            (dispatch :grid/draw ax ay size))))}
     (fn [[ax ay bx by]]
       (let [m (min (- bx ax) (- by ay))]
         [:g
          [:path {:d (string/join " " ["M" ax ay "h" m "v" m "H" ax "Z"])}]
          [text {:x bx :y ay :fill "white"}
           (-> (/ m scale)
               (js/Math.abs)
               (js/Math.round)
               (str "px"))]]))]))

(defmethod draw :ruler [props]
  (let [{:keys [workspace]} (uix/context context)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:on-release identity}
     (fn [[ax ay bx by]]
       [:g
        [:line {:x1 ax :y1 ay :x2 bx :y2 by}]
        [text {:x (- bx 48) :y (- by 8) :fill "white"}
         (-> (chebyshev ax ay bx by)
             (px->ft (* size scale))
             (str "ft."))]])]))

(defmethod draw :circle [props]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:on-release
      (fn [points]
        (let [[ax ay bx by] (mapv #(/ % scale) points)
              [cx cy] (:pos/vec workspace)]
          (dispatch :shape/create :circle [(- ax cx) (- ay cy) (- bx cx) (- by cy)])))}
     (fn [[ax ay bx by]]
       (let [radius (chebyshev ax ay bx by)]
         [:g
          [:circle {:cx ax :cy ay :r radius}]
          [text {:x ax :y ay :fill "white"}
           (-> radius (px->ft (* size scale)) (str "ft. radius"))]]))]))

(defmethod draw :rect [props]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:on-release
      (fn [points]
        (let [[ax ay bx by] (mapv #(/ % scale) points)
              [cx cy] (:pos/vec workspace)]
          (dispatch :shape/create :rect [(- ax cx) (- ay cy) (- bx cx) (- by cy)])))}
     (fn [[ax ay bx by]]
       [:g
        [:path {:d (string/join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])}]
        [text {:x (+ ax 8) :y (- ay 8) :fill "white"}
         (let [[w h] [(px->ft (js/Math.abs (- bx ax)) (* size scale))
                      (px->ft (js/Math.abs (- by ay)) (* size scale))]]
           (str w "ft. x " h "ft."))]])]))

(defmethod draw :line [props]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:on-release
      (fn [points]
        (let [[ax ay bx by] (mapv #(/ % scale) points)
              [cx cy] (:pos/vec workspace)]
          (dispatch :shape/create :line [(- ax cx) (- ay cy) (- bx cx) (- by cy)])))}
     (fn [[ax ay bx by]]
       [:g [:line {:x1 ax :y1 ay :x2 bx :y2 by}]
        [text {:x (+ ax 8) :y (- ay 8) :fill "white"}
         (-> (chebyshev ax ay bx by)
             (px->ft (* size scale))
             (str "ft."))]])]))

(defmethod draw :cone [props]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:on-release
      (fn [points]
        (let [[ax ay bx by] (mapv #(/ % scale) points)
              [cx cy] (:pos/vec workspace)]
          (dispatch :shape/create :cone [(- ax cx) (- ay cy) (- bx cx) (- by cy)])))}
     (fn [[ax ay bx by]]
       [:g
        [:polygon {:points (string/join " " (cone-points ax ay bx by))}]
        [text {:x (+ bx 16) :y (+ by 16) :fill "white"}
         (-> (js/Math.hypot (- bx ax) (- by ay))
             (px->ft (* size scale))
             (str "ft."))]])]))

(defmethod draw :default [] nil)

(defn bounds []
  (let [{:keys [data]} (uix/context context)
        {[_ _ hw hh] :bounds/host
         [_ _ gw gh] :bounds/guest} (query/viewer data)
        [ox oy] [(/ (- hw gw) 2) (/ (- hh gh) 2)]]
    [:g.canvas-bounds {:transform (str "translate(" ox ", " oy ")")}
     [:path {:d (string/join " " ["M" 0 0 "H" gw "V" gh "H" 0 "Z"])}]]))

(defn canvas [props]
  (let [{:keys [data workspace dispatch]} (uix/context context)
        {:keys [pos/vec canvas/mode canvas/map canvas/theme zoom/scale]} workspace
        {:keys [image/width image/height]} map
        {:keys [viewer/privileged? viewer/host?]
         [_ _ hw hh] :bounds/host
         [_ _ gw gh] :bounds/guest} (query/viewer data)
        [tx ty] vec
        [tx ty] (if host? [tx ty]
                    [(- tx (/ (max 0 (- hw gw)) 2 scale))
                     (- ty (/ (max 0 (- hh gh)) 2 scale))])]
    [:svg.canvas
     {:class (css {:theme--guest (not host?)} (str "theme--" (name theme)))}
     [:> draggable
      {:position #js {:x 0 :y 0}
       :on-start (fn [] (dispatch :view/clear))
       :on-stop
       (fn [event data]
         (let [ox (.-x data) oy (.-y data)]
           (dispatch :camera/translate (+ (/ ox scale) tx) (+ (/ oy scale) ty))))}
      [:g
       [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]
       [:g.canvas-board {:transform (str "scale(" scale ") translate(" tx ", " ty ")")}
        [board {:key (:image/checksum map) :image map}]
        [grid]
        [shapes]
        [tokens]
        [mask]]

       [:g.canvas-drawable {:class (css (str "canvas-drawable-" (name mode)))}
        [draw {:mode mode}]]]]
     (when privileged?
       [bounds])]))
