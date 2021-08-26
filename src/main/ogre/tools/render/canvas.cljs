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

(defn board-attrs [layer lighting privileged?]
  (cond-> {}
    (and (= layer :dark) privileged?)
    (merge {:style {:filter "saturate(0%) brightness(20%)"}})
    (= layer :dim)
    (merge {:style {:filter "saturate(20%) brightness(50%)"}})
    (and (= layer :dim) (= lighting :dark))
    (merge {:clip-path "url(#clip-dim-light)"})
    (and (= layer :bright) (not= lighting :bright))
    (merge {:clip-path "url(#clip-bright-light)"})))

(defn board [{:keys [image]}]
  (let [{:keys [data]} (uix/context context)
        {:keys [viewer/privileged? viewer/workspace]} (query/viewer data)
        {:keys [canvas/lighting]} workspace
        url (use-image (:image/checksum image))]
    (when (string? url)
      (for [type (if privileged? [:dark :dim :bright] [:dim :bright])]
        [:image (merge (board-attrs type lighting privileged?)
                       {:x 0 :y 0 :href url}
                       {:key type})]))))

(defn grid []
  (let [{:keys [data workspace]} (uix/context context)
        {[_ _ w h] :canvas/bounds} (query/viewer data)
        {[cx cy] :pos/vec size :grid/size} workspace
        [sx sy ax ay bx]
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
     [:path {:d (string/join " " ["M" sx sy "H" ax "V" ay "H" bx "Z"]) :fill "url(#grid)"}]]))

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
     (when (= lighting :dark)
       [:defs
        [:clipPath {:id "clip-dim-light"}
         (for [token elements
               :let [{[x y] :pos/vec [br dr] :token/light} token]
               :when (or (> br 0) (> dr 0))]
           [:circle {:key (:db/id token) :cx x :cy y
                     :r (+ (ft->px br size) (ft->px dr size) (/ size 2))}])]])

     (when-not (= lighting :bright)
       [:defs
        [:clipPath {:id "clip-bright-light"}
         (for [token elements
               :let [{[x y] :pos/vec [r _] :token/light} token]
               :when (> r 0)]
           [:circle {:key (:db/id token) :cx x :cy y
                     :r (+ (ft->px r size) (/ size 2))}])]])

     (for [token elements :let [{[x y] :pos/vec} token]]
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

(defn drawable [{:keys [on-release]} render-fn]
  (let [{:keys [data]} (uix/context context)
        {[x y w h] :canvas/bounds} (query/viewer data)
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
          [:path {:d (string/join " " ["M" ax ay "h" m "v" m "H" ax "Z"])
                  :fill "transparent" :stroke "white" :stroke-dasharray "3px"}]
          [:text {:x bx :y ay :fill "white"}
           (-> (/ (- bx ax) scale)
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
        [:line {:x1 ax :y1 ay :x2 bx :y2 by :stroke "white" :stroke-dasharray "12px"}]
        [:text {:x (- bx 48) :y (- by 8) :fill "white"}
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
          [:circle {:cx ax :cy ay :r radius :fill "transparent" :stroke "white"}]
          [:text {:x ax :y ay :fill "white"}
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
        [:path {:d (string/join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])
                :fill "transparent" :stroke "white"}]
        [:text {:x (+ ax 8) :y (- ay 8) :fill "white"}
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
       [:g [:line {:x1 ax :y1 ay :x2 bx :y2 by
                   :stroke "white" :stroke-width 4 :stroke-linecap "round"}]
        [:text {:x (+ ax 8) :y (- ay 8) :fill "white"}
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
        [:polygon {:points (string/join " " (cone-points ax ay bx by)) :fill "transparent" :stroke "white"}]
        [:text {:x (+ bx 16) :y (+ by 16) :fill "white"}
         (-> (js/Math.hypot (- bx ax) (- by ay))
             (px->ft (* size scale))
             (str "ft."))]])]))

(defmethod draw :default [] nil)

(defn bounds []
  (let [{:keys [data]} (uix/context context)
        {[_ _ w h] :canvas/guest-bounds} (query/viewer data)]
    [:path {:d (string/join " " ["M" w 0 "V" h "H" 0])
            :fill "none" :stroke "white" :stroke-width 0.5 :stroke-dasharray 6
            :style {:pointer-events "none" :shape-rendering "crispedges"}}]))

(defn canvas [props]
  (let [{:keys [data workspace dispatch]} (uix/context context)
        {:keys [pos/vec grid/show canvas/mode canvas/map zoom/scale]} workspace
        {:keys [viewer/privileged? viewer/host?]} (query/viewer data)
        [tx ty] vec]
    [:svg.canvas
     {:class (css {:canvas--guest (not host?)})}
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
        (when (or (= mode :grid) show)
          [grid])
        [shapes]
        [tokens]]

       [draw {:mode mode}]]]
     (when privileged?
       [bounds])]))
