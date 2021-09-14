(ns ogre.tools.render.canvas
  (:require [clojure.set :refer [difference]]
            [clojure.string :as string :refer [join]]
            [datascript.core :as ds]
            [ogre.tools.query :as query]
            [ogre.tools.render :refer [css use-image]]
            [ogre.tools.render.pattern :refer [pattern]]
            [ogre.tools.state :refer [state]]
            [ogre.tools.vec :refer [chebyshev euclidean triangle]]
            [react-draggable :as draggable]
            [uix.core.alpha :as uix]))

(def atmosphere
  {:none
   [1 0 0 0 0
    0 1 0 0 0
    0 0 1 0 0
    0 0 0 1 0]

   :dusk
   [0.3 0.3 0.0 0.0 0.0
    0.0 0.3 0.3 0.0 0.0
    0.0 0.0 0.8 0.0 0.0
    0.0 0.0 0.0 1.0 0.0]

   :midnight
   [0.0 0.0 0.0 0.0 0.0
    0.0 0.1 0.0 0.0 0.0
    0.1 0.1 0.1 0.0 0.0
    0.0 0.0 0.0 1.0 0.0]})

(defn ft->px [ft size]
  (-> (/ ft 5) (* size)))

(defn px->ft [px size]
  (js/Math.round (* (/ px size) 5)))

(defn xf [& kvs]
  (->> (partition 2 kvs)
       (map (fn [[k v]]
              (case k
                :scale (str "scale(" v ")")
                :translate (let [[x y] v]
                             (str "translate(" x ", " y ")")))))
       (string/join " ")))

(defn text [attrs child]
  [:text.canvas-text attrs child])

(defn visible? [token]
  (let [{:keys [element/flags]} token]
    (or (nil? flags) (flags :player) (not (some flags [:hidden :invisible])))))

(defn label [{:keys [element/name initiative/suffix]}]
  (cond-> ""
    (string? name) (str name)
    (number? suffix) (str " (" (char (+ suffix 64)) ")")))

(defn board [{:keys [image]}]
  (let [{:keys [data]} (uix/context state)
        {:keys [viewer/host? viewer/workspace]} (query/viewer data)
        {:keys [canvas/lighting canvas/color grid/size zoom/scale]} workspace
        tokens (query/elements data :token)
        url (use-image (:image/checksum image) {:persist? true})]
    (if (string? url)
      [:g.canvas-image
       [:defs {:key color}
        [:filter {:id "atmosphere"}
         [:feColorMatrix
          {:type "matrix"
           :values (join " " (atmosphere (or color :none)))}]]]
       (if (not (= lighting :bright))
         [:defs
          [:clipPath {:id "clip-light-bright"}
           (for [token tokens
                 :let [{[x y] :pos/vec [r _] :token/light} token]
                 :when (and (> r 0) (or host? (visible? token)))]
             [:circle {:key (:db/id token) :cx x :cy y :r (+ (ft->px r size) (/ size 2))}])]
          (if (= lighting :dark)
            [:clipPath {:id "clip-light-dim"}
             (for [token tokens
                   :let [{[x y] :pos/vec [br dr] :token/light} token]
                   :when (and (or (> br 0) (> dr 0)) (or host? (visible? token)))]
               [:circle {:key (:db/id token) :cx x :cy y :r (+ (ft->px br size) (ft->px dr size) (/ size 2))}])])])
       (if (and (= lighting :dark) host?)
         [:image {:x 0 :y 0 :href url :style {:filter "url(#atmosphere) brightness(20%)"}}])
       (if (not (= lighting :bright))
         [:image {:x 0 :y 0 :href url :style {:filter "url(#atmosphere) brightness(50%)"} :clip-path (if (= lighting :dark) "url(#clip-light-dim)")}])
       [:image {:x 0 :y 0 :href url :style {:filter "url(#atmosphere)"} :clip-path (if (not= lighting :bright) "url(#clip-light-bright)")}]])))

(defn mask []
  (let [{:keys [data workspace]} (uix/context state)
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
  (let [{:keys [data workspace]} (uix/context state)
        {:keys [canvas/mode grid/show zoom/scale]} workspace]
    (if (or show (= mode :grid))
      (let [{[_ _ w h] :bounds/self} (query/viewer data)
            {[cx cy] :pos/vec size :grid/size} workspace
            w (/ w scale)
            h (/ h scale)
            [sx sy ax ay bx]
            [(- (* w -3) cx)
             (- (* h -3) cy)
             (- (* w  3) cx)
             (- (* h  3) cy)
             (- (* w -3) cx)]]
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
      {:points (string/join " " (apply triangle vecs))
       :fill-opacity opacity
       :stroke color})]))

(defn shapes [props]
  (let [{:keys [data workspace dispatch]} (uix/context state)
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
          {:class (css "canvas-shape"
                       (str "canvas-shape-" (name kind))
                       {:selected (contains? selected element)})}
          [:defs [pattern {:id id :name patt :color color}]]
          [shape {:element (into {} (ds/touch element)) :attrs {:fill (str "url(#" id ")")}}]])])))

(defn marker []
  [:path {:d "M7.247 11.14 2.451 5.658C1.885 5.013 2.345 4 3.204 4h9.592a1 1 0 0 1 .753 1.659l-4.796 5.48a1 1 0 0 1-1.506 0z"}])

(defn token [{:keys [entity selected size] :as props}]
  (let [flag-names  (mapv #(str "flag--" (name %)) (:element/flags entity))
        class-name  (css "canvas-token" {:selected selected} flag-names)
        token-radiu (/ (ft->px (:size (:token/size entity)) size) 2)
        token-label (label entity)
        aura-radius (:aura/radius entity)
        aura-length (+ (ft->px aura-radius size) (/ size 2))
        [cx cy]     [(* (.cos js/Math 0.75) aura-length)
                     (* (.sin js/Math 0.75) aura-length)]]
    [:g {:class class-name}
     [:circle.canvas-token-shape {:cx 0 :cy 0 :r (max (- token-radiu 4) 8)}]
     (when (seq token-label)
       [text {:x 0 :y (+ token-radiu 8)} token-label])
     (when (> aura-radius 0)
       [:circle.canvas-token-aura {:cx 0 :cy 0 :r aura-length}])
     (when (and (> aura-radius 0) (seq (:aura/label entity)))
       [text {:x (+ cx 8) :y (+ cy 8)} (:aura/label entity)])
     (when selected
       [:g.canvas-token-marker
        {:transform (xf :translate [-17 (* -1 (+ token-radiu 16))] :scale 2.20)}
        [:g.canvas-token-marker-bounce
         [marker]]])]))

(defn tokens [props]
  (let [{:keys [data workspace dispatch]} (uix/context state)
        {:keys [viewer/host?]} (query/viewer data)
        {:keys [grid/size zoom/scale canvas/selected]} workspace
        entities (-> (query/elements data :token) (set) (difference selected))]
    (for [entity entities :let [{[x y] :pos/vec} entity]
          :when (or host? (visible? entity))]
      [:> draggable
       {:key      (:db/id entity)
        :position #js {:x x :y y}
        :scale    scale
        :on-start (fn [event] (.stopPropagation event))
        :on-stop
        (fn [event data]
          (.stopPropagation event)
          (let [dist (euclidean x y (.-x data) (.-y data))]
            (if (= dist 0)
              (dispatch :element/select (:db/id entity))
              (dispatch :token/translate (:db/id entity) (.-x data) (.-y data)))))}
       [:g.canvas-token
        [token
         {:entity   (into {} (ds/touch entity))
          :size     size
          :selected false}]]])))

(defn selection []
  (let [{:keys [dispatch data workspace]} (uix/context state)
        {:keys [viewer/host?]} (query/viewer data)
        {:keys [canvas/selected grid/size zoom/scale]} workspace
        idents (map :db/id selected)]
    (if (= (:element/type (first selected)) :token)
      [:> draggable
       {:position #js {:x 0 :y 0}
        :scale scale
        :on-start (fn [event] (.stopPropagation event))
        :on-stop
        (fn [_ data]
          (let [ox (.-x data) oy (.-y data)]
            (if (not= [ox oy] [0 0])
              (dispatch :token/translate-all idents ox oy))))}
       [:g.canvas-selected {:key idents}
        (for [entity selected
              :let [{[x y] :pos/vec} entity]
              :when (or host? (visible? entity))]
          [:g.canvas-token
           {:key (:db/id entity) :transform (xf :translate [x y])}
           [token
            {:entity   (into {} (ds/touch entity))
             :size     size
             :selected true}]])]])))

(defn drawable [{:keys [on-release]} render-fn]
  (let [{:keys [data]} (uix/context state)
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
         (let [p @points]
           (reset! points nil)
           (on-release p)))}
      [:rect
       {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"
        :style {:will-change "transform"}}]]
     (when (seq @points)
       (render-fn @points))]))

(defn select []
  (let [{:keys [workspace dispatch]} (uix/context state)
        {:keys [zoom/scale]} workspace]
    [drawable
     {:on-release
      (fn [points]
        (let [[ax ay bx by] (mapv #(/ % scale) points)
              [cx cy] (:pos/vec workspace)]
          (dispatch :selection/from-rect [(- ax cx) (- ay cy) (- bx cx) (- by cy)])))}
     (fn [[ax ay bx by]]
       [:path {:d (string/join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])}])]))

(defmulti draw :mode)

(defmethod draw :grid [props]
  (let [{:keys [workspace dispatch]} (uix/context state)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:on-release
      (fn [[ax ay bx by]]
        (let [size (js/Math.abs (min (- bx ax) (- by ay)))]
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
  (let [{:keys [workspace]} (uix/context state)
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
  (let [{:keys [workspace dispatch]} (uix/context state)
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
  (let [{:keys [workspace dispatch]} (uix/context state)
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
  (let [{:keys [workspace dispatch]} (uix/context state)
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
  (let [{:keys [workspace dispatch]} (uix/context state)
        {:keys [grid/size zoom/scale]} workspace]
    [drawable
     {:on-release
      (fn [points]
        (let [[ax ay bx by] (mapv #(/ % scale) points)
              [cx cy] (:pos/vec workspace)]
          (dispatch :shape/create :cone [(- ax cx) (- ay cy) (- bx cx) (- by cy)])))}
     (fn [[ax ay bx by]]
       [:g
        [:polygon {:points (string/join " " (triangle ax ay bx by))}]
        [text {:x (+ bx 16) :y (+ by 16) :fill "white"}
         (-> (euclidean ax ay bx by)
             (px->ft (* size scale))
             (str "ft."))]])]))

(defmethod draw :default [] nil)

(defn bounds []
  (let [{:keys [data]} (uix/context state)
        {[_ _ hw hh] :bounds/host
         [_ _ gw gh] :bounds/guest} (query/viewer data)
        [ox oy] [(/ (- hw gw) 2) (/ (- hh gh) 2)]]
    [:g.canvas-bounds {:transform (xf :translate [ox oy])}
     [:rect {:x 0 :y 0 :width gw :height gh :rx 8}]]))

(defn canvas [props]
  (let [{:keys [data workspace dispatch]} (uix/context state)
        {:keys [pos/vec canvas/mode canvas/map canvas/theme zoom/scale]} workspace
        {:keys [image/width image/height]} map
        {:keys [viewer/privileged? viewer/host?]
         [_ _ hw hh] :bounds/host
         [_ _ gw gh] :bounds/guest} (query/viewer data)
        [tx ty] vec
        [tx ty] (if host? [tx ty]
                    [(- tx (/ (max 0 (- hw gw)) 2 scale))
                     (- ty (/ (max 0 (- hh gh)) 2 scale))])]
    [:svg.canvas {:class (str "theme--" (name theme))}
     [:> draggable
      {:position #js {:x 0 :y 0}
       :on-stop
       (fn [event data]
         (let [ox (.-x data) oy (.-y data)]
           (if (and (= ox 0) (= oy 0))
             (dispatch :selection/clear)
             (dispatch :camera/translate (+ (/ ox scale) tx) (+ (/ oy scale) ty)))))}
      [:g {:style {:will-change "transform"}}
       [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]
       [:g.canvas-board
        {:transform (xf :scale scale :translate [tx ty])}
        [board {:key (:image/checksum map) :image map}]
        [grid]
        [shapes]
        [tokens]
        [selection]
        [mask]]

       (when (and (= mode :select) (= (:canvas/modifier workspace) :shift))
         [:g {:class "canvas-drawable canvas-drawable-select"}
          [select]])

       (when (not= mode :select)
         [:g {:class (str "canvas-drawable canvas-drawable-" (name mode))}
          [draw {:mode mode}]])]]
     (when privileged?
       [bounds])]))
