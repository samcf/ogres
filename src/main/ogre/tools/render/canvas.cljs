(ns ogre.tools.render.canvas
  (:require [clojure.set :refer [difference]]
            [clojure.string :as string :refer [join]]
            [datascript.core :refer [squuid]]
            [ogre.tools.render :refer [css use-image]]
            [ogre.tools.state :refer [use-query]]
            [ogre.tools.render.pattern :refer [pattern]]
            [ogre.tools.vec :as vec :refer [chebyshev euclidean triangle]]
            [react-draggable :as draggable]
            [uix.core.alpha :as uix]
            [uix.dom.alpha :refer [create-portal]]))

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

(defn visible? [flags]
  (or (nil? flags)
      (flags :player)
      (not (some flags [:hidden :invisible]))))

(defn label [{:keys [element/name initiative/suffix]}]
  (cond-> ""
    (string? name) (str name)
    (number? suffix) (str " " (char (+ suffix 64)))))

(def board-query
  {:pull
   [:root/host?
    {:root/canvas
     [[:canvas/lighting :default :bright]
      [:canvas/color :default :none]
      [:grid/size :default 70]
      [:zoom/scale :default 1]
      {:canvas/scene [:image/checksum]}
      {:canvas/tokens
       [:db/id
        [:element/flags :default #{}]
        [:token/light :default [5 5]]
        [:pos/vec :default [0 0]]]}]}]})

(defn board []
  (let [[result] (use-query board-query)
        {host? :root/host?
         {lighting :canvas/lighting
          color    :canvas/color
          tokens   :canvas/tokens
          size     :grid/size
          scale    :zoom/scale
          {checksum :image/checksum}
          :canvas/scene}
         :root/canvas} result
        url (use-image checksum)]
    (if (string? url)
      [:g.canvas-image
       [:defs {:key color}
        [:filter {:id "atmosphere"}
         [:feColorMatrix
          {:type "matrix"
           :values (join " " (atmosphere color))}]]]
       (if (not (= lighting :bright))
         [:defs
          [:clipPath {:id "clip-light-bright"}
           (for [token tokens
                 :let [{[x y] :pos/vec [r _] :token/light} token]
                 :when (and (> r 0)
                            (or host? (visible? (:element/flags token))))]
             [:circle {:key (:db/id token) :cx x :cy y :r (+ (ft->px r size) (/ size 2))}])]
          (if (= lighting :dark)
            [:clipPath {:id "clip-light-dim"}
             (for [token tokens
                   :let [{[x y] :pos/vec [br dr] :token/light} token]
                   :when (and (or (> br 0) (> dr 0))
                              (or host? (visible? (:element/flags token))))]
               [:circle {:key (:db/id token) :cx x :cy y :r (+ (ft->px br size) (ft->px dr size) (/ size 2))}])])])
       (if (and (= lighting :dark) host?)
         [:image {:x 0 :y 0 :href url :style {:filter "url(#atmosphere) brightness(20%)"}}])
       (if (not (= lighting :bright))
         [:image {:x 0 :y 0 :href url :style {:filter "url(#atmosphere) brightness(50%)"} :clip-path (if (= lighting :dark) "url(#clip-light-dim)")}])
       [:image {:x 0 :y 0 :href url :style {:filter "url(#atmosphere)"} :clip-path (if (not= lighting :bright) "url(#clip-light-bright)")}]])))

(def mask-query
  {:pull
   [:root/host?
    {:root/canvas
     [:canvas/lighting
      {:canvas/scene
       [:image/width
        :image/height]}]}]})

(defn mask []
  (let [[data dispatch] (use-query mask-query)
        {host? :root/host?
         {lighting :canvas/lighting
          {width  :image/width
           height :image/height} :canvas/scene} :root/canvas} data]
    (when (and (not host?) (= lighting :dark))
      [:g.canvas-mask
       [:defs
        [:mask {:id "mask-light-all"}
         [:rect {:x 0 :y 0 :width width :height height :fill "white"}]
         [:rect {:x 0 :y 0 :width width :height height :clip-path "url(#clip-light-dim)" :fill "black"}]
         [:rect {:x 0 :y 0 :width width :height height :clip-path "url(#clip-light-bright)" :fill "black"}]]]
       [:rect {:x 0 :y 0 :width width :height height :mask "url(#mask-light-all)"}]])))

(def grid-query
  {:pull
   [:bounds/self
    {:root/canvas
     [[:canvas/mode :default :select]
      [:pos/vec :default [0 0]]
      [:grid/size :default 70]
      [:grid/show :default true]
      [:zoom/scale :default 1]]}]})

(defn grid []
  (let [[data dispatch] (use-query grid-query)
        {[_ _ w h] :bounds/self
         {mode    :canvas/mode
          size    :grid/size
          show    :grid/show
          scale   :zoom/scale
          [cx cy] :pos/vec} :root/canvas} data]
    (if (or show (= mode :grid))
      (let [w (/ w scale)
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
      {:cx 0 :cy 0 :r (chebyshev ax ay bx by)
       :fill-opacity opacity :stroke color})]))

(defmethod shape :rect [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [ax ay bx by] vecs]
    [:path
     (merge
      attrs
      {:d (string/join " " ["M" 0 0 "H" (- bx ax) "V" (- by ay) "H" 0 "Z"])
       :fill-opacity opacity :stroke color})]))

(defmethod shape :line [props]
  (let [{:keys [element]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [ax ay bx by] vecs]
    [:line {:x1 0 :y1 0 :x2 (- bx ax) :y2 (- by ay) :stroke color :stroke-width 4 :stroke-linecap "round"}]))

(defmethod shape :cone [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [ax ay bx by] vecs]
    [:polygon
     (merge
      attrs
      {:points (string/join " " (triangle 0 0 (- bx ax) (- by ay)))
       :fill-opacity opacity :stroke color})]))

(def shapes-query
  {:pull
   '[{:root/canvas
      [[:zoom/scale :default 1]
       [:grid/align :default false]
       {:canvas/shapes
        [:db/id
         :element/name
         :shape/kind
         :shape/vecs
         [:shape/color :default "#f44336"]
         [:shape/opacity :default 0.25]
         [:shape/pattern :default :solid]
         :canvas/_selected]}]}]})

(defn shapes [props]
  (let [[result dispatch] (use-query shapes-query)]
    (for [element (-> result :root/canvas :canvas/shapes)]
      (let [{id :db/id [ax ay] :shape/vecs} element]
        [:> draggable
         {:key      id
          :scale    (-> result :root/canvas :zoom/scale)
          :position #js {:x ax :y ay}
          :on-start (fn [event] (.stopPropagation event))
          :on-stop
          (fn [event data]
            (let [ox (.-x data) oy (.-y data)]
              (if (> (euclidean ax ay ox oy) 0)
                (dispatch :shape/translate id ox oy (not= (.-metaKey event) (-> result :root/canvas :grid/align)))
                (dispatch :element/select id))))}
         (let [id (squuid)]
           [:g
            {:css
             {:canvas-shape true
              :selected (:canvas/_selected element)
              (str "canvas-shape-" (name (:shape/kind element))) true}}
            [:defs [pattern {:id id :name (:shape/pattern element) :color (:shape/color element)}]]
            [shape {:element element :attrs {:fill (str "url(#" id ")")}}]])]))))

(defn marker []
  [:path {:d "M7.247 11.14 2.451 5.658C1.885 5.013 2.345 4 3.204 4h9.592a1 1 0 0 1 .753 1.659l-4.796 5.48a1 1 0 0 1-1.506 0z"}])

(defn token-query [id]
  {:query
   '[:find (pull $ ?id pattern) . :in $ ?id pattern]

   :pull
   [:element/name
    [:element/flags :default #{}]
    [:token/size :default {:name :medium :size 5}]
    {:token/stamp [:image/checksum]}
    :aura/label
    [:aura/radius :default 0]
    :initiative/suffix
    :canvas/_selected
    {:canvas/_tokens [[:grid/size :default 70]]}]

   :args
   [id]})

(defn token [props]
  (let [[data dispatch] (use-query (token-query (:id props)))
        flags       (:element/flags data)
        flag-names  (mapv #(str "flag--" (name %)) flags)
        class-name  (css "canvas-token" {:selected (:canvas/_selected data)} flag-names)
        size        (-> data :canvas/_tokens :grid/size)
        token-radiu (/ (ft->px (:size (:token/size data)) size) 2)
        token-label (label data)
        aura-radius (:aura/radius data)
        aura-length (+ (ft->px aura-radius size) (/ size 2))
        checksum    (-> data :token/stamp :image/checksum)
        pattern-url (cond
                      (flags :deceased)  "token-stamp-deceased"
                      (string? checksum) (str "token-stamp-" checksum)
                      :else              "token-stamp-default")
        [cx cy]     [(* (.cos js/Math 0.75) aura-length)
                     (* (.sin js/Math 0.75) aura-length)]]
    [:g {:class class-name}
     [:circle.canvas-token-shape
      {:cx 0 :cy 0 :r (max (- token-radiu 2) 8) :fill (str "url(#" pattern-url ")")}]
     (if (seq token-label)
       [text {:x 0 :y (+ token-radiu 8)} token-label])
     (if (> aura-radius 0)
       [:circle.canvas-token-aura {:cx 0 :cy 0 :r aura-length}])
     (if (and (> aura-radius 0) (seq (:aura/label data)))
       [text {:x (+ cx 8) :y (+ cy 8)} (:aura/label data)])
     (if (:canvas/_selected data)
       [:g.canvas-token-marker
        {:transform (xf :translate [-17 (* -1 (+ token-radiu 20))] :scale 2.20)}
        [:g.canvas-token-marker-bounce
         [marker]]])]))

(defn stamp [{:keys [checksum]}]
  (let [url (use-image checksum)]
    [:image {:href url :width 1 :height 1 :preserveAspectRatio "xMidYMin slice"}]))

(def stamps-query
  {:pull
   [{:root/canvas
     [{:canvas/tokens
       [{:token/stamp [:image/checksum]}]}]}]})

(defn stamps [props]
  (let [[data] (use-query stamps-query)
        lookup (fn [t] (-> t :token/stamp :image/checksum))
        images (->> data :root/canvas :canvas/tokens (map lookup) set)
        attrs  {:width "100%" :height "100%" :patternContentUnits "objectBoundingBox"}]
    [:defs
     [:pattern (merge attrs {:id "token-stamp-default" :viewBox "0 0 16 16" :fill "#f2f2eb"})
      [:rect {:x 0 :y 0 :width 16 :height 16 :fill "hsl(200, 20%, 12%)"}]
      [:path {:d "M11 6a3 3 0 1 1-6 0 3 3 0 0 1 6 0z"}]
      [:path {:d "M0 8a8 8 0 1 1 16 0A8 8 0 0 1 0 8zm8-7a7 7 0 0 0-5.468 11.37C3.242 11.226 4.805 10 8 10s4.757 1.225 5.468 2.37A7 7 0 0 0 8 1z" :fill-rule "evenodd"}]]
     [:pattern (merge attrs {:id "token-stamp-deceased" :viewBox "0 0 16 16" :fill "#f2f2eb"})
      [:rect {:x 0 :y 0 :width 16 :height 16 :fill "hsl(200, 20%, 12%)"}]
      [:path {:d "M8 15A7 7 0 1 1 8 1a7 7 0 0 1 0 14zm0 1A8 8 0 1 0 8 0a8 8 0 0 0 0 16z"}]
      [:path {:d "M9.146 5.146a.5.5 0 0 1 .708 0l.646.647.646-.647a.5.5 0 0 1
                  .708.708l-.647.646.647.646a.5.5 0 0 1-.708.708l-.646-.647-.646.647a.5.5
                  0 1 1-.708-.708l.647-.646-.647-.646a.5.5 0 0 1 0-.708zm-5 0a.5.5 0 0 1
                  .708 0l.646.647.646-.647a.5.5 0 1 1 .708.708l-.647.646.647.646a.5.5
                  0 1 1-.708.708L5.5 7.207l-.646.647a.5.5 0 1 1-.708-.708l.647-.646-.647-.646a.5.5
                  0 0 1 0-.708zM10 11a2 2 0 1 1-4 0 2 2 0 0 1 4 0z"}]]
     (for [checksum images]
       [:pattern (merge attrs {:key checksum :id (str "token-stamp-" checksum)})
        [stamp {:checksum checksum}]])]))

(def tokens-query
  {:pull
   [:root/host?
    {:root/canvas
     [[:zoom/scale :default 1]
      [:grid/align :default false]
      {:canvas/tokens
       [:db/id
        [:pos/vec :default [0 0]]
        [:element/flags :default #{}]
        :canvas/_selected]}]}]})

(defn tokens [props]
  (let [[result dispatch] (use-query tokens-query)]
    (for [entity (-> result :root/canvas :canvas/tokens)
          :let [{id :db/id [ax ay] :pos/vec} entity]
          :when (and (not (:canvas/_selected entity))
                     (or (:root/host? result)
                         (visible? (:element/flags entity))))]
      [:> draggable
       {:key      id
        :position #js {:x ax :y ay}
        :scale    (-> result :root/canvas :zoom/scale)
        :on-start (fn [event] (.stopPropagation event))
        :on-stop
        (fn [event data]
          (.stopPropagation event)
          (let [bx (.-x data) by (.-y data)]
            (if (= (euclidean ax ay bx by) 0)
              (dispatch :element/select id (not (.-shiftKey event)))
              (let [align? (not= (.-metaKey event) (-> result :root/canvas :grid/align))]
                (dispatch :token/translate id [bx by] align?)))))}
       [:g.canvas-token
        [token {:id id}]]])))

(def selection-query
  {:pull
   [:root/host?
    {:root/canvas
     [[:grid/size :default 70]
      [:grid/align :default false]
      [:zoom/scale :default 1]
      {:canvas/selected
       [:db/id
        :element/type
        :element/flags
        [:pos/vec :default [0 0]]]}]}]})

(defn selection []
  (let [[result dispatch] (use-query selection-query)
        {:keys [root/host? root/canvas]} result
        {:keys [canvas/selected zoom/scale]} canvas
        idents (map :db/id selected)]
    (if (= (:element/type (first selected)) :token)
      [:> draggable
       {:position #js {:x 0 :y 0}
        :scale scale
        :on-start
        (fn [event]
          (.stopPropagation event))
        :on-stop
        (fn [event data]
          (let [ox (.-x data) oy (.-y data)]
            (if (and (= ox 0) (= oy 0))
              (let [id (.. event -target (closest ".canvas-token[data-id]") -dataset -id)]
                (dispatch :element/select (js/Number id) false))
              (dispatch :token/translate-all idents [ox oy] (not= (.-metaKey event) (:grid/align canvas))))))}
       [:g.canvas-selected {:key idents}
        (for [entity selected :when (or host? (visible? (:element/flags entity)))]
          (let [id (:db/id entity)]
            [:g.canvas-token
             {:key id :data-id id :transform (xf :translate (:pos/vec entity))}
             [token {:id id}]]))]])))

(defn drawable [{:keys [on-release]} render-fn]
  (let [points (uix/state nil)]
    [:<>
     [:> draggable
      {:position #js {:x 0 :y 0}
       :on-start
       (fn [event data]
         (.stopPropagation event)
         (let [x (.-clientX event) y (.-clientY event)]
           (reset! points [x y x y])))
       :on-drag
       (fn [event data]
         (swap! points
                (fn [[ax ay bx by]]
                  [ax ay (+ ax (.-x data)) (+ ay (.-y data))])))
       :on-stop
       (fn [event]
         (let [vecs @points]
           (reset! points nil)
           (on-release vecs event)))}
      [:rect
       {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"
        :style {:will-change "transform"}}]]
     (when (seq @points)
       (render-fn @points))]))

(def select-query
  {:pull
   [:bounds/self
    {:root/canvas
     [[:zoom/scale :default 1]
      [:pos/vec :default [0 0]]]}]})

(defn select [{:keys [node]}]
  (let [[result dispatch] (use-query select-query)
        {[ox oy _ _] :bounds/self
         {scale :zoom/scale [cx cy] :pos/vec} :root/canvas} result]
    [drawable
     {:on-release
      (fn [vecs]
        (let [[ax ay bx by] (vec/+ (vec/s (/ scale) vecs) (vec/s -1 [ox oy ox oy]))]
          (dispatch :selection/from-rect [(- ax cx) (- ay cy) (- bx cx) (- by cy)])))}
     (fn [vecs]
       (let [[ax ay bx by] (vec/- vecs [ox oy ox oy])]
         (create-portal
          [:path {:d (string/join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])}]
          @node)))]))

(def draw-query
  {:pull
   [:bounds/self
    {:root/canvas
     [[:grid/size :default 70]
      [:grid/align :default false]
      [:zoom/scale :default 1]
      [:pos/vec :default [0 0]]]}]})

(defmulti draw :mode)

(defmethod draw :grid []
  (let [[result dispatch] (use-query draw-query)
        {[ox oy] :bounds/self
         {size :grid/size
          scale :zoom/scale} :root/canvas} result]
    [drawable
     {:on-release
      (fn [[ax ay bx by]]
        (let [size (js/Math.abs (min (- bx ax) (- by ay)))]
          (when (> size 0)
            (dispatch :grid/draw ax ay size))))}
     (fn [xs]
       (let [[ax ay bx by] (vec/- xs [ox oy ox oy])
             size (min (- bx ax) (- by ay))]
         [:g
          [:path {:d (string/join " " ["M" ax ay "h" size "v" size "H" ax "Z"])}]
          [text {:x bx :y ay :fill "white"}
           (-> (/ size scale)
               (js/Math.abs)
               (js/Math.round)
               (str "px"))]]))]))

(defmethod draw :ruler []
  (let [[result dispatch] (use-query draw-query)
        {[ox oy _ _] :bounds/self
         {size  :grid/size
          scale :zoom/scale} :root/canvas} result]
    [drawable
     {:on-release identity}
     (fn [xs]
       (let [[ax ay bx by] (vec/- xs [ox oy ox oy])]
         [:g
          [:line {:x1 ax :y1 ay :x2 bx :y2 by}]
          [text {:x (- bx 48) :y (- by 8) :fill "white"}
           (-> (chebyshev ax ay bx by)
               (px->ft (* size scale))
               (str "ft."))]]))]))

(defmethod draw :circle []
  (let [[result dispatch] (use-query draw-query)
        {[ox oy] :bounds/self
         {[cx cy] :pos/vec
          size    :grid/size
          scale   :zoom/scale} :root/canvas} result]
    [drawable
     {:on-release
      (fn [xs]
        (->> (vec/+ (vec/s -1 [ox oy ox oy]) xs)
             (vec/s (/ scale))
             (vec/+ (vec/s -1 [cx cy cx cy]))
             (dispatch :shape/create :circle)))}
     (fn [xs]
       (let [[ax ay bx by] (vec/- xs [ox oy ox oy])
             radius (chebyshev ax ay bx by)]
         [:g
          [:circle {:cx ax :cy ay :r radius}]
          [text {:x ax :y ay :fill "white"}
           (-> radius (px->ft (* size scale)) (str "ft. radius"))]]))]))

(defmethod draw :rect []
  (let [[result dispatch] (use-query draw-query)
        {[ox oy] :bounds/self
         {[cx cy] :pos/vec
          size    :grid/size
          scale   :zoom/scale} :root/canvas} result]
    [drawable
     {:on-release
      (fn [xs]
        (->> (vec/- xs [ox oy ox oy])
             (vec/s (/ scale))
             (vec/+ (vec/s -1 [cx cy cx cy]))
             (vec/normalize)
             (dispatch :shape/create :rect)))}
     (fn [xs]
       (let [[ax ay bx by] (vec/- xs [ox oy ox oy])]
         [:g
          [:path {:d (string/join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])}]
          [text {:x (+ ax 8) :y (- ay 8) :fill "white"}
           (let [w (px->ft (js/Math.abs (- bx ax)) (* size scale))
                 h (px->ft (js/Math.abs (- by ay)) (* size scale))]
             (str w "ft. x " h "ft."))]]))]))

(defmethod draw :line []
  (let [[result dispatch] (use-query draw-query)
        {[ox oy] :bounds/self
         {[cx cy] :pos/vec
          size    :grid/size
          scale   :zoom/scale} :root/canvas} result]
    [drawable
     {:on-release
      (fn [xs]
        (->> (vec/- xs [ox oy ox oy])
             (vec/s (/ scale))
             (vec/+ (vec/s -1 [cx cy cx cy]))
             (dispatch :shape/create :line)))}
     (fn [xs]
       (let [[ax ay bx by] (vec/- xs [ox oy ox oy])]
         [:g [:line {:x1 ax :y1 ay :x2 bx :y2 by}]
          [text {:x (+ ax 8) :y (- ay 8) :fill "white"}
           (-> (chebyshev ax ay bx by)
               (px->ft (* size scale))
               (str "ft."))]]))]))

(defmethod draw :cone []
  (let [[result dispatch] (use-query draw-query)
        {[ox oy] :bounds/self
         {[cx cy] :pos/vec
          size    :grid/size
          scale   :zoom/scale} :root/canvas} result]
    [drawable
     {:on-release
      (fn [xs]
        (->> (vec/- xs [ox oy ox oy])
             (vec/s (/ scale))
             (vec/+ (vec/s -1 [cx cy cx cy]))
             (dispatch :shape/create :cone)))}
     (fn [xs]
       (let [[ax ay bx by] (vec/- xs [ox oy ox oy])]
         [:g
          [:polygon {:points (string/join " " (triangle ax ay bx by))}]
          [text {:x (+ bx 16) :y (+ by 16) :fill "white"}
           (-> (euclidean ax ay bx by)
               (px->ft (* size scale))
               (str "ft."))]]))]))

(defmethod draw :default [] nil)

(defn bounds []
  (let [[result] (use-query {:pull [:bounds/host :bounds/guest]})
        {[_ _ hw hh] :bounds/host
         [_ _ gw gh] :bounds/guest} result
        [ox oy] [(/ (- hw gw) 2) (/ (- hh gh) 2)]]
    [:g.canvas-bounds {:transform (xf :translate [ox oy])}
     [:rect {:x 0 :y 0 :width gw :height gh :rx 8}]]))

(def canvas-query
  {:pull
   [:root/privileged?
    :root/host?
    :bounds/host
    :bounds/guest
    {:root/canvas
     [[:pos/vec :default [0 0]]
      [:canvas/mode :default :select]
      [:canvas/theme :default :light]
      :canvas/modifier
      [:zoom/scale :default 1]]}]})

(defn canvas [props]
  (let [select-node       (uix/ref)
        [result dispatch] (use-query canvas-query)
        {priv? :root/privileged?
         host? :root/host?
         [_ _ hw hh] :bounds/host
         [_ _ gw gh] :bounds/guest
         {scale :zoom/scale
          mode  :canvas/mode
          theme :canvas/theme
          modif :canvas/modifier
          coord :pos/vec} :root/canvas} result
        coord (if host? coord
                  (->> [(- hw gw) (- hh gh)]
                       (mapv (partial max 0))
                       (vec/s (/ -1 2 scale))
                       (vec/+ coord)))]
    [:svg.canvas {:class (str "theme--" (name theme))}
     [:> draggable
      {:position #js {:x 0 :y 0}
       :on-stop
       (fn [event data]
         (let [ox (.-x data) oy (.-y data)]
           (if (and (= ox 0) (= oy 0))
             (dispatch :selection/clear)
             (dispatch :camera/translate (vec/+ coord (vec/s (/ scale) [ox oy]))))))}
      [:g {:style {:will-change "transform"}}
       [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]
       (if (and (= mode :select) (= modif :shift))
         [select {:node select-node}])
       [:g.canvas-board
        {:transform (xf :scale scale :translate coord)}
        [stamps]
        [board]
        [grid]
        [shapes]
        [tokens]
        [selection]
        [mask]]
       (if (and (= mode :select) (= modif :shift))
         [:g {:ref select-node :class "canvas-drawable canvas-drawable-select"}])
       (if (not= mode :select)
         [:g {:class (str "canvas-drawable canvas-drawable-" (name mode))}
          [draw {:mode mode}]])]]
     (if priv?
       [bounds])]))
