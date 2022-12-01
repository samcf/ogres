(ns ogres.app.render.canvas
  (:require [clojure.set :refer [difference]]
            [clojure.string :refer [join]]
            [ogres.app.geom :refer [bounding-box chebyshev euclidean triangle]]
            [ogres.app.hooks :refer [create-portal subscribe! use-cursor use-dispatch use-image use-portal use-publish use-query]]
            [ogres.app.render :refer [icon]]
            [ogres.app.render.draw :refer [draw]]
            [ogres.app.render.forms :refer [token-context-menu shape-context-menu]]
            [ogres.app.render.pattern :refer [pattern]]
            [react-draggable]
            [uix.core.alpha :as uix]))

(def draw-modes
  #{:grid :ruler :circle :rect :cone :line :poly :mask})

(def atmosphere
  {:none     [1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0]
   :dusk     [0.3 0.3 0.0 0.0 0.0 0.0 0.3 0.3 0.0 0.0 0.0 0.0 0.8 0.0 0.0 0.0 0.0 0.0 1.0 0.0]
   :midnight [0.0 0.0 0.0 0.0 0.0 0.0 0.1 0.0 0.0 0.0 0.1 0.1 0.1 0.0 0.0 0.0 0.0 0.0 1.0 0.0]})

(def conditions
  [[:player "people-fill"]
   [:blinded "eye-slash-fill"]
   [:charmed "arrow-through-heart-fill"]
   [:exhausted "moon-stars-fill"]
   [:invisible "incognito"]
   [:grappled "fist"]
   [:prone "falling"]
   [:frightened "black-cat"]
   [:incapacitated "emoji-dizzy"]
   [:unconscious "skull"]])

(defn key-by
  "Returns a map of the given `coll` whose keys are the result of calling `f`
   with each element in the collection and whose values are the element
   itself."
  [f coll]
  (into {} (map (juxt f identity) coll)))

(defn separate
  "Split coll into two sequences, one that matches pred and one that doesn't."
  [pred coll]
  (let [pcoll (map (juxt identity pred) coll)]
    (vec (for [f [filter remove]]
           (map first (f second pcoll))))))

(defn stop-propagation [event]
  (.stopPropagation event))

(defn ft->px [ft size] (* (/ ft 5) size))

(defn visible? [flags]
  (or (contains? flags :player)
      (not (contains? flags :hidden))))

(defn label [{:keys [token/label initiative/suffix]}]
  (cond-> ""
    (string? label) (str label)
    (number? suffix) (str " " (char (+ suffix 64)))))

(def scene-query
  [{:local/window
    [{:window/canvas
      [[:canvas/color :default :none]
       {:canvas/image
        [:image/checksum]}]}]}])

(defn scene []
  (let [result (use-query scene-query)
        {{{color :canvas/color {checksum :image/checksum}
           :canvas/image}
          :window/canvas}
         :local/window} result
        url (use-image checksum)]
    [:g.canvas-image
     [:defs {:key color}
      [:filter {:id "atmosphere"}
       [:feColorMatrix {:type "matrix" :values (join " " (atmosphere color))}]]]
     [:image {:x 0 :y 0 :href url :style {:filter "url(#atmosphere)"}}]]))

(def light-mask-query
  [:local/type
   {:local/window
    [{:window/canvas
      [[:grid/size :default 70]
       [:canvas/visibility :default :revealed]
       {:canvas/tokens
        [:entity/key
         [:token/flags :default #{}]
         [:token/light :default 15]
         [:token/vec :default [0 0]]]}
       {:canvas/image [:image/checksum :image/width :image/height]}]}]}])

(defn light-mask []
  (let [result (use-query light-mask-query)
        {type :local/type
         {{visibility :canvas/visibility
           size       :grid/size
           tokens     :canvas/tokens
           {checksum :image/checksum
            width    :image/width
            height   :image/height}
           :canvas/image}
          :window/canvas}
         :local/window} result]
    (if (and checksum (not= visibility :revealed))
      [:g.canvas-mask {:css {:is-dimmed (= visibility :dimmed)}}
       [:defs
        [pattern {:id "mask-pattern" :name :lines :color "black"}]
        [:radialGradient {:id "mask-gradient"}
         [:stop {:offset "0%" :stop-color "black" :stop-opacity "100%"}]
         [:stop {:offset "70%" :stop-color "black" :stop-opacity "100%"}]
         [:stop {:offset "100%" :stop-color "black" :stop-opacity "0%"}]]
        [:mask {:id "light-mask"}
         [:rect {:x 0 :y 0 :width width :height height :fill "white" :fill-opacity "100%"}]
         (for [{key :entity/key flags :token/flags [x y] :token/vec radius :token/light} tokens
               :when (and (> radius 0) (or (= type :host) (visible? flags)))]
           [:circle {:key key :cx x :cy y :r (+ (ft->px radius size) (/ size 2)) :fill "url(#mask-gradient)"}])]]
       [:rect.canvas-mask-background
        {:x 0 :y 0 :width width :height height :mask "url(#light-mask)"}]
       (if (= visibility :hidden)
         [:rect.canvas-mask-pattern
          {:x 0 :y 0 :width width :height height
           :fill "url(#mask-pattern)" :mask "url(#light-mask)"}])])))

(def canvas-mask-query
  [:local/type
   {:local/window
    [[:window/draw-mode :default :select]
     {:window/canvas
      [[:mask/filled? :default false]
       {:canvas/image [:image/width :image/height]}
       {:canvas/masks [:entity/key :mask/vecs :mask/enabled?]}]}]}])

(defn canvas-mask []
  (let [dispatch (use-dispatch)
        result   (use-query canvas-mask-query)
        {type :local/type
         {mode :window/draw-mode
          {filled? :mask/filled?
           masks   :canvas/masks
           {width  :image/width
            height :image/height}
           :canvas/image}
          :window/canvas}
         :local/window} result
        modes #{:mask :mask-toggle :mask-remove}]
    [:g.canvas-mask
     [:defs
      [pattern {:id "mask-pattern" :name :lines}]
      [:mask {:id "canvas-mask"}
       (if filled?
         [:rect {:x 0 :y 0 :width width :height height :fill "white"}])
       (for [{key :entity/key enabled? :mask/enabled? xs :mask/vecs} masks]
         [:polygon {:key key :points (join " " xs) :fill (if enabled? "white" "black")}])]]
     [:rect.canvas-mask-background {:x 0 :y 0 :width width :height height :mask "url(#canvas-mask)"}]
     [:rect.canvas-mask-pattern {:x 0 :y 0 :width width :height height :fill "url(#mask-pattern)" :mask "url(#canvas-mask)"}]
     (if (and (= type :host) (contains? modes mode))
       (for [{key :entity/key xs :mask/vecs enabled? :mask/enabled?} masks]
         [:polygon.canvas-mask-polygon
          {:key key
           :data-enabled enabled?
           :points (join " " xs)
           :on-mouse-down stop-propagation
           :on-click
           (fn []
             (case mode
               :mask-toggle (dispatch :mask/toggle key (not enabled?))
               :mask-remove (dispatch :mask/remove key)))}]))]))

(def grid-query
  [[:bounds/self :default [0 0 0 0]]
   {:local/window
    [[:window/vec :default [0 0]]
     [:window/scale :default 1]
     [:window/draw-mode :default :select]
     [:window/show-grid :default true]
     {:window/canvas
      [[:grid/size :default 70]]}]}])

(defn grid []
  (let [data (use-query grid-query)
        {[_ _ w h] :bounds/self
         {[cx cy] :window/vec
          mode    :window/draw-mode
          scale   :window/scale
          show    :window/show-grid
          {size :grid/size} :window/canvas} :local/window} data]
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
            {:d (join " " ["M" 0 0 "H" size "V" size])}]]]
         [:path {:d (join " " ["M" sx sy "H" ax "V" ay "H" bx "Z"]) :fill "url(#grid)"}]]))))

(defmulti shape (fn [props] (:shape/kind (:element props))))

(defmethod shape :circle [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
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
      {:d (join " " ["M" 0 0 "H" (- bx ax) "V" (- by ay) "H" 0 "Z"])
       :fill-opacity opacity :stroke color})]))

(defmethod shape :line [props]
  (let [{:keys [element]} props
        {:keys [shape/vecs shape/color]} element
        [ax ay bx by] vecs]
    [:line {:x1 0 :y1 0 :x2 (- bx ax) :y2 (- by ay) :stroke color :stroke-width 4 :stroke-linecap "round"}]))

(defmethod shape :cone [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [ax ay bx by] vecs]
    [:polygon
     (merge
      attrs
      {:points (join " " (triangle 0 0 (- bx ax) (- by ay)))
       :fill-opacity opacity :stroke color})]))

(defn poly-xf [x y]
  (comp (partition-all 2)
        (mapcat (fn [[ax ay]] [(- ax x) (- ay y)]))))

(defmethod shape :poly [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [ax ay] (into [] (take 2) vecs)
        pairs   (into [] (poly-xf ax ay) vecs)]
    [:polygon (assoc attrs :points (join " " pairs) :fill-opacity opacity :stroke color)]))

(def shapes-query
  [:local/type
   {:local/window
    [:entity/key
     [:window/scale :default 1]
     [:window/snap-grid :default false]
     {:window/canvas
      [{:canvas/shapes
        [:entity/key
         :shape/kind
         :shape/vecs
         [:shape/color :default "#f44336"]
         [:shape/opacity :default 0.25]
         [:shape/pattern :default :solid]
         {:window/_selected [:entity/key]}]}]}]}])

(defn shapes []
  (let [dispatch (use-dispatch)
        result   (use-query shapes-query)
        {:local/keys [type window]} result
        {:window/keys [scale snap-grid canvas]} window
        participant? (or (= type :host) (= type :conn))]
    (for [entity (:canvas/shapes canvas)
          :let [key (:entity/key entity)
                {:shape/keys [kind color vecs]} entity
                selecting (into #{} (map :entity/key) (:window/_selected entity))
                selected? (contains? selecting (:entity/key window))
                [ax ay] vecs]]
      ^{:key key}
      [use-portal {:label (if (and participant? selected?) :selected)}
       [:> react-draggable
        {:scale    scale
         :position #js {:x ax :y ay}
         :on-start stop-propagation
         :on-stop
         (fn [event data]
           (let [ox (.-x data) oy (.-y data)]
             (if (> (euclidean ax ay ox oy) 0)
               (dispatch :shape/translate key ox oy (not= snap-grid (.-metaKey event)))
               (dispatch :element/select key true))))}
        (let [id (random-uuid)]
          [:g
           {:css {:canvas-shape true :selected selected? (str "canvas-shape-" (name kind)) true}}
           [:defs [pattern {:id id :name (:shape/pattern entity) :color color}]]
           [shape {:element entity :attrs {:fill (str "url(#" id ")")}}]
           (if (and participant? selected?)
             [:foreignObject.context-menu-object {:x -200 :y 0 :width 400 :height 400}
              [shape-context-menu
               {:shape entity}]])])]])))

(defn stamp [{:keys [checksum]}]
  (let [url (use-image checksum)]
    [:image {:href url :width 1 :height 1 :preserveAspectRatio "xMidYMin slice"}]))

(def stamps-query
  [{:local/window
    [{:window/canvas
      [{:canvas/tokens
        [{:token/image
          [:image/checksum]}]}]}]}])

(defn stamps []
  (let [result    (use-query stamps-query)
        tokens    (-> result :local/window :window/canvas :canvas/tokens)
        checksums (into #{} (comp (map :token/image) (map :image/checksum)) tokens)
        attrs     {:width "100%" :height "100%" :patternContentUnits "objectBoundingBox"}]
    [:defs
     [:pattern (merge attrs {:id "token-stamp-default" :viewBox "0 0 16 16" :fill "#f2f2eb"})
      [:rect {:x 0 :y 0 :width 16 :height 16 :fill "hsl(200, 20%, 12%)"}]
      [:path {:d "M11 6a3 3 0 1 1-6 0 3 3 0 0 1 6 0z"}]
      [:path {:d "M0 8a8 8 0 1 1 16 0A8 8 0 0 1 0 8zm8-7a7 7 0 0 0-5.468 11.37C3.242 11.226 4.805 10 8 10s4.757 1.225 5.468 2.37A7 7 0 0 0 8 1z" :fill-rule "evenodd"}]]
     [:pattern (merge attrs {:id "token-stamp-deceased" :viewBox "-2 -2 16 16" :fill "#f2f2eb"})
      [:rect {:x -2 :y -2 :width 16 :height 16 :fill "hsl(200, 20%, 12%)"}]
      [icon {:name "skull" :size 12}]]
     (for [checksum checksums]
       [:pattern (merge attrs {:key checksum :id (str "token-stamp-" checksum)})
        [stamp {:checksum checksum}]])]))

(defn token [{:keys [data size]}]
  (let [radius (-> data :token/size (ft->px size) (/ 2) (- 2) (max 16))]
    [:<>
     (if (> (:aura/radius data) 0)
       [:circle.canvas-token-aura
        {:cx 0 :cy 0 :r (+ (ft->px (:aura/radius data) size) (/ size 2))}])
     [:circle.canvas-token-ring
      {:cx 0 :cy 0 :style {:r radius :fill "transparent"}}]
     (let [checksum (:image/checksum (:token/image data))
           pattern  (cond
                      ((:token/flags data) :unconscious) "token-stamp-deceased"
                      (string? checksum)   (str "token-stamp-" checksum)
                      :else                "token-stamp-default")]
       [:circle.canvas-token-shape
        {:cx 0 :cy 0 :r radius :fill (str "url(#" pattern ")")}])
     (let [icons (into {} conditions)
           degrs [125 95 65 -125 -95 -65]
           exclu #{:player :hidden :unconscious}]
       (for [[index flag]
             (into [] (comp (take 6) (map-indexed vector))
                   (difference (:token/flags data) exclu))]
         (let [rn (* (/ js/Math.PI 180) (nth degrs index 0))
               cx (* (js/Math.sin rn) radius)
               cy (* (js/Math.cos rn) radius)]
           [:g.canvas-token-flags {:key flag :transform (str "translate(" cx ", " cy ")")}
            [:circle {:cx 0 :cy 0 :r 8}]
            [:g {:transform (str "translate(" -6 ", " -6 ")")}
             [icon {:name (icons flag) :size 12}]]])))
     (let [token-label (label data)]
       (if (seq token-label)
         [:foreignObject.context-menu-object
          {:x -200 :y (- radius 8) :width 400 :height 32}
          [:div.canvas-token-label
           [:span token-label]]]))]))

(defn token-comparator [a b]
  (let [[ax ay] (:token/vec a)
        [bx by] (:token/vec b)]
    (compare [(:token/size b) by bx]
             [(:token/size a) ay ax])))

(def tokens-query
  [:local/type
   {:local/window
    [:entity/key
     :window/selected
     [:window/snap-grid :default false]
     [:window/scale :default 1]
     {:window/canvas
      [[:grid/size :default 70]
       {:canvas/tokens
        [:entity/key
         [:initiative/suffix :default nil]
         [:token/vec :default [0 0]]
         [:token/flags :default #{}]
         [:token/label :default ""]
         [:token/size :default 5]
         [:token/light :default 15]
         [:aura/radius :default 0]
         {:token/image [:image/checksum]}
         {:canvas/_initiative [:entity/key]}
         {:window/_selected [:entity/key]}]}]}]}])

(defn tokens []
  (let [dispatch (use-dispatch)
        result   (use-query tokens-query)
        {:local/keys [type window]} result
        {:window/keys [snap-grid scale canvas]} window
        {:canvas/keys [tokens]} canvas
        size (:grid/size canvas)

        flags-xf
        (comp (map name)
              (map (fn [s] (str "flag--" s)))
              (map (fn [s] [s true])))

        css
        (fn [token]
          (into {} flags-xf (:token/flags token)))

        [selected tokens]
        (->> tokens
             (filter (fn [token] (or (= type :host) (visible? (:token/flags token)))))
             (sort token-comparator)
             (separate (fn [token] ((into #{} (map :entity/key) (:window/_selected token)) (:entity/key window)))))]
    [:<>
     (for [data tokens :let [{key :entity/key [ax ay] :token/vec} data]]
       [:> react-draggable
        {:key      key
         :position #js {:x ax :y ay}
         :scale    scale
         :on-start stop-propagation
         :on-stop
         (fn [event data]
           (.stopPropagation event)
           (let [bx (.-x data) by (.-y data)]
             (if (= (euclidean ax ay bx by) 0)
               (dispatch :element/select key (not (.-shiftKey event)))
               (dispatch :token/translate key bx by (not= (.-metaKey event) snap-grid)))))}
        [:g.canvas-token {:css (css data)}
         [token {:data data :size size}]]])
     (if (seq selected)
       (let [keys         (map :entity/key selected)
             [ax _ bx by] (apply bounding-box (map :token/vec selected))]
         [use-portal {:label (if (or (= type :host) (= type :conn)) :selected)}
          [:> react-draggable
           {:position #js {:x 0 :y 0}
            :scale    scale
            :on-start stop-propagation
            :on-stop
            (fn [event data]
              (let [ox (.-x data) oy (.-y data)]
                (if (and (= ox 0) (= oy 0))
                  (let [key (.. event -target (closest ".canvas-token[data-key]") -dataset -key)]
                    (dispatch :element/select (uuid key) (not (.-shiftKey event))))
                  (dispatch :token/translate-all keys ox oy (not= (.-metaKey event) snap-grid)))))}
           [:g.canvas-selected {:key keys}
            (for [data selected :let [{key :entity/key [x y] :token/vec} data]]
              [:g.canvas-token
               {:key key :css (css data) :data-key key :transform (str "translate(" x "," y ")")}
               [token {:data data :size size}]])
            (if (or (= type :host) (= type :conn))
              [:foreignObject
               {:x (- (+ (* ax scale) (/ (* (- bx ax) scale) 2)) (/ 400 2))
                :y (- (+ (* by scale) (* scale 56)) 24)
                :width 400 :height 400
                :transform (str "scale(" (/ scale) ")")
                :style {:pointer-events "none"}}
               [token-context-menu {:tokens selected :type type}]])]]]))]))

(defn bounds []
  (let [result (use-query [:bounds/host :bounds/view])
        {[_ _ hw hh] :bounds/host
         [_ _ vw vh] :bounds/view} result
        [ox oy] [(/ (- hw vw) 2) (/ (- hh vh) 2)]]
    [:g.canvas-bounds {:transform (str "translate(" ox " , " oy ")")}
     [:rect {:x 0 :y 0 :width vw :height vh :rx 8}]]))

(defn cursor [{[x y] :coord color :color}]
  (let [position (uix/state nil)
        callback (uix/callback
                  (fn [point]
                    (let [x (aget point 0)
                          y (aget point 1)]
                      (reset! position [x y]))) [])
        update   (use-cursor callback)
        [ax ay]  @position]
    (uix/layout-effect!
     (fn [] (update x y)) [x y])
    (if (not (nil? @position))
      [:g.canvas-cursor {:transform (str "translate(" (- ax 4) ", " (- ay 4) ")") :color color}
       [icon {:name "cursor-fill-rotated" :size 32}]])))

(def cursors-query
  [{:root/session
    [{:session/conns [{:local/window [:entity/key]} :local/color :entity/key]}]}])

(defn cursors []
  (let [coords (uix/state {})
        result (use-query cursors-query [:db/ident :root])
        conns  (key-by :entity/key (:session/conns (:root/session result)))]
    (subscribe!
     (fn [{[uuid x y] :args}]
       (swap! coords assoc uuid [x y])) :cursor/moved [])
    [:g.canvas-cursors
     (for [[uuid [x y]] @coords
           :let  [color (get-in conns [uuid :local/color])]
           :when (contains? conns uuid)]
       ^{:key uuid} [cursor {:coord [x y] :color color}])]))

(def canvas-query
  [:local/type
   [:local/privileged? :default false]
   [:bounds/self :default [0 0 0 0]]
   [:bounds/host :default [0 0 0 0]]
   [:bounds/view :default [0 0 0 0]]
   {:local/window
    [:entity/key
     :window/modifier
     [:window/vec :default [0 0]]
     [:window/scale :default 1]
     [:window/draw-mode :default :select]
     {:window/canvas
      [[:canvas/theme :default :light]]}]}])

(defn canvas []
  (let [publish  (use-publish)
        dispatch (use-dispatch)
        result   (use-query canvas-query)
        {type        :local/type
         priv?       :local/privileged?
         [_ _ hw hh] :bounds/host
         [_ _ vw vh] :bounds/view
         [sx sy _ _] :bounds/self
         {key     :entity/key
          scale   :window/scale
          mode    :window/draw-mode
          modif   :window/modifier
          [cx cy] :window/vec
          {theme :canvas/theme} :window/canvas} :local/window} result
        cx (if (= type :view) (->> (- hw vw) (max 0) (* (/ -1 2 scale)) (+ cx)) cx)
        cy (if (= type :view) (->> (- hh vh) (max 0) (* (/ -1 2 scale)) (+ cy)) cy)]
    [:svg.canvas {:key key :css {(str "theme--" (name theme)) true :is-host (= type :host) :is-priv priv?}}
     [:> react-draggable
      {:position #js {:x 0 :y 0}
       :on-stop
       (fn [_ data]
         (let [ox (.-x data)
               oy (.-y data)]
           (if (and (= ox 0) (= oy 0))
             (dispatch :selection/clear)
             (let [tx (+ cx (* ox (/ scale)))
                   ty (+ cy (* oy (/ scale)))]
               (dispatch :window/translate tx ty)))))}
      [:g {:style         {:will-change "transform"}
           :on-mouse-move (fn [event]
                            (if (= (.. event -currentTarget (getAttribute "transform")) "translate(0,0)")
                              (let [x (- (/ (- (.-clientX event) sx) scale) cx)
                                    y (- (/ (- (.-clientY event) sy) scale) cy)]
                                (publish {:topic :cursor/move :args [x y]}))))}
       [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]
       (if (and (= mode :select) (= modif :shift))
         [draw {:mode :select}])
       [:g.canvas-board
        {:transform (str "scale(" scale ") translate(" cx ", " cy ")")}
        [stamps]
        [scene]
        [grid]
        [shapes]
        [tokens]
        [light-mask]
        [canvas-mask]
        [cursors]
        [create-portal (fn [ref] [:g {:ref ref :style {:outline "none"}}]) :selected]]]]
     [create-portal
      (fn [ref]
        [:g {:ref ref
             :style {:outline "none"}
             :class "canvas-drawable canvas-drawable-select"}]) :multiselect]
     (if (contains? draw-modes mode)
       [:g {:class (str "canvas-drawable canvas-drawable-" (name mode))}
        ^{:key mode} [draw {:mode mode :node nil}]])
     (if priv?
       [bounds])]))
