(ns ogres.app.render.canvas
  (:require [clojure.set :refer [difference]]
            [clojure.string :refer [join]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.geom :refer [bounding-box chebyshev euclidean triangle]]
            [ogres.app.hooks :refer [create-portal subscribe! use-cursor use-dispatch use-image use-portal use-publish use-query]]
            [ogres.app.render :refer [icon]]
            [ogres.app.render.draw :refer [draw]]
            [ogres.app.render.forms :refer [token-context-menu shape-context-menu]]
            [ogres.app.render.pattern :refer [pattern]]
            [react-draggable]
            [uix.core.alpha :as uix]))

(def ^:private draw-modes
  #{:grid :ruler :circle :rect :cone :line :poly :mask})

(def ^:private color-matrix
  {:none     [1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0]
   :dusk     [0.3 0.3 0.0 0.0 0.0 0.0 0.3 0.3 0.0 0.0 0.0 0.0 0.8 0.0 0.0 0.0 0.0 0.0 1.0 0.0]
   :midnight [0.0 0.0 0.0 0.0 0.0 0.0 0.1 0.0 0.0 0.0 0.1 0.1 0.1 0.0 0.0 0.0 0.0 0.0 1.0 0.0]})

(def ^:private condition->icon
  {:blinded       "eye-slash-fill"
   :charmed       "arrow-through-heart-fill"
   :exhausted     "moon-stars-fill"
   :frightened    "black-cat"
   :grappled      "fist"
   :incapacitated "emoji-dizzy"
   :invisible     "incognito"
   :player        "people-fill"
   :prone         "falling"
   :unconscious   "skull"})

(defn- key-by
  "Returns a map of the given `coll` whose keys are the result of calling `f`
   with each element in the collection and whose values are the element
   itself."
  [f coll]
  (into {} (map (juxt f identity) coll)))

(defn- separate
  "Split coll into two sequences, one that matches pred and one that doesn't."
  [pred coll]
  (let [pcoll (map (juxt identity pred) coll)]
    (vec (for [f [filter remove]]
           (map first (f second pcoll))))))

(defn- stop-propagation [event]
  (.stopPropagation event))

(defn- visible? [flags]
  (or (contains? flags :player)
      (not (contains? flags :hidden))))

(defn- label [{:keys [token/label initiative/suffix]}]
  (cond-> ""
    (string? label) (str label)
    (number? suffix) (str " " (char (+ suffix 64)))))

(def ^:private query-scene
  [{:local/window
    [{:window/canvas
      [[:canvas/grid-size :default 70]
       [:canvas/timeofday :default :none]
       {:canvas/image [:image/checksum]}]}]}])

(defn- render-scene []
  (let [result (use-query query-scene)
        {{{size        :canvas/grid-size
           time-of-day :canvas/timeofday
           {checksum   :image/checksum} :canvas/image}
          :window/canvas}
         :local/window} result
        url (use-image checksum)]
    [:g.canvas-image {:transform (str "scale(" (/ grid-size size) ")")}
     [:defs {:key time-of-day}
      [:filter {:id "atmosphere"}
       [:feColorMatrix {:type "matrix" :values (join " " (color-matrix time-of-day))}]]]
     [:image {:x 0 :y 0 :href url :style {:filter "url(#atmosphere)"}}]]))

(def ^:private query-mask-vis
  [:local/type
   {:local/window
    [{:window/canvas
      [[:canvas/grid-size :default 70]
       [:canvas/lighting :default :revealed]
       {:canvas/tokens
        [:db/key
         [:token/flags :default #{}]
         [:token/light :default 15]
         [:token/vec :default [0 0]]]}
       {:canvas/image [:image/checksum :image/width :image/height]}]}]}])

(defn- render-mask-vis []
  (let [result (use-query query-mask-vis)
        {type :local/type
         {{visibility :canvas/lighting
           size       :canvas/grid-size
           tokens     :canvas/tokens
           {checksum  :image/checksum
            width     :image/width
            height    :image/height}
           :canvas/image}
          :window/canvas}
         :local/window} result
        width  (* width  (/ grid-size size))
        height (* height (/ grid-size size))]
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
         (for [{key :db/key flags :token/flags [x y] :token/vec radius :token/light} tokens
               :when (and (> radius 0) (or (= type :host) (visible? flags)))
               :let  [radius (-> grid-size (* radius) (/ 5) (+ grid-size))]]
           [:circle {:key key :cx x :cy y :r radius :fill "url(#mask-gradient)"}])]]
       [:rect.canvas-mask-background
        {:x 0 :y 0 :width width :height height :mask "url(#light-mask)"}]
       (if (= visibility :hidden)
         [:rect.canvas-mask-pattern
          {:x 0 :y 0 :width width :height height
           :fill "url(#mask-pattern)" :mask "url(#light-mask)"}])])))

(def ^:private query-mask-fog
  [:local/type
   {:local/window
    [[:window/draw-mode :default :select]
     {:window/canvas
      [[:canvas/grid-size :default 70]
       [:mask/filled? :default false]
       {:canvas/image [:image/width :image/height]}
       {:canvas/masks [:db/key :mask/vecs :mask/enabled?]}]}]}])

(defn- render-mask-fog []
  (let [dispatch (use-dispatch)
        result   (use-query query-mask-fog)
        {type :local/type
         {mode :window/draw-mode
          {size    :canvas/grid-size
           filled? :mask/filled?
           masks   :canvas/masks
           {width  :image/width
            height :image/height}
           :canvas/image}
          :window/canvas}
         :local/window} result
        modes #{:mask :mask-toggle :mask-remove}
        width  (* width  (/ grid-size size))
        height (* height (/ grid-size size))]
    [:g.canvas-mask
     [:defs
      [pattern {:id "mask-pattern" :name :lines}]
      [:mask {:id "canvas-mask"}
       (if filled?
         [:rect {:x 0 :y 0 :width width :height height :fill "white"}])
       (for [{key :db/key enabled? :mask/enabled? xs :mask/vecs} masks]
         [:polygon {:key key :points (join " " xs) :fill (if enabled? "white" "black")}])]]
     [:rect.canvas-mask-background {:x 0 :y 0 :width width :height height :mask "url(#canvas-mask)"}]
     [:rect.canvas-mask-pattern {:x 0 :y 0 :width width :height height :fill "url(#mask-pattern)" :mask "url(#canvas-mask)"}]
     (if (and (= type :host) (contains? modes mode))
       (for [{key :db/key xs :mask/vecs enabled? :mask/enabled?} masks]
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

(def ^:private query-grid
  [[:bounds/self :default [0 0 0 0]]
   {:local/window
    [[:window/vec :default [0 0]]
     [:window/scale :default 1]
     [:window/draw-mode :default :select]
     {:window/canvas
      [[:canvas/show-grid :default true]]}]}])

(defn- render-grid []
  (let [data (use-query query-grid)
        {[_ _ w h] :bounds/self
         {[cx cy] :window/vec
          mode    :window/draw-mode
          scale   :window/scale
          canvas  :window/canvas} :local/window} data]
    (if (or (:canvas/show-grid canvas) (= mode :grid))
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
          [:pattern {:id "grid" :width grid-size :height grid-size :patternUnits "userSpaceOnUse"}
           [:path
            {:d (join " " ["M" 0 0 "H" grid-size "V" grid-size])}]]]
         [:path {:d (join " " ["M" sx sy "H" ax "V" ay "H" bx "Z"]) :fill "url(#grid)"}]]))))

(defn- poly-xf [x y]
  (comp (partition-all 2)
        (mapcat (fn [[ax ay]] [(- ax x) (- ay y)]))))

(defmulti ^:private shape
  (fn [props] (:shape/kind (:element props))))

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

(defmethod shape :poly [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [ax ay] (into [] (take 2) vecs)
        pairs   (into [] (poly-xf ax ay) vecs)]
    [:polygon (assoc attrs :points (join " " pairs) :fill-opacity opacity :stroke color)]))

(def ^:private query-shapes
  [:local/type
   {:local/window
    [:db/key
     [:window/scale :default 1]
     {:window/canvas
      [[:canvas/snap-grid :default false]
       {:canvas/shapes
        [:db/key
         :shape/kind
         :shape/vecs
         [:shape/color :default "#f44336"]
         [:shape/opacity :default 0.25]
         [:shape/pattern :default :solid]
         {:window/_selected [:db/key]}]}]}]}])

(defn- render-shapes []
  (let [dispatch (use-dispatch)
        result   (use-query query-shapes)
        {type    :local/type
         window  :local/window
         {scale  :window/scale
          canvas :window/canvas
          {snap-grid :canvas/snap-grid} :window/canvas}
         :local/window} result
        participant? (or (= type :host) (= type :conn))]
    (for [entity (:canvas/shapes canvas)
          :let [key (:db/key entity)
                {:shape/keys [kind color vecs]} entity
                selecting (into #{} (map :db/key) (:window/_selected entity))
                selected? (contains? selecting (:db/key window))
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

(defn- render-stamp [{:keys [checksum]}]
  (let [url (use-image checksum)]
    [:image {:href url :width 1 :height 1 :preserveAspectRatio "xMidYMin slice"}]))

(def ^:private query-stamps
  [{:local/window
    [{:window/canvas
      [{:canvas/tokens
        [{:token/image
          [:image/checksum]}]}]}]}])

(defn- render-stamps []
  (let [result    (use-query query-stamps)
        tokens    (-> result :local/window :window/canvas :canvas/tokens)
        checksums (into #{} (comp (map :token/image) (map :image/checksum)) tokens)
        attrs     {:width "100%" :height "100%" :patternContentUnits "objectBoundingBox"}]
    [:defs
     [:pattern (merge attrs {:id "token-stamp-default" :viewBox "-2 -2 16 16" :fill "#f2f2eb"})
      [:rect {:x -2 :y -2 :width 16 :height 16 :fill "hsl(208deg 21% 20%)"}]
      [icon {:name "dnd" :size 12}]]
     [:pattern (merge attrs {:id "token-stamp-deceased" :viewBox "-2 -2 16 16" :fill "#f2f2eb"})
      [:rect {:x -2 :y -2 :width 16 :height 16 :fill "hsl(208deg 21% 20%)"}]
      [icon {:name "skull" :size 12}]]
     (for [checksum checksums]
       [:pattern (merge attrs {:key checksum :id (str "token-stamp-" checksum)})
        [render-stamp {:checksum checksum}]])]))

(defn- render-token [data]
  [:<>
   (let [radius (* grid-size (/ (:aura/radius data) 5))]
     (if (> radius 0)
       [:circle.canvas-token-aura {:cx 0 :cy 0 :r (+ radius (/ grid-size 2))}]))
   (let [radii (- (/ grid-size 2) 2)
         flags (:token/flags data)
         scale (/ (:token/size data) 5)
         hashs (:image/checksum (:token/image data))
         pttrn (cond (flags :unconscious) (str "token-stamp-deceased")
                     (string? hashs)      (str "token-stamp-" hashs)
                     :else                (str "token-stamp-default"))]
     [:g.canvas-token-container   {:transform (str "scale(" scale ")")}
      [:circle.canvas-token-ring  {:cx 0 :cy 0 :style {:r radii :fill "transparent"}}]
      [:circle.canvas-token-shape {:cx 0 :cy 0 :r radii :fill (str "url(#" pttrn ")")}]
      (for [[deg flag] (->> #{:player :hidden :unconscious}
                            (difference flags)
                            (take 4)
                            (mapv vector [115 70 -115 -70]))
            :let [rn (* (/ js/Math.PI 180) deg)
                  cx (* (js/Math.sin rn) radii)
                  cy (* (js/Math.cos rn) radii)]]
        [:g.canvas-token-flags {:key flag :transform (str "translate(" cx ", " cy ")")}
         [:circle {:cx 0 :cy 0 :r 12}]
         [:g {:transform (str "translate(" -8 ", " -8 ")")}
          [icon {:name (condition->icon flag) :size 16}]]])
      (if (seq (label data))
        [:foreignObject.context-menu-object {:x -200 :y (- radii 8) :width 400 :height 32}
         [:div.canvas-token-label
          [:span (label data)]]])])])

(defn- token-comparator [a b]
  (let [[ax ay] (:token/vec a)
        [bx by] (:token/vec b)]
    (compare [(:token/size b) by bx]
             [(:token/size a) ay ax])))

(def ^:private query-tokens
  [:local/type
   {:local/window
    [:db/key
     :window/selected
     [:window/scale :default 1]
     {:window/canvas
      [[:canvas/snap-grid :default false]
       {:canvas/tokens
        [:db/key
         [:initiative/suffix :default nil]
         [:token/vec :default [0 0]]
         [:token/flags :default #{}]
         [:token/label :default ""]
         [:token/size :default 5]
         [:token/light :default 15]
         [:aura/radius :default 0]
         {:token/image [:image/checksum]}
         {:canvas/_initiative [:db/key]}
         {:window/_selected [:db/key]}]}]}]}])

(defn- render-tokens []
  (let [dispatch (use-dispatch)
        result   (use-query query-tokens)
        {:local/keys [type window]} result
        {:window/keys [scale canvas]} window
        {:canvas/keys [snap-grid tokens]} canvas

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
             (separate (fn [token] ((into #{} (map :db/key) (:window/_selected token)) (:db/key window)))))]
    [:<>
     (for [data tokens :let [{key :db/key [ax ay] :token/vec} data]]
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
         [render-token data]]])
     (if (seq selected)
       (let [keys         (map :db/key selected)
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
            (for [data selected :let [{key :db/key [x y] :token/vec} data]]
              [:g.canvas-token
               {:key key :css (css data) :data-key key :transform (str "translate(" x "," y ")")}
               [render-token data]])
            (if (or (= type :host) (= type :conn))
              [:foreignObject
               {:x (- (+ (* ax scale) (/ (* (- bx ax) scale) 2)) (/ 400 2))
                :y (- (+ (* by scale) (* scale 56)) 24)
                :width 400 :height 400
                :transform (str "scale(" (/ scale) ")")
                :style {:pointer-events "none"}}
               [token-context-menu {:tokens selected :type type}]])]]]))]))

(defn- render-bounds []
  (let [result (use-query [:bounds/host :bounds/view])
        {[_ _ hw hh] :bounds/host
         [_ _ vw vh] :bounds/view} result
        [ox oy] [(/ (- hw vw) 2) (/ (- hh vh) 2)]]
    [:g.canvas-bounds {:transform (str "translate(" ox " , " oy ")")}
     [:rect {:x 0 :y 0 :width vw :height vh :rx 8}]]))

(defn- render-cursor [{[x y] :coord color :color}]
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
       [icon {:name "cursor-fill" :size 32}]])))

(def ^:private query-cursors
  [[:session/share-cursors :default true]
   {:session/conns
    [:db/key
     [:session/share-cursor :default true]
     [:local/color :default "royalBlue"]]}])

(defn- render-cursors []
  (let [coords (uix/state {})
        result (use-query query-cursors [:db/ident :session])
        {conns :session/conns
         share :session/share-cursors} result
        conns  (key-by :db/key conns)]
    (subscribe!
     (fn [{[uuid x y] :args}]
       (if share (swap! coords assoc uuid [x y]))) :cursor/moved [share])
    (if share
      [:g.canvas-cursors
       (for [[uuid [x y]] @coords
             :let  [local (conns uuid)
                    color (:local/color local)
                    share (:session/share-cursor local)]
             :when (and local share)]
         ^{:key uuid} [render-cursor {:coord [x y] :color color}])])))

(def ^:private query-canvas
  [:local/type
   [:local/privileged? :default false]
   [:bounds/self :default [0 0 0 0]]
   [:bounds/host :default [0 0 0 0]]
   [:bounds/view :default [0 0 0 0]]
   {:local/window
    [:db/key
     :window/modifier
     [:window/vec :default [0 0]]
     [:window/scale :default 1]
     [:window/draw-mode :default :select]
     {:window/canvas
      [[:canvas/dark-mode :default false]]}]}])

(defn render-canvas []
  (let [dispatch (use-dispatch)
        publish  (use-publish)
        result   (use-query query-canvas)
        {type        :local/type
         priv?       :local/privileged?
         [_ _ hw hh] :bounds/host
         [_ _ vw vh] :bounds/view
         [sx sy _ _] :bounds/self
         {key     :db/key
          scale   :window/scale
          mode    :window/draw-mode
          modif   :window/modifier
          [cx cy] :window/vec
          {dark-mode :canvas/dark-mode}
          :window/canvas}
         :local/window} result
        cx (if (= type :view) (->> (- hw vw) (max 0) (* (/ -1 2 scale)) (+ cx)) cx)
        cy (if (= type :view) (->> (- hh vh) (max 0) (* (/ -1 2 scale)) (+ cy)) cy)
        on-translate
        (uix/callback
         (fn [_ data]
           (let [ox (.-x data)
                 oy (.-y data)]
             (if (and (= ox 0) (= oy 0))
               (dispatch :selection/clear)
               (let [tx (+ cx (* ox (/ scale)))
                     ty (+ cy (* oy (/ scale)))]
                 (dispatch :window/translate tx ty)))))
         [dispatch cx cy scale])
        on-cursor-move
        (uix/callback
         (fn [event]
           (let [transform (.. event -currentTarget (getAttribute "transform"))]
             (if (= transform "translate(0,0)")
               (let [x (- (/ (- (.-clientX event) sx) scale) cx)
                     y (- (/ (- (.-clientY event) sy) scale) cy)]
                 (publish {:topic :cursor/move :args [x y]})))))
         [sx sy cx cy scale])]
    [:svg.canvas {:key key :css {:theme--light (not dark-mode) :theme--dark dark-mode :is-host (= type :host) :is-priv priv?}}
     [:> react-draggable {:position #js {:x 0 :y 0} :on-stop on-translate}
      [:g {:style {:will-change "transform"} :on-mouse-move on-cursor-move}
       [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]
       (if (and (= mode :select) (= modif :shift))
         [draw {:mode :select}])
       [:g.canvas-board {:transform (str "scale(" scale ") translate(" cx ", " cy ")")}
        [render-stamps]
        [render-scene]
        [render-grid]
        [render-shapes]
        [render-tokens]
        [render-mask-vis]
        [render-mask-fog]
        [render-cursors]
        [create-portal
         (fn [ref]
           [:g {:ref ref :style {:outline "none"}}]) :selected]]]]
     [create-portal
      (fn [ref]
        [:g {:ref ref :style {:outline "none"} :class "canvas-drawable canvas-drawable-select"}]) :multiselect]
     (if (contains? draw-modes mode)
       [:g {:class (str "canvas-drawable canvas-drawable-" (name mode))}
        ^{:key mode} [draw {:mode mode :node nil}]])
     (if priv? [render-bounds])]))
