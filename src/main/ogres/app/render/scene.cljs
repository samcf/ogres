(ns ogres.app.render.scene
  (:require [clojure.set :refer [difference]]
            [clojure.string :refer [join]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.geom :refer [bounding-box chebyshev euclidean triangle]]
            [ogres.app.hooks :refer [create-portal use-subscribe use-cursor use-dispatch use-image use-portal use-publish use-query]]
            [ogres.app.render :refer [css icon]]
            [ogres.app.render.draw :refer [draw]]
            [ogres.app.render.forms :refer [token-context-menu shape-context-menu]]
            [ogres.app.render.pattern :refer [pattern]]
            [ogres.app.util :refer [key-by separate]]
            [react-draggable]
            [uix.core :as uix :refer [defui $ use-callback use-layout-effect use-state]]))

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

(defn ^:private stop-propagation [event]
  (.stopPropagation event))

(defn ^:private visible?
  [flags]
  (or (contains? flags :player)
      (not (contains? flags :hidden))))

(defn ^:private label
  [{:keys [token/label initiative/suffix]}]
  (cond-> ""
    (string? label) (str label)
    (number? suffix) (str " " (char (+ suffix 64)))))

(def ^:private query-scene-image
  [{:local/camera
    [{:camera/scene
      [[:scene/grid-size :default 70]
       [:scene/timeofday :default :none]
       {:scene/image [:image/checksum]}]}]}])

(defui ^:private render-scene-image []
  (let [result (use-query query-scene-image)
        {{{size        :scene/grid-size
           time-of-day :scene/timeofday
           {checksum   :image/checksum} :scene/image}
          :camera/scene}
         :local/camera} result
        url (use-image checksum)]
    ($ :g.scene-image {:transform (str "scale(" (/ grid-size size) ")")}
      ($ :defs {:key time-of-day}
        ($ :filter {:id "atmosphere"}
          ($ :feColorMatrix {:type "matrix" :values (join " " (color-matrix time-of-day))})))
      ($ :image {:x 0 :y 0 :href url :style {:filter "url(#atmosphere)"}}))))

(def ^:private query-mask-vis
  [:local/type
   {:local/camera
    [{:camera/scene
      [[:scene/grid-size :default 70]
       [:scene/lighting :default :revealed]
       {:scene/tokens
        [:db/key
         [:token/flags :default #{}]
         [:token/light :default 15]
         [:token/point :default [0 0]]]}
       {:scene/image [:image/checksum :image/width :image/height]}]}]}])

(defui ^:private render-mask-vis []
  (let [result (use-query query-mask-vis)
        {type :local/type
         {{visibility :scene/lighting
           size       :scene/grid-size
           tokens     :scene/tokens
           {checksum  :image/checksum
            width     :image/width
            height    :image/height}
           :scene/image}
          :camera/scene}
         :local/camera} result
        width  (* width  (/ grid-size size))
        height (* height (/ grid-size size))]
    (if (and checksum (not= visibility :revealed))
      ($ :g.scene-mask {:class (css {:is-dimmed (= visibility :dimmed)})}
        ($ :defs
          ($ pattern {:id "mask-pattern" :name :lines :color "black"})
          ($ :radialGradient {:id "mask-gradient"}
            ($ :stop {:offset "0%" :stop-color "black" :stop-opacity "100%"})
            ($ :stop {:offset "70%" :stop-color "black" :stop-opacity "100%"})
            ($ :stop {:offset "100%" :stop-color "black" :stop-opacity "0%"}))
          ($ :mask {:id "light-mask"}
            ($ :rect {:x 0 :y 0 :width width :height height :fill "white" :fill-opacity "100%"})
            (for [{key :db/key flags :token/flags [x y] :token/point radius :token/light} tokens
                  :when (and (> radius 0) (or (= type :host) (visible? flags)))
                  :let  [radius (-> grid-size (* radius) (/ 5) (+ grid-size))]]
              ($ :circle {:key key :cx x :cy y :r radius :fill "url(#mask-gradient)"}))))
        ($ :rect.scene-mask-background
          {:x 0 :y 0 :width width :height height :mask "url(#light-mask)"})
        (if (= visibility :hidden)
          ($ :rect.scene-mask-pattern
            {:x 0 :y 0 :width width :height height
             :fill "url(#mask-pattern)" :mask "url(#light-mask)"}))))))

(def ^:private query-mask-fog
  [:local/type
   {:local/camera
    [[:camera/draw-mode :default :select]
     {:camera/scene
      [[:scene/grid-size :default 70]
       [:mask/filled? :default false]
       {:scene/image [:image/width :image/height]}
       {:scene/masks [:db/key :mask/vecs :mask/enabled?]}]}]}])

(defui ^:private render-mask-fog []
  (let [dispatch (use-dispatch)
        result   (use-query query-mask-fog)
        {type :local/type
         {mode :camera/draw-mode
          {size    :scene/grid-size
           filled? :mask/filled?
           masks   :scene/masks
           {width  :image/width
            height :image/height}
           :scene/image}
          :camera/scene}
         :local/camera} result
        modes #{:mask :mask-toggle :mask-remove}
        width  (* width  (/ grid-size size))
        height (* height (/ grid-size size))]
    ($ :g.scene-mask
      ($ :defs
        ($ pattern {:id "mask-pattern" :name :lines})
        ($ :mask {:id "scene-mask"}
          (if filled?
            ($ :rect {:x 0 :y 0 :width width :height height :fill "white"}))
          (for [{key :db/key enabled? :mask/enabled? xs :mask/vecs} masks]
            ($ :polygon {:key key :points (join " " xs) :fill (if enabled? "white" "black")}))))
      ($ :rect.scene-mask-background {:x 0 :y 0 :width width :height height :mask "url(#scene-mask)"})
      ($ :rect.scene-mask-pattern {:x 0 :y 0 :width width :height height :fill "url(#mask-pattern)" :mask "url(#scene-mask)"})
      (if (and (= type :host) (contains? modes mode))
        (for [{key :db/key xs :mask/vecs enabled? :mask/enabled?} masks]
          ($ :polygon.scene-mask-polygon
            {:key key
             :data-enabled enabled?
             :points (join " " xs)
             :on-mouse-down stop-propagation
             :on-click
             (fn []
               (case mode
                 :mask-toggle (dispatch :mask/toggle key (not enabled?))
                 :mask-remove (dispatch :mask/remove key)))}))))))

(def ^:private query-grid
  [[:bounds/self :default [0 0 0 0]]
   {:local/camera
    [[:camera/point :default [0 0]]
     [:camera/scale :default 1]
     [:camera/draw-mode :default :select]
     {:camera/scene
      [[:scene/show-grid :default true]]}]}])

(defui ^:private render-grid []
  (let [data (use-query query-grid)
        {[_ _ w h] :bounds/self
         {[cx cy] :camera/point
          mode    :camera/draw-mode
          scale   :camera/scale
          scene   :camera/scene} :local/camera} data]
    (if (or (:scene/show-grid scene) (= mode :grid))
      (let [w (/ w scale)
            h (/ h scale)
            [sx sy ax ay bx]
            [(- (* w -3) cx)
             (- (* h -3) cy)
             (- (* w  3) cx)
             (- (* h  3) cy)
             (- (* w -3) cx)]]
        ($ :g {:class "scene-grid"}
          ($ :defs
            ($ :pattern {:id "grid" :width grid-size :height grid-size :patternUnits "userSpaceOnUse"}
              ($ :path {:d (join " " ["M" 0 0 "H" grid-size "V" grid-size])})))
          ($ :path {:d (join " " ["M" sx sy "H" ax "V" ay "H" bx "Z"]) :fill "url(#grid)"}))))))

(defn ^:private poly-xf [x y]
  (comp (partition-all 2)
        (mapcat (fn [[ax ay]] [(- ax x) (- ay y)]))))

(defui ^:private render-shape-circle
  [props]
  (let [{:keys [entity attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} entity
        [ax ay bx by] vecs]
    ($ :circle
      (->> {:cx 0 :cy 0 :r (chebyshev ax ay bx by) :fill-opacity opacity :stroke color}
           (merge attrs)))))

(defui ^:private render-shape-rect
  [props]
  (let [{:keys [entity attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} entity
        [ax ay bx by] vecs]
    ($ :path
      (->> {:d (join " " ["M" 0 0 "H" (- bx ax) "V" (- by ay) "H" 0 "Z"]) :fill-opacity opacity :stroke color}
           (merge attrs)))))

(defui ^:private render-shape-line
  [props]
  (let [{:keys [entity]} props
        {:keys [shape/vecs shape/color]} entity
        [ax ay bx by] vecs]
    ($ :line {:x1 0 :y1 0 :x2 (- bx ax) :y2 (- by ay) :stroke color :stroke-width 4 :stroke-linecap "round"})))

(defui ^:private render-shape-cone
  [props]
  (let [{:keys [entity attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} entity
        [ax ay bx by] vecs]
    ($ :polygon
      (->> {:points (join " " (triangle 0 0 (- bx ax) (- by ay))) :fill-opacity opacity :stroke color}
           (merge attrs)))))

(defui ^:private render-shape-poly
  [props]
  (let [{:keys [entity attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} entity
        [ax ay] (into [] (take 2) vecs)
        pairs   (into [] (poly-xf ax ay) vecs)]
    ($ :polygon (assoc attrs :points (join " " pairs) :fill-opacity opacity :stroke color))))

(defui ^:private render-shape
  [{:keys [entity] :as props}]
  (let [shape-fns {:circle render-shape-circle
                   :rect   render-shape-rect
                   :line   render-shape-line
                   :cone   render-shape-cone
                   :poly   render-shape-poly}]
    (if-let [component (shape-fns (:shape/kind entity))]
      ($ component props))))

(def ^:private query-shapes
  [:local/type
   {:local/camera
    [:db/key
     [:camera/scale :default 1]
     {:camera/scene
      [{:scene/shapes
        [:db/key
         :shape/kind
         :shape/vecs
         [:shape/color :default "#f44336"]
         [:shape/opacity :default 0.25]
         [:shape/pattern :default :solid]
         {:camera/_selected [:db/key]}]}]}]}])

(defui ^:private render-shapes []
  (let [dispatch (use-dispatch)
        result   (use-query query-shapes)
        {type    :local/type
         camera  :local/camera
         {scale  :camera/scale
          scene  :camera/scene}
         :local/camera} result
        participant? (or (= type :host) (= type :conn))]
    (for [entity (:scene/shapes scene)
          :let   [key (:db/key entity)
                  {:shape/keys [kind color vecs]} entity
                  selecting (into #{} (map :db/key) (:camera/_selected entity))
                  selected? (contains? selecting (:db/key camera))
                  [ax ay] vecs]]
      ($ use-portal {:key key :name (if (and participant? selected?) :selected)}
        (fn []
          ($ react-draggable
            {:scale    scale
             :position #js {:x ax :y ay}
             :on-start stop-propagation
             :on-stop
             (fn [_ data]
               (let [ox (.-x data) oy (.-y data)]
                 (if (> (euclidean ax ay ox oy) 0)
                   (dispatch :shape/translate key ox oy)
                   (dispatch :element/select key true))))}
            (let [id (random-uuid)]
              ($ :g {:class (css {:scene-shape true :selected selected? (str "scene-shape-" (name kind)) true})}
                ($ :defs
                  ($ pattern {:id id :name (:shape/pattern entity) :color color}))
                ($ render-shape {:entity entity :attrs {:fill (str "url(#" id ")")}})
                (if (and participant? selected?)
                  ($ :foreignObject.context-menu-object {:x -200 :y 0 :width 400 :height 400}
                    ($ shape-context-menu
                      {:shape entity})))))))))))

(defui ^:private render-token-face
  [{:keys [checksum]}]
  (let [url (use-image checksum)]
    ($ :image {:href url :width 1 :height 1 :preserveAspectRatio "xMidYMin slice"})))

(def ^:private query-token-faces
  [{:local/camera
    [{:camera/scene
      [{:scene/tokens
        [{:token/image
          [:image/checksum]}]}]}]}])

(defui ^:private render-token-faces []
  (let [result    (use-query query-token-faces)
        tokens    (-> result :local/camera :camera/scene :scene/tokens)
        checksums (into #{} (comp (map :token/image) (map :image/checksum)) tokens)
        attrs     {:width "100%" :height "100%" :patternContentUnits "objectBoundingBox"}]
    ($ :defs
      ($ :pattern (merge attrs {:id "token-face-default" :viewBox "-2 -2 16 16" :fill "#f2f2eb"})
        ($ :rect {:x -2 :y -2 :width 16 :height 16 :fill "hsl(208deg 21% 20%)"})
        ($ icon {:name "dnd" :size 12}))
      ($ :pattern (merge attrs {:id "token-face-deceased" :viewBox "-2 -2 16 16" :fill "#f2f2eb"})
        ($ :rect {:x -2 :y -2 :width 16 :height 16 :fill "hsl(208deg 21% 20%)"})
        ($ icon {:name "skull" :size 12}))
      (for [checksum checksums]
        ($ :pattern (merge attrs {:key checksum :id (str "token-face-" checksum)})
          ($ render-token-face {:checksum checksum}))))))

(defui ^:private render-token
  [{:keys [data]}]
  ($ :<>
    (let [radius (* grid-size (/ (:aura/radius data) 5))]
      (if (> radius 0)
        ($ :circle.scene-token-aura {:cx 0 :cy 0 :r (+ radius (/ grid-size 2))})))
    (let [radii (- (/ grid-size 2) 2)
          flags (:token/flags data)
          scale (/ (:token/size data) 5)
          hashs (:image/checksum (:token/image data))
          pttrn (cond (flags :unconscious) (str "token-face-deceased")
                      (string? hashs)      (str "token-face-" hashs)
                      :else                (str "token-face-default"))]
      ($ :g.scene-token-container {:transform (str "scale(" scale ")")}
        ($ :circle.scene-token-ring {:cx 0 :cy 0 :style {:r radii :fill "transparent"}})
        ($ :circle.scene-token-shape {:cx 0 :cy 0 :r radii :fill (str "url(#" pttrn ")")})
        (for [[deg flag] (->> #{:player :hidden :unconscious}
                              (difference flags)
                              (take 4)
                              (mapv vector [115 70 -115 -70]))
              :let [rn (* (/ js/Math.PI 180) deg)
                    cx (* (js/Math.sin rn) radii)
                    cy (* (js/Math.cos rn) radii)]]
          ($ :g.scene-token-flags {:key flag :transform (str "translate(" cx ", " cy ")")}
            ($ :circle {:cx 0 :cy 0 :r 12})
            ($ :g {:transform (str "translate(" -8 ", " -8 ")")}
              ($ icon {:name (condition->icon flag) :size 16}))))
        (if (seq (label data))
          ($ :foreignObject.context-menu-object {:x -200 :y (- radii 8) :width 400 :height 32}
            ($ :div.scene-token-label
              ($ :span (label data)))))))))

(defn ^:private token-comparator
  [a b]
  (let [[ax ay] (:token/point a)
        [bx by] (:token/point b)]
    (compare [(:token/size b) by bx]
             [(:token/size a) ay ax])))

(def ^:private query-tokens
  [:local/type
   {:local/camera
    [:db/key
     :camera/selected
     [:camera/scale :default 1]
     {:camera/scene
      [{:scene/tokens
        [:db/key
         [:initiative/suffix :default nil]
         [:token/point :default [0 0]]
         [:token/flags :default #{}]
         [:token/label :default ""]
         [:token/size :default 5]
         [:token/light :default 15]
         [:aura/radius :default 0]
         {:token/image [:image/checksum]}
         {:scene/_initiative [:db/key]}
         {:camera/_selected [:db/key]}]}]}]}])

(defui ^:private render-tokens []
  (let [dispatch (use-dispatch)
        result (use-query query-tokens)
        {type :local/type camera :local/camera} result
        {scale :camera/scale scene :camera/scene} camera

        flags-xf
        (comp (map name)
              (map (fn [s] (str "flag--" s)))
              (map (fn [s] [s true])))

        token-css
        (fn [token]
          (into {} flags-xf (:token/flags token)))

        [selected tokens]
        (->> (:scene/tokens scene)
             (filter (fn [token] (or (= type :host) (visible? (:token/flags token)))))
             (sort token-comparator)
             (separate (fn [token] ((into #{} (map :db/key) (:camera/_selected token)) (:db/key camera)))))]
    ($ :<>
      (for [data tokens :let [{key :db/key [ax ay] :token/point} data]]
        ($ react-draggable
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
                 (dispatch :token/translate key bx by))))}
          ($ :g.scene-token {:class (css (token-css data))}
            ($ render-token {:data data}))))
      (if (seq selected)
        (let [keys (map :db/key selected)
              [ax _ bx by] (apply bounding-box (map :token/point selected))]
          ($ use-portal {:name (if (or (= type :host) (= type :conn)) :selected)}
            (fn []
              ($ react-draggable
                {:position #js {:x 0 :y 0}
                 :scale    scale
                 :on-start stop-propagation
                 :on-stop
                 (fn [event data]
                   (let [ox (.-x data) oy (.-y data)]
                     (if (and (= ox 0) (= oy 0))
                       (let [key (.. event -target (closest ".scene-token[data-key]") -dataset -key)]
                         (dispatch :element/select (uuid key) (not (.-shiftKey event))))
                       (dispatch :token/translate-all keys ox oy))))}
                ($ :g.scene-selected {:key keys}
                  (for [data selected :let [{key :db/key [x y] :token/point} data]]
                    ($ :g.scene-token
                      {:key key :class (css (token-css data)) :data-key key :transform (str "translate(" x "," y ")")}
                      ($ render-token {:data data})))
                  (if (or (= type :host) (= type :conn))
                    ($ :foreignObject
                      {:x (- (+ (* ax scale) (/ (* (- bx ax) scale) 2)) (/ 400 2))
                       :y (- (+ (* by scale) (* scale 56)) 24)
                       :width 400 :height 400
                       :transform (str "scale(" (/ scale) ")")
                       :style {:pointer-events "none"}}
                      ($ token-context-menu {:tokens selected :type type}))))))))))))

(defui ^:private render-bounds []
  (let [result (use-query [:bounds/host :bounds/view])
        {[_ _ hw hh] :bounds/host
         [_ _ vw vh] :bounds/view} result
        [ox oy] [(/ (- hw vw) 2) (/ (- hh vh) 2)]]
    ($ :g.scene-bounds {:transform (str "translate(" ox " , " oy ")")}
      ($ :rect {:x 0 :y 0 :width vw :height vh :rx 8}))))

(defui ^:private render-cursor
  [{[x y] :coord color :color}]
  (let [[point set-point] (use-state nil)
        on-animate-cursor (use-callback
                           (fn [point]
                             (let [x (aget point 0) y (aget point 1)]
                               (set-point [x y]))) [])
        on-point-move     (use-cursor on-animate-cursor [x y])]
    (use-layout-effect
     (fn []
       (on-point-move [x y])) [on-point-move x y])
    (if (not (nil? point))
      (let [[ax ay] point]
        ($ :g.scene-cursor {:transform (str "translate(" (- ax 4) ", " (- ay 4) ")") :color color}
          ($ icon {:name "cursor-fill" :size 32}))))))

(def ^:private query-cursors
  [[:session/share-cursors :default true]
   {:session/conns
    [:db/key
     [:session/share-cursor :default true]
     [:local/color :default "royalBlue"]]}])

(defui ^:private render-cursors []
  (let [[coords set-coords] (use-state {})
        result (use-query query-cursors [:db/ident :session])
        {conns :session/conns
         share :session/share-cursors} result
        conns (key-by :db/key conns)]
    (use-subscribe :cursor/moved
      (use-callback
       (fn [{[uuid x y] :args}]
         (if share
           (set-coords
            (fn [m]
              (assoc m uuid [x y]))))) [share]))
    (if share
      ($ :g.scene-cursors
        (for [[uuid point] coords
              :let  [local (conns uuid)
                     color (:local/color local)
                     share (:session/share-cursor local)]
              :when (and local share)]
          ($ render-cursor {:key uuid :coord point :color color}))))))

(def ^:private query-scene
  [:local/type
   :local/modifier
   [:local/privileged? :default false]
   [:bounds/self :default [0 0 0 0]]
   [:bounds/host :default [0 0 0 0]]
   [:bounds/view :default [0 0 0 0]]
   {:local/camera
    [:db/key
     [:camera/point :default [0 0]]
     [:camera/scale :default 1]
     [:camera/draw-mode :default :select]
     {:camera/scene
      [[:scene/dark-mode :default false]]}]}])

(defui render-scene []
  (let [dispatch (use-dispatch)
        publish  (use-publish)
        result   (use-query query-scene)
        {type        :local/type
         privileged? :local/privileged?
         modifier    :local/modifier
         [_ _ hw hh] :bounds/host
         [_ _ vw vh] :bounds/view
         [sx sy _ _] :bounds/self
         {key     :db/key
          scale   :camera/scale
          mode    :camera/draw-mode
          [cx cy] :camera/point
          {dark-mode :scene/dark-mode}
          :camera/scene}
         :local/camera} result
        cx (if (= type :view) (->> (- hw vw) (max 0) (* (/ -1 2 scale)) (+ cx)) cx)
        cy (if (= type :view) (->> (- hh vh) (max 0) (* (/ -1 2 scale)) (+ cy)) cy)
        on-translate
        (use-callback
         (fn [_ data]
           (let [ox (.-x data)
                 oy (.-y data)]
             (if (and (= ox 0) (= oy 0))
               (dispatch :selection/clear)
               (let [tx (+ cx (* ox (/ scale)))
                     ty (+ cy (* oy (/ scale)))]
                 (dispatch :camera/translate tx ty)))))
         [dispatch cx cy scale])
        on-cursor-move
        (use-callback
         (fn [event]
           (let [transform (.. event -currentTarget (getAttribute "transform"))]
             (if (= transform "translate(0,0)")
               (let [x (- (/ (- (.-clientX event) sx) scale) cx)
                     y (- (/ (- (.-clientY event) sy) scale) cy)]
                 (publish {:topic :cursor/move :args [x y]})))))
         [publish sx sy cx cy scale])]
    ($ :svg.scene {:key key :class (css {:theme--light (not dark-mode) :theme--dark dark-mode :is-host (= type :host) :is-priv privileged?})}
      ($ react-draggable {:position #js {:x 0 :y 0} :on-stop on-translate}
        ($ :g {:style {:will-change "transform"} :on-mouse-move on-cursor-move}
          ($ :rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"})
          (if (and (= mode :select) (= modifier :shift))
            ($ draw {:mode :select}))
          ($ :g.scene-board {:transform (str "scale(" scale ") translate(" cx ", " cy ")")}
            ($ render-token-faces)
            ($ render-scene-image)
            ($ render-grid)
            ($ render-shapes)
            ($ render-tokens)
            ($ render-mask-vis)
            ($ render-mask-fog)
            ($ render-cursors)
            ($ create-portal {:name :selected}
              (fn [{:keys [ref]}]
                ($ :g {:ref ref :style {:outline "none"}}))))))
      ($ create-portal {:name :multiselect}
        (fn [{:keys [ref]}]
          ($ :g {:ref   ref
                 :class "scene-drawable scene-drawable-select"
                 :style {:outline "none"}})))
      (if (contains? draw-modes mode)
        ($ :g {:class (str "scene-drawable scene-drawable-" (name mode))}
          ($ draw {:key mode :mode mode :node nil})))
      (if privileged? ($ render-bounds)))))
