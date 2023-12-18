(ns ogres.app.render.scene
  (:require [clojure.set :refer [difference intersection]]
            [clojure.string :refer [join]]
            [goog.object :refer [getValueByKeys]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.geom :refer [bounding-box chebyshev triangle]]
            [ogres.app.hooks :refer [create-portal use-subscribe use-dispatch use-image use-portal use-publish use-query]]
            [ogres.app.render :refer [icon]]
            [ogres.app.render.draw :refer [draw]]
            [ogres.app.render.forms :refer [token-context-menu shape-context-menu]]
            [ogres.app.render.pattern :refer [pattern]]
            [ogres.app.util :refer [key-by separate]]
            [uix.core :as uix :refer [defui $ use-callback use-effect use-memo use-state]]
            ["@dnd-kit/core"
             :refer  [DndContext useDndMonitor useDraggable]
             :rename {DndContext    dnd-context
                      useDndMonitor use-dnd-monitor
                      useDraggable  use-draggable}]))

(def ^:private draw-modes
  #{:grid :ruler :circle :rect :cone :line :poly :mask})

(def ^:private color-matrix
  {:none     [1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0]
   :dusk     [0.3 0.3 0.0 0.0 0.0 0.0 0.3 0.3 0.0 0.0 0.0 0.0 0.8 0.0 0.0 0.0 0.0 0.0 1.0 0.0]
   :midnight [0.0 0.0 0.0 0.0 0.0 0.0 0.1 0.0 0.0 0.0 0.1 0.1 0.1 0.0 0.0 0.0 0.0 0.0 1.0 0.0]})

(def ^:private condition->icon
  {:blinded       "eye-slash-fill"
   :charmed       "arrow-through-heart-fill"
   :defeaned      "ear-fill"
   :exhausted     "moon-stars-fill"
   :frightened    "black-cat"
   :grappled      "fist"
   :incapacitated "emoji-dizzy"
   :initiative    "hourglass-split"
   :invisible     "incognito"
   :petrified     "gem"
   :player        "people-fill"
   :poisoned      "droplet-fill"
   :prone         "falling"
   :restrained    "spiderweb"
   :stunned       "stars"
   :unconscious   "skull"})

(defn ^:private stop-propagation [event]
  (.stopPropagation event))

(defn ^:private label
  [{:keys [token/label initiative/suffix]}]
  (cond-> ""
    (string? label) (str label)
    (number? suffix) (str " " (char (+ suffix 64)))))

(defn ^:private token-comparator [a b]
  (let [[ax ay] (:token/point a)
        [bx by] (:token/point b)]
    (compare [(:token/size b) by bx]
             [(:token/size a) ay ax])))

(defn ^:private dnd-modifier-int [params]
  (let [dx (.. params -transform -x)
        dy (.. params -transform -y)]
    (js/Object.assign
     #js {} (.-transform params)
     #js {"x" (js/Math.trunc dx)
          "y" (js/Math.trunc dy)})))

(defn ^:private dnd-modifier-scale [scale]
  (fn [params]
    (let [dx (.. params -transform -x)
          dy (.. params -transform -y)]
      (js/Object.assign
       #js {} (.-transform params)
       #js {"x" (/ dx scale)
            "y" (/ dy scale)}))))

(def ^:private invert-drag-data-xf
  (comp (filter (comp seq :local/dragging))
        (mapcat (fn [local]
                  (map (juxt :db/id (constantly local))
                       (:local/dragging local))))))

(defn ^:private dragged-by-fn [name keys]
  (fn [dict]
    (into dict (map (juxt identity (constantly name))) keys)))

(defn ^:private use-cursor-point [uuid ox oy]
  (let [[point set-point] (use-state nil)]
    (use-effect
     (fn []
       (if (nil? uuid)
         (set-point nil))) [uuid])
    (use-subscribe :cursor/moved {:disabled (nil? uuid)}
      (use-callback
       (fn [{[id cx cy] :args}]
         (if (= id uuid)
           (set-point
            (fn [[_ _ dx dy]]
              (let [rx (- cx ox) ry (- cy oy)]
                (if (nil? dx)
                  [(- rx rx) (- ry ry) rx ry]
                  [(- cx ox dx) (- cy oy dy) dx dy])))))) [uuid ox oy])) point))

(defui ^:private render-live
  [{:keys [children owner ox oy]}]
  (let [[dx dy] (use-cursor-point owner ox oy)]
    (children dx dy)))

(defui ^:private render-drag
  [{:keys [children id class idxs disabled]
    :or   {disabled false}}]
  (let [options (use-draggable #js {"id" id "data" #js {"class" class "id" idxs} "disabled" disabled})]
    (children options)))

(def ^:private query-scene-image
  [{:local/camera
    [{:camera/scene
      [[:scene/grid-size :default 70]
       [:scene/timeofday :default :none]
       {:scene/image [:image/checksum]}]}]}])

(defui ^:private scene-image [props]
  (let [url (use-image (:checksum props))]
    ((:children props) url)))

(defui ^:private render-scene-image []
  (let [result (use-query query-scene-image)
        {{{size        :scene/grid-size
           time-of-day :scene/timeofday
           {checksum   :image/checksum} :scene/image}
          :camera/scene}
         :local/camera} result]
    ($ :g.scene-image {:transform (str "scale(" (/ grid-size size) ")")}
      ($ :defs {:key time-of-day}
        ($ :filter {:id "atmosphere"}
          ($ :feColorMatrix {:type "matrix" :values (join " " (color-matrix time-of-day))})))
      (if checksum
        ($ scene-image {:checksum checksum}
          (fn [url]
            ($ :image {:x 0 :y 0 :href url :style {:filter "url(#atmosphere)"}})))))))

(def ^:private query-mask-vis
  [:local/type
   {:local/camera
    [{:camera/scene
      [[:scene/grid-size :default 70]
       [:scene/lighting :default :revealed]
       {:scene/tokens
        [:db/id
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
      ($ :g.scene-mask {:data-visibility (name visibility)}
        ($ :defs
          ($ pattern {:id "mask-pattern" :name :lines :color "black"})
          ($ :radialGradient {:id "mask-gradient"}
            ($ :stop {:offset "0%" :stop-color "black" :stop-opacity "100%"})
            ($ :stop {:offset "70%" :stop-color "black" :stop-opacity "100%"})
            ($ :stop {:offset "100%" :stop-color "black" :stop-opacity "0%"}))
          ($ :mask {:id "light-mask"}
            ($ :rect {:x 0 :y 0 :width width :height height :fill "white" :fill-opacity "100%"})
            (for [{id :db/id flags :token/flags [x y] :token/point radius :token/light} tokens
                  :when (and (> radius 0) (or (= type :host) (not (flags :hidden))))
                  :let  [radius (-> grid-size (* radius) (/ 5) (+ grid-size))]]
              ($ :circle {:key id :cx x :cy y :r radius :fill "url(#mask-gradient)"}))))
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
       {:scene/masks [:db/id :mask/vecs :mask/enabled?]}]}]}])

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
          (for [{:keys [db/id  mask/vecs mask/enabled?]} masks]
            ($ :polygon {:key id :points (join " " vecs) :fill (if enabled? "white" "black")}))))
      ($ :rect.scene-mask-background {:x 0 :y 0 :width width :height height :mask "url(#scene-mask)"})
      ($ :rect.scene-mask-pattern {:x 0 :y 0 :width width :height height :fill "url(#mask-pattern)" :mask "url(#scene-mask)"})
      (if (and (= type :host) (contains? modes mode))
        (for [{:keys [db/id mask/vecs mask/enabled?]} masks]
          ($ :polygon.scene-mask-polygon
            {:key id
             :data-enabled enabled?
             :points (join " " vecs)
             :on-pointer-down stop-propagation
             :on-click
             (fn []
               (case mode
                 :mask-toggle (dispatch :mask/toggle id (not enabled?))
                 :mask-remove (dispatch :mask/remove id)))}))))))

(def ^:private query-grid
  [[:bounds/self :default [0 0 0 0]]
   {:local/camera
    [[:camera/point :default [0 0]]
     [:camera/scale :default 1]
     [:camera/draw-mode :default :select]
     {:camera/scene
      [[:scene/show-grid :default true]
       [:scene/grid-origin :default [0 0]]]}]}])

(defui ^:private render-grid []
  (let [data (use-query query-grid)
        {[_ _ w h] :bounds/self
         {[cx cy] :camera/point
          mode    :camera/draw-mode
          scale   :camera/scale
          scene   :camera/scene} :local/camera} data]
    (if (and (:scene/show-grid scene) (not= mode :grid))
      (let [wd (/ w scale)
            ht (/ h scale)
            ax (+ (* wd -3) cx)
            ay (+ (* ht -3) cy)
            bx (+ (* wd  3) cx)
            by (+ (* ht  3) cy)
            [ox oy] (:scene/grid-origin scene)]
        ($ :g.scene-grid {:transform (str "translate(" (mod ox grid-size) "," (mod oy grid-size) ")")}
          ($ :defs
            ($ :pattern {:id "grid" :width grid-size :height grid-size :patternUnits "userSpaceOnUse"}
              ($ :path {:d (join " " ["M" 0 0 "H" grid-size "V" grid-size])})))
          ($ :path {:d (join " " ["M" ax ay "H" bx "V" by "H" ax "Z"]) :fill "url(#grid)"}))))))

(defn ^:private poly-xf [x y]
  (comp (partition-all 2)
        (mapcat (fn [[ax ay]] [(- ax x) (- ay y)]))))

(defui ^:private render-shape-circle
  [props]
  (let [{:keys [data attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} data
        [ax ay bx by] vecs]
    ($ :circle
      (->> {:cx 0 :cy 0 :r (chebyshev ax ay bx by) :fill-opacity opacity :stroke color}
           (merge attrs)))))

(defui ^:private render-shape-rect
  [props]
  (let [{:keys [data attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} data
        [ax ay bx by] vecs]
    ($ :path
      (->> {:d (join " " ["M" 0 0 "H" (- bx ax) "V" (- by ay) "H" 0 "Z"]) :fill-opacity opacity :stroke color}
           (merge attrs)))))

(defui ^:private render-shape-line
  [props]
  (let [{:keys [data]} props
        {:keys [shape/vecs shape/color]} data
        [ax ay bx by] vecs]
    ($ :line {:x1 0 :y1 0 :x2 (- bx ax) :y2 (- by ay) :stroke color :stroke-width 4 :stroke-linecap "round"})))

(defui ^:private render-shape-cone
  [props]
  (let [{:keys [data attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} data
        [ax ay bx by] vecs]
    ($ :polygon
      (->> {:points (join " " (triangle 0 0 (- bx ax) (- by ay))) :fill-opacity opacity :stroke color}
           (merge attrs)))))

(defui ^:private render-shape-poly
  [props]
  (let [{:keys [data attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} data
        [ax ay] (into [] (take 2) vecs)
        pairs   (into [] (poly-xf ax ay) vecs)]
    ($ :polygon (assoc attrs :points (join " " pairs) :fill-opacity opacity :stroke color))))

(def ^:private render-shape
  (uix/memo
   (uix/fn [props]
     (case (:shape/kind (:data props))
       :circle ($ render-shape-circle props)
       :rect   ($ render-shape-rect props)
       :line   ($ render-shape-line props)
       :cone   ($ render-shape-cone props)
       :poly   ($ render-shape-poly props)))))

(def ^:private query-shapes
  [:local/type
   {:local/camera
    [:db/id
     [:camera/scale :default 1]
     {:camera/scene
      [{:scene/shapes
        [:db/id
         :shape/kind
         :shape/vecs
         [:shape/color :default "#f44336"]
         [:shape/opacity :default 0.25]
         [:shape/pattern :default :solid]
         :camera/_selected]}]}]}])

(defui ^:private render-shapes []
  (let [dispatch (use-dispatch)
        result   (use-query query-shapes)
        scale    (:camera/scale (:local/camera result))
        shapes   (:scene/shapes (:camera/scene (:local/camera result)))
        user?    (not= (:local/type result) :view)]
    (use-dnd-monitor
     #js {"onDragEnd"
          (use-callback
           (fn [data]
             (if (= (getValueByKeys data "active" "data" "current" "class") "shape")
               (let [id (first (getValueByKeys data "active" "data" "current" "id"))
                     dx (.. data -delta -x)
                     dy (.. data -delta -y)]
                 (if (and (= dx 0) (= dy 0))
                   (dispatch :element/select id)
                   (dispatch :shape/translate id dx dy))))) [dispatch])})
    ($ :g.scene-shapes
      (for [{id :db/id [sx sy] :shape/vecs :as data} shapes
            :let [selecting (into #{} (map :db/id) (:camera/_selected data))
                  selected? (contains? selecting (:db/id (:local/camera result)))]]
        ($ use-portal {:key id :name (if (and user? selected?) :selected)}
          ($ render-drag {:id id :class "shape" :idxs (list id)}
            (fn [options]
              (let [id (random-uuid)
                    dx (getValueByKeys options "transform" "x")
                    dy (getValueByKeys options "transform" "y")
                    tx (+ sx (or dx 0))
                    ty (+ sy (or dy 0))]
                ($ :g.scene-shape
                  {:ref (.-setNodeRef options)
                   :class (str "scene-shape-" (name (:shape/kind data)))
                   :transform (str "translate(" tx ", " ty ")")
                   :on-pointer-down (getValueByKeys options "listeners" "onPointerDown")
                   :data-selected selected?}
                  ($ :defs ($ pattern {:id id :name (:shape/pattern data) :color (:shape/color data)}))
                  ($ render-shape {:data data :attrs {:fill (str "url(#" id ")")}})
                  (if (and user? selected?)
                    ($ :foreignObject.context-menu-object
                      {:x -200 :y 0
                       :width 400 :height 400
                       :transform (str "scale(" (/ scale) ")")}
                      ($ shape-context-menu
                        {:data data}))))))))))))

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

(defui ^:private render-token-defs []
  (let [result    (use-query query-token-faces)
        tokens    (-> result :local/camera :camera/scene :scene/tokens)
        checksums (into #{} (comp (map :token/image) (map :image/checksum)) tokens)
        attrs     {:width "100%" :height "100%" :patternContentUnits "objectBoundingBox"}]
    ($ :defs {:style {:color "white"}}
      ($ :linearGradient#token-base-player {:x1 0 :y1 0 :x2 1 :y2 1}
        ($ :stop {:style {:stop-color "#fcd34d"} :offset "0%"})
        ($ :stop {:style {:stop-color "#b45309"} :offset "100%"}))
      ($ :pattern#token-face-default (merge attrs {:viewBox "-2 -2 16 16"})
        ($ :rect {:x -2 :y -2 :width 16 :height 16 :fill "var(--color-blues-700)"})
        ($ icon {:name "dnd" :size 12}))
      ($ :pattern#token-face-deceased (merge attrs {:viewBox "-2 -2 16 16"})
        ($ :rect {:x -2 :y -2 :width 16 :height 16 :fill "var(--color-blues-700)"})
        ($ icon {:name "skull" :size 12}))
      (for [checksum checksums]
        ($ :pattern (merge attrs {:key checksum :id (str "token-face-" checksum)})
          ($ render-token-face {:checksum checksum}))))))

(defn ^:private token-flags [data]
  (let [{[{turn :initiative/turn}] :scene/_initiative} data]
    (cond-> (:token/flags data)
      (:scene/_initiative data)       (conj :initiative)
      (= (:db/id data) (:db/id turn)) (conj :turn))))

(defn ^:private token-flags-attr [data]
  (join " " (map name (token-flags data))))

(defn ^:private token-conditions [data]
  (let [xform (comp (map key) (filter (complement #{:initiative})))
        order (into [:initiative] xform condition->icon)
        exclu #{:player :hidden :unconscious}]
    (take 4 (filter (difference (token-flags data) exclu) order))))

(def ^:private render-token
  (uix/memo
   (uix/fn [{:keys [data]}]
     (let [radii (- (/ grid-size 2) 2)
           flags (:token/flags data)
           scale (/ (:token/size data) 5)
           hashs (:image/checksum (:token/image data))
           pttrn (cond (flags :unconscious) (str "token-face-deceased")
                       (string? hashs)      (str "token-face-" hashs)
                       :else                (str "token-face-default"))]
       ($ :g.scene-token
         {:transform  (str "scale(" scale ")") :data-flags (token-flags-attr data)}
         (let [radius (* grid-size (/ (:aura/radius data) 5))]
           (if (> radius 0)
             ($ :circle.scene-token-aura {:cx 0 :cy 0 :r (+ radius (/ grid-size 2))})))
         ($ :circle.scene-token-shape {:cx 0 :cy 0 :r radii :fill (str "url(#" pttrn ")")})
         ($ :circle.scene-token-ring
           {:cx 0 :cy 0 :r (+ radii 5) :pathLength 100})
         (for [[deg flag] (mapv vector [-120 120 -65 65] (token-conditions data))
               :let [rn (* (/ js/Math.PI 180) deg)
                     cx (* (js/Math.sin rn) radii)
                     cy (* (js/Math.cos rn) radii)]]
           ($ :g.scene-token-flags {:key flag :data-flag flag :transform (str "translate(" cx ", " cy ")")}
             ($ :circle {:cx 0 :cy 0 :r 12})
             ($ :g {:transform (str "translate(" -8 ", " -8 ")")}
               ($ icon {:name (condition->icon flag) :size 16}))))
         (if (seq (label data))
           ($ :foreignObject.context-menu-object {:x -200 :y (- radii 8) :width 400 :height 32}
             ($ :.scene-token-label
               ($ :span (label data))))))))))

(def ^:private query-tokens
  [{:root/local
    [:local/type
     {:local/camera
      [:db/id
       :camera/selected
       [:camera/scale :default 1]
       {:camera/scene
        [{:scene/tokens
          [:db/id
           [:initiative/suffix :default nil]
           [:token/point :default [0 0]]
           [:token/flags :default #{}]
           [:token/label :default ""]
           [:token/size :default 5]
           [:token/light :default 15]
           [:aura/radius :default 0]
           {:token/image [:image/checksum]}
           {:scene/_initiative [:db/id :initiative/turn]}
           :camera/_selected]}]}]}]
    :root/session
    [{:session/conns
      [:local/uuid :local/color :local/dragging]}]}])

(defui ^:private render-tokens []
  (let [[dragged-by set-dragged-by] (use-state {})
        dispatch (use-dispatch)
        result   (use-query query-tokens [:db/ident :root])
        local    (:root/local result)
        scene    (:camera/scene (:local/camera local))
        scale    (:camera/scale (:local/camera local))
        type     (:local/type local)
        drag     (use-memo
                  (fn []
                    (->> (:session/conns (:root/session result))
                         (into {} invert-drag-data-xf))) [result])
        [selected tokens]
        (->> (:scene/tokens scene)
             (filter (fn [token] (or (= type :host) (not ((:token/flags token) :hidden)))))
             (sort token-comparator)
             (separate (fn [token]
                         ((into #{} (map :db/id) (:camera/_selected token))
                          (:db/id (:local/camera local))))))]
    (use-effect
     (fn [] (set-dragged-by (dragged-by-fn "remote" (keys drag)))) [drag])
    (use-dnd-monitor
     #js {"onDragStart"
          (use-callback
           (fn [data]
             (let [class (getValueByKeys data "active" "data" "current" "class")]
               (if (or (= class "token") (= class "tokens"))
                 (let [idxs (getValueByKeys data "active" "data" "current" "id")]
                   (set-dragged-by (dragged-by-fn "local" idxs))
                   (dispatch :drag/start idxs))))) [dispatch])
          "onDragEnd"
          (use-callback
           (fn [data]
             (let [event (.. data -activatorEvent)]
               (case (getValueByKeys data "active" "data" "current" "class")
                 "tokens"
                 (let [id (.. event -target (closest "[data-id]") -dataset -id)
                       dx (.. data -delta -x)
                       dy (.. data -delta -y)]
                   (if (and (= dx 0) (= dy 0))
                     (dispatch :element/select (js/Number id) (.-shiftKey event))
                     (dispatch :token/translate-all (seq (.. data -active -data -current -id)) dx dy)))
                 "token"
                 (let [id (first (.. data -active -data -current -id))
                       dx (.. data -delta -x)
                       dy (.. data -delta -y)]
                   (if (and (= dx 0) (= dy 0))
                     (dispatch :element/select id (.-shiftKey event))
                     (dispatch :token/translate id dx dy))) nil))) [dispatch])
          "onDragCancel"
          (use-callback
           (fn [data]
             (case (getValueByKeys data "active" "data" "current" "class")
               ("tokens" "token") (dispatch :drag/end) nil)) [dispatch])})
    ($ :<>
      ($ :g.scene-tokens
        (for [{id :db/id [tx ty] :token/point :as data} tokens :let [owner (drag id)]]
          ($ render-live {:key id :owner (:local/uuid owner) :ox tx :oy ty}
            (fn [rx ry]
              ($ render-drag {:id id :idxs (list id) :class "token" :disabled (some? owner)}
                (fn [options]
                  (let [dx (getValueByKeys options "transform" "x")
                        dy (getValueByKeys options "transform" "y")
                        ax (+ tx (or rx dx 0))
                        ay (+ ty (or ry dy 0))]
                    ($ :g.scene-token-position
                      {:ref (.-setNodeRef options)
                       :transform (str "translate(" ax ", " ay ")")
                       :on-pointer-down (or (getValueByKeys options "listeners" "onPointerDown") stop-propagation)
                       :data-color (:local/color owner)
                       :data-dragging (or (some? owner) (.-isDragging options))
                       :data-dragged-by (get dragged-by id "none")}
                      ($ render-token {:data data})))))))))
      (if (seq selected)
        (let [idxs (into (sorted-set) (map :db/id) selected)
              cont (boolean (seq (intersection idxs (set (keys drag)))))]
          ($ use-portal {:key idxs :name (if (or (= type :host) (= type :conn)) :selected)}
            ($ render-drag {:id "tokens" :class "tokens" :idxs (seq idxs) :disabled cont}
              (fn [options]
                (let [dx (getValueByKeys options "transform" "x")
                      dy (getValueByKeys options "transform" "y")]
                  ($ :g.scene-tokens-selected
                    {:ref (.-setNodeRef options)
                     :transform (str "translate(" (or dx 0) ", " (or dy 0) ")")
                     :on-pointer-down (or (getValueByKeys options "listeners" "onPointerDown") stop-propagation)}
                    (for [{id :db/id [tx ty] :token/point :as data} selected :let [owner (drag id)]]
                      ($ render-live {:key id :owner (:local/uuid owner) :ox tx :oy ty}
                        (fn [rx ry]
                          ($ :g.scene-token-position
                            {:data-id id
                             :transform (str "translate(" (+ tx rx) ", " (+ ty ry) ")")
                             :data-color (:local/color owner)
                             :data-dragging (or (some? owner) (.-isDragging options))
                             :data-dragged-by (get dragged-by id "none")}
                            ($ render-token {:data data})))))
                    (if (or (= type :host) (= type :conn))
                      (let [[ax _ bx by] (apply bounding-box (map :token/point selected))]
                        ($ :foreignObject.context-menu-object
                          {:x (- (+ (* ax scale) (/ (* (- bx ax) scale) 2)) (/ 400 2))
                           :y (- (+ (* by scale) (* scale 56)) 24)
                           :width 400 :height 400
                           :transform (str "scale(" (/ scale) ")")}
                          ($ token-context-menu {:tokens selected :type type}))))))))))))))

(defui ^:private render-bounds []
  (let [result (use-query [:bounds/host :bounds/view])
        {[_ _ hw hh] :bounds/host
         [_ _ vw vh] :bounds/view} result
        [ox oy] [(/ (- hw vw) 2) (/ (- hh vh) 2)]]
    ($ :g.scene-bounds {:transform (str "translate(" ox " , " oy ")")}
      ($ :rect {:x 0 :y 0 :width vw :height vh :rx 8}))))

(def ^:private cursors-query
  [{:root/local [{:local/camera [:camera/scene]}]}
   {:root/session
    [[:session/share-cursors :default true]
     {:session/host [:local/uuid]}
     {:session/conns
      [:local/uuid
       [:local/share-cursor :default true]
       [:local/color :default "none"]
       {:local/camera [:camera/scene]}]}]}])

(defui ^:private render-cursors []
  (let [result (use-query cursors-query [:db/ident :root])
        conns  (key-by :local/uuid (:session/conns (:root/session result)))
        share  (:session/share-cursors (:root/session result))
        host   (:session/host (:root/session result))
        [points set-points] (use-state {})]
    (use-subscribe :cursor/moved {:disabled (not share)}
      (use-callback
       (fn [{[uuid x y] :args}]
         (set-points (fn [points] (assoc points uuid [x y])))) []))
    (if share
      ($ :g.scene-cursors
        (for [[uuid [x y]] points
              :let [conn (conns uuid)]
              :when (and (some? conn)
                         (:local/share-cursor conn)
                         (= (:camera/scene (:local/camera conn))
                            (:camera/scene (:local/camera (:root/local result)))))]
          ($ use-portal {:key uuid :name (if (= (:local/uuid host) (:local/uuid conn)) :host-cursor)}
            ($ :g.scene-cursor
              {:transform (str "translate(" (- x 3) ", " (- y 3) ")") :data-color (:local/color conn)}
              ($ icon {:name "cursor-fill" :size 28}))))))))

(def ^:private scene-camera-draggable
  (uix/memo
   (uix/fn [{:keys [on-cursor-move children]
             :or   {on-cursor-move identity}}]
     (let [options (use-draggable #js {"id" "scene-camera" "data" #js {"class" "camera"}})
           drag? (.-isDragging options)
           dx (getValueByKeys options "transform" "x")
           dy (getValueByKeys options "transform" "y")]
       ($ :g
         {:ref (.-setNodeRef options)
          :style {:will-change "transform"}
          :transform (str "translate(" (or dx 0) ", " (or dy 0) ")")
          :on-pointer-down (.. options -listeners -onPointerDown)
          :on-pointer-move
          (use-callback
           (fn [event]
             (if (not drag?)
               (on-cursor-move (.-clientX event) (.-clientY event))))
           [on-cursor-move drag?])}
         ($ :rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"})
         children)))))

(def ^:private scene-camera
  (uix/memo
   (uix/fn [{:keys [scale on-translate on-cursor-move children]
             :or   {on-translate identity on-cursor-move identity}}]
     (let [scale-fn (use-memo (fn [] (dnd-modifier-scale scale)) [scale])]
       ($ dnd-context
         #js {"modifiers" #js [dnd-modifier-int]
              "onDragEnd"
              (use-callback
               (fn [data]
                 (on-translate
                  (.. data -delta -x)
                  (.. data -delta -y))) [on-translate])}
         ($ scene-camera-draggable
           {:on-cursor-move on-cursor-move}
           ($ dnd-context
             #js {"modifiers" #js [scale-fn dnd-modifier-int]}
             children)))))))

(def ^:private scene-elements
  (uix/memo
   (uix/fn []
     ($ :<>
       ($ render-token-defs)
       ($ render-scene-image)
       ($ render-grid)
       ($ render-shapes)
       ($ render-tokens)
       ($ create-portal {:name :host-cursor}
         (fn [{:keys [ref]}]
           ($ :g {:ref ref :style {:outline "none"}})))
       ($ render-mask-vis)
       ($ render-mask-fog)
       ($ render-cursors)
       ($ create-portal {:name :selected}
         (fn [{:keys [ref]}]
           ($ :g {:ref ref :style {:outline "none"}})))))))

(def ^:private query-scene
  [:local/type
   :local/modifier
   [:local/privileged? :default false]
   [:bounds/self :default [0 0 0 0]]
   [:bounds/host :default [0 0 0 0]]
   [:bounds/view :default [0 0 0 0]]
   {:local/camera
    [:db/id
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
         [_ _ hw hh] :bounds/host
         [_ _ vw vh] :bounds/view
         [sx sy _ _] :bounds/self
         {id      :db/id
          scale   :camera/scale
          mode    :camera/draw-mode
          [cx cy] :camera/point
          {dark-mode :scene/dark-mode}
          :camera/scene}
         :local/camera} result
        cx (if (= type :view) (->> (- hw vw) (max 0) (* (/ -1 2 scale)) (- cx)) cx)
        cy (if (= type :view) (->> (- hh vh) (max 0) (* (/ -1 2 scale)) (- cy)) cy)]
    ($ :svg.scene {:key id :data-user-type (name type) :data-theme (if dark-mode "dark" "light")}
      ($ scene-camera
        {:scale scale
         :on-translate
         (use-callback
          (fn [dx dy]
            (if (and (= dx 0) (= dy 0))
              (dispatch :selection/clear)
              (let [tx (- (/ dx scale) cx)
                    ty (- (/ dy scale) cy)]
                (dispatch :camera/translate (- tx) (- ty)))))
          [dispatch cx cy scale])
         :on-cursor-move
         (use-callback
          (fn [dx dy]
            (let [mx (int (+ (/ (- dx sx) scale) cx))
                  my (int (+ (/ (- dy sy) scale) cy))]
              (publish {:topic :cursor/move :args [mx my]})))
          [publish sx sy cx cy scale])}
        (if (and (= mode :select) (= (:local/modifier result) :shift))
          ($ draw {:mode :select}))
        ($ :g.scene-board {:transform (str "scale(" scale ") translate(" (- cx) ", " (- cy) ")")}
          ($ scene-elements)))
      ($ create-portal {:name :multiselect}
        (fn [{:keys [ref]}]
          ($ :g.scene-drawable.scene-drawable-select
            {:ref ref :style {:outline "none"}})))
      (if (contains? draw-modes mode)
        ($ :g.scene-drawable {:class (str "scene-drawable-" (name mode))}
          ($ draw {:key mode :mode mode :node nil})))
      (if (:local/privileged? result) ($ render-bounds)))))
