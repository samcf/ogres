(ns ogres.app.component.scene
  (:require [clojure.set :refer [difference intersection]]
            [clojure.string :refer [join]]
            [goog.object :refer [getValueByKeys]]
            [ogres.app.component :refer [icon image]]
            [ogres.app.component.scene-context-menu :refer [token-context-menu shape-context-menu]]
            [ogres.app.component.scene-draw :refer [draw]]
            [ogres.app.component.scene-pattern :refer [pattern]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.geom :refer [bounding-rect shape-bounding-rect chebyshev-distance circle->path poly->path cone-points point-within-rect]]
            [ogres.app.hooks :refer [create-portal use-subscribe use-dispatch use-portal use-query]]
            [ogres.app.util :refer [key-by round-grid]]
            [react-transition-group :refer [TransitionGroup CSSTransition Transition]]
            [uix.core :as uix :refer [defui $ create-ref use-callback use-effect use-memo use-state]]
            [uix.dom :as dom]
            ["@rwh/react-keystrokes" :refer [useKey] :rename {useKey use-key}]
            ["@dnd-kit/core"
             :refer [DndContext useDndMonitor useDraggable]
             :rename {DndContext dnd-context
                      useDndMonitor use-dnd-monitor
                      useDraggable use-draggable}]))

(defn ^:private token-light-xf [user]
  (comp (filter (fn [{radius :token/light flags :token/flags}]
                  (and (> radius 0) (or (= user :host) (not (flags :hidden))))))
        (map (fn [{[x y] :token/point radius :token/light}]
               [x y (+ (/ (* radius grid-size) 5) grid-size)]))))

(def ^:private mask-area-xf
  (comp (filter :mask/enabled?) (map :mask/vecs)))

(def ^:private draw-modes
  #{:grid :ruler :circle :rect :cone :line :poly :mask})

(def ^:private condition->icon
  {:blinded       "eye-slash-fill"
   :charmed       "arrow-through-heart-fill"
   :defeaned      "ear-fill"
   :exhausted     "moon-stars-fill"
   :frightened    "black-cat"
   :grappled      "fist"
   :incapacitated "lock-fill"
   :initiative    "hourglass-split"
   :invisible     "incognito"
   :paralyzed     "lightning-fill"
   :petrified     "gem"
   :poisoned      "droplet-fill"
   :prone         "falling"
   :restrained    "spiderweb"
   :stunned       "stars"
   :unconscious   "activity"})

(defn ^:private stop-propagation [event]
  (.stopPropagation event))

(defn ^:private token-label
  [{:keys [token/label initiative/suffix]}]
  (cond-> ""
    (string? label) (str label)
    (number? suffix) (str " " (char (+ suffix 64)))))

(defn ^:private tokens-order-cmp [a b]
  (let [{size-a :token/size [ax ay] :token/point} a
        {size-b :token/size [bx by] :token/point} b]
    (compare [size-b ax ay] [size-a bx by])))

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
  (comp (filter (comp seq :user/dragging))
        (mapcat (fn [user]
                  (map (juxt :db/id (constantly user))
                       (:user/dragging user))))))

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

(def ^:private image-defs-query
  [{:user/camera
    [{:camera/scene
      [[:scene/grid-size :default 70]
       [:scene/lighting :default :revealed]
       {:scene/image [:image/checksum :image/width :image/height]}]}]}])

(defui ^:private image-defs []
  (let [result (use-query image-defs-query)
        {{{{width    :image/width
            height   :image/height
            checksum :image/checksum} :scene/image
           size :scene/grid-size} :camera/scene} :user/camera} result
        transform (str "scale(" (/ grid-size size) ")")
        node (uix/use-ref)]
    ;; Safari has issues with rendering an `<image ...> immediately after it is
    ;; defined. One way to fix this is to update its `class` attribute after
    ;; a small delay. (#146)
    (uix/use-layout-effect
     (fn []
       (.setTimeout
        js/window
        (fn []
          (if-let [node (deref node)]
            (.. node -classList (add "__SAFARI_FIX")))) 128)) [])
    ($ :defs
      ($ :filter {:id "scene-image-filter" :filterRes 1 :color-interpolation-filters "sRGB"}
        ($ :feColorMatrix {:in "SourceGraphic" :type "saturate" :values 0.2 :result "Next"})
        ($ :feComponentTransfer {:in "Next"}
          ($ :feFuncR {:type "linear" :slope 0.60})
          ($ :feFuncG {:type "linear" :slope 0.60})
          ($ :feFuncB {:type "linear" :slope 0.60})))
      (if (some? checksum)
        ($ image {:checksum checksum}
          (fn [{:keys [data-url]}]
            ($ :image
              {:x 0 :y 0 :id "scene-image" :ref node :href data-url :width width :height height :transform transform}))))
      ($ :rect {:id "scene-image-cover" :x 0 :y 0 :width width :height height :transform transform})
      ($ :clipPath {:id "scene-image-clip"}
        ($ :use {:href "#scene-image-cover"})))))

(def ^:private mask-defs-query
  [:user/type
   {:user/camera
    [{:camera/scene
      [[:scene/masked :default false]
       [:scene/lighting :default :revealed]
       {:scene/masks
        [:db/id
         [:mask/vecs :default []]
         [:mask/enabled? :default true]]}
       {:scene/tokens
        [:db/id
         [:token/flags :default #{}]
         [:token/light :default 15]
         [:token/point :default [0 0]]]}]}]}])

(defui ^:private mask-defs []
  (let [result (use-query mask-defs-query)
        {user :user/type
         {{tokens :scene/tokens
           masks  :scene/masks
           light  :scene/lighting
           masked :scene/masked} :camera/scene} :user/camera} result]
    ($ :defs
      ($ pattern {:id "mask-pattern" :name :crosses})
      ($ :path {:id "masks-path" :d (transduce mask-area-xf poly->path masks)})
      ($ :path {:id "light-path" :d (transduce (token-light-xf user) circle->path tokens)})
      ($ :clipPath {:id "masks-clip"}
        ($ :use {:href "#masks-path"}))
      ($ :mask {:id "masks-mask"}
        ($ :use {:href "#scene-image-cover" :fill "white"})
        ($ :use {:href "#masks-path"}))
      (case [(= user :host) light masked]
        [true :revealed true]
        ($ :mask {:id "mask-primary"}
          ($ :use {:href "#scene-image-cover" :fill "white"})
          ($ :use {:href "#masks-path"}))
        [true :revealed false]
        ($ :mask {:id "mask-primary"}
          ($ :use {:href "#masks-path" :fill "white"}))
        [true :dimmed true]
        ($ :<>
          ($ :mask {:id "mask-primary"}
            ($ :use {:href "#scene-image-cover" :fill "white"})
            ($ :use {:href "#masks-path"}))
          ($ :mask {:id "mask-secondary"}
            ($ :g {:clip-path "url(#masks-clip)"}
              ($ :use {:href "#scene-image-cover" :fill "white"})
              ($ :use {:href "#masks-path" :fill "rgba(0, 0, 0, 0.5)"})
              ($ :use {:href "#light-path"}))))
        [true :dimmed false]
        ($ :<>
          ($ :mask {:id "mask-primary"}
            ($ :g {:mask "url(#masks-mask)"}
              ($ :use {:href "#scene-image-cover" :fill "white"})
              ($ :use {:href "#light-path"})))
          ($ :mask {:id "mask-secondary"}
            ($ :use {:href "#masks-path" :fill "white"})))
        [true :hidden true]
        ($ :mask {:id "mask-primary"}
          ($ :use {:href "#scene-image-cover" :fill "white"})
          ($ :use {:href "#light-path" :clip-path "url(#masks-clip)"}))
        [true :hidden false]
        ($ :mask {:id "mask-primary"}
          ($ :use {:href "#scene-image-cover" :fill "white"})
          ($ :use {:href "#light-path"})
          ($ :use {:href "#masks-path" :fill "white"}))
        ([false :revealed true] [false :revealed false])
        ($ :clipPath {:id "mask-primary"}
          ($ :use {:href "#masks-path"}))
        [false :dimmed true]
        ($ :<>
          ($ :clipPath {:id "mask-primary"}
            ($ :use {:href "#light-path"}))
          ($ :clipPath {:id "mask-secondary"}
            ($ :use {:href "#masks-path"})))
        [false :dimmed false]
        ($ :<>
          ($ :clipPath {:id "mask-primary"}
            ($ :use {:href "#light-path"}))
          ($ :clipPath {:id "mask-secondary"}
            ($ :use {:href "#masks-path"})))
        [false :hidden true]
        ($ :clipPath {:id "mask-primary" :clip-path "url(#masks-clip)"}
          ($ :use {:href "#light-path"}))
        [false :hidden false]
        ($ :<>
          ($ :clipPath {:id "mask-primary"}
            ($ :use {:href "#light-path"}))
          ($ :clipPath {:id "mask-secondary"}
            ($ :use {:href "#masks-path"})))))))

(def ^:private mask-polys-query
  [[:user/type :default :host]
   {:user/camera
    [[:camera/draw-mode :default :select]
     {:camera/scene
      [{:scene/masks
        [:db/id
         [:mask/vecs :default []]
         [:mask/enabled? :default true]]}]}]}])

(defui ^:private mask-polys []
  (let [dispatch (use-dispatch)
        result   (use-query mask-polys-query)
        {user :user/type
         {{masks :scene/masks} :camera/scene
          draw-mode :camera/draw-mode} :user/camera} result
        modes #{:mask :mask-toggle :mask-remove}]
    ($ :g.scene-mask-polys {:id "masks-polys"}
      (for [{id :db/id vecs :mask/vecs enabled? :mask/enabled?} masks
            :while (and (= user :host) (contains? modes draw-mode))]
        ($ :polygon.scene-mask-polygon
          {:key id
           :data-enabled enabled?
           :points (join " " vecs)
           :on-pointer-down stop-propagation
           :on-click
           (fn []
             (case draw-mode
               :mask-toggle (dispatch :mask/toggle id (not enabled?))
               :mask-remove (dispatch :mask/remove id)))})))))

(def ^:private grid-defs-query
  [[:bounds/self :default [0 0 0 0]]
   {:user/camera
    [[:camera/point :default [0 0]]
     [:camera/scale :default 1]
     {:camera/scene
      [[:scene/grid-origin :default [0 0]]]}]}])

(defui ^:private grid-defs []
  (let [data (use-query grid-defs-query)
        {[_ _ w h] :bounds/self
         {[cx cy] :camera/point
          scale   :camera/scale
          scene   :camera/scene} :user/camera} data
        wd (/ w scale)
        ht (/ h scale)
        ax (+ (* wd -3) cx)
        ay (+ (* ht -3) cy)
        bx (+ (* wd  3) cx)
        by (+ (* ht  3) cy)
        [ox oy] (:scene/grid-origin scene)]
    ($ :defs
      ($ :pattern {:id "grid-pattern" :width grid-size :height grid-size :patternUnits "userSpaceOnUse"}
        ($ :path.scene-grid-path {:d (join " " ["M" 0 0 "H" grid-size "V" grid-size])}))
      ($ :g {:id "scene-grid" :transform (str "translate(" (mod ox grid-size) "," (mod oy grid-size) ")")}
        ($ :path {:d (join " " ["M" ax ay "H" bx "V" by "H" ax "Z"]) :fill "url(#grid-pattern)"})))))

(defn ^:private shape-poly-xf [x y]
  (comp (partition-all 2)
        (mapcat (fn [[ax ay]] [(- ax x) (- ay y)]))))

(defui ^:private shape-circle
  [props]
  (let [{:keys [data attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} data
        [ax ay bx by] vecs]
    ($ :circle
      (->> {:cx 0 :cy 0 :r (chebyshev-distance ax ay bx by) :fill-opacity opacity :stroke color}
           (merge attrs)))))

(defui ^:private shape-rect
  [props]
  (let [{:keys [data attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} data
        [ax ay bx by] vecs]
    ($ :path
      (->> {:d (join " " ["M" 0 0 "H" (- bx ax) "V" (- by ay) "H" 0 "Z"]) :fill-opacity opacity :stroke color}
           (merge attrs)))))

(defui ^:private shape-line
  [props]
  (let [{:keys [data]} props
        {:keys [shape/vecs shape/color]} data
        [ax ay bx by] vecs]
    ($ :line {:x1 0 :y1 0 :x2 (- bx ax) :y2 (- by ay) :stroke color :stroke-width 4 :stroke-linecap "round"})))

(defui ^:private shape-cone
  [props]
  (let [{:keys [data attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} data
        [ax ay bx by] vecs]
    ($ :polygon
      (->> {:points (join " " (cone-points 0 0 (- bx ax) (- by ay))) :fill-opacity opacity :stroke color}
           (merge attrs)))))

(defui ^:private shape-poly
  [props]
  (let [{:keys [data attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} data
        [ax ay] (into [] (take 2) vecs)
        pairs   (into [] (shape-poly-xf ax ay) vecs)]
    ($ :polygon (assoc attrs :points (join " " pairs) :fill-opacity opacity :stroke color))))

(def ^:private shape
  (uix/memo
   (uix/fn [props]
     (case (:shape/kind (:data props))
       :circle ($ shape-circle props)
       :rect   ($ shape-rect props)
       :line   ($ shape-line props)
       :cone   ($ shape-cone props)
       :poly   ($ shape-poly props)))))

(def ^:private shapes-query
  [{:root/user
    [:user/type
     {:user/camera
      [:db/id
       :camera/selected
       [:camera/scale :default 1]
       {:camera/scene
        [{:scene/shapes
          [:db/id
           :shape/kind
           :shape/vecs
           [:shape/color :default "#f44336"]
           [:shape/opacity :default 0.25]
           [:shape/pattern :default :solid]]}]}]}]}
   {:root/session
    [{:session/conns
      [:user/uuid :user/color :user/dragging]}]}])

(defui ^:private shapes []
  (let [dispatch (use-dispatch)
        result   (use-query shapes-query [:db/ident :root])
        [dragged-by set-dragged-by] (use-state {})
        {{type :user/type
          {scale :camera/scale
           selected :camera/selected
           {shapes :scene/shapes} :camera/scene} :user/camera} :root/user
         {conns :session/conns} :root/session} result
        selected (into #{} (map :db/id) selected)
        dragging (into {} invert-drag-data-xf conns)
        user? (not= type :view)]
    (use-effect
     (fn [] (set-dragged-by (dragged-by-fn "remote" (keys dragging))))
     ^:lint/disable [result])
    (use-dnd-monitor
     #js {"onDragStart"
          (use-callback
           (fn [data]
             (if (= (getValueByKeys data "active" "data" "current" "class") "shape")
               (let [id (getValueByKeys data "active" "data" "current" "id")]
                 (set-dragged-by (dragged-by-fn "local" id))
                 (dispatch :drag/start id))))
           [dispatch])
          "onDragEnd"
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
            :let [owner     (dragging id)
                  tempid    (random-uuid)
                  selected? (contains? selected id)]]
        ($ use-portal {:key id :name (if (and user? selected?) :selected)}
          ($ render-live {:owner (:user/uuid owner) :ox sx :oy sy}
            (fn [rx ry]
              ($ render-drag {:id id :class "shape" :idxs (list id) :disabled (some? owner)}
                (fn [options]
                  (let [dx (getValueByKeys options "transform" "x")
                        dy (getValueByKeys options "transform" "y")
                        tx (+ sx (or rx dx 0))
                        ty (+ sy (or ry dy 0))]
                    ($ :g.scene-shapes.scene-shapes-selected
                      ($ :g.scene-shape
                        {:ref (.-setNodeRef options)
                         :class (str "scene-shape-" (name (:shape/kind data)))
                         :transform (str "translate(" tx ", " ty ")")
                         :on-pointer-down (or (getValueByKeys options "listeners" "onPointerDown") stop-propagation)
                         :data-color (:user/color owner)
                         :data-dragging (or (some? owner) (.-isDragging options))
                         :data-dragged-by (get dragged-by id "none")
                         :data-selected selected?}
                        ($ :defs ($ pattern {:id tempid :name (:shape/pattern data) :color (:shape/color data)}))
                        ($ shape {:data data :attrs {:fill (str "url(#" tempid ")")}})
                        (if (and user? selected?)
                          (let [{kind :shape/kind points :shape/vecs} data
                                [ax _ bx by] (shape-bounding-rect kind points)
                                sz 400
                                tx (-> (+ ax bx) (* scale) (- sz) (/ 2) (- (* scale sx)) int)
                                ty (-> (- by sy) (* scale) int)]
                            ($ :foreignObject.context-menu-object
                              {:x tx :y ty :width sz :height sz :transform (str "scale(" (/ scale) ")")}
                              ($ shape-context-menu {:data data}))))))))))))))))

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
        exclu #{:player :hidden :dead}]
    (take 4 (filter (difference (token-flags data) exclu) order))))

(defui ^:private token [{:keys [node data]}]
  (let [radii (- (/ grid-size 2) 2)
        scale (/ (:token/size data) 5)
        hashs (:image/checksum (:token/image data))
        pttrn (cond (string? hashs)      (str "token-face-" hashs)
                    :else                (str "token-face-default"))]
    ($ :g.scene-token
      {:ref node :id (str "token-" (:db/id data)) :data-flags (token-flags-attr data)}
      (let [radius (:aura/radius data)
            radius (if (> radius 0) (+ (* grid-size (/ radius 5)) (* scale (/ grid-size 2))) 0)]
        ($ :circle.scene-token-aura {:cx 0 :cy 0 :style {:r radius}}))
      ($ :g {:style {:transform (str "scale(" scale ")")}}
        ($ :circle.scene-token-shape {:cx 0 :cy 0 :r radii :fill (str "url(#" pttrn ")")})
        ($ :circle.scene-token-base {:cx 0 :cy 0 :r (+ radii 5)})
        (for [[deg flag] (mapv vector [-120 120 -65 65] (token-conditions data))
              :let [rn (* (/ js/Math.PI 180) deg)
                    cx (* (js/Math.sin rn) radii)
                    cy (* (js/Math.cos rn) radii)]]
          ($ :g.scene-token-flags {:key flag :data-flag flag :transform (str "translate(" cx ", " cy ")")}
            ($ :circle {:cx 0 :cy 0 :r 12})
            ($ :g {:transform (str "translate(" -8 ", " -8 ")")}
              ($ icon {:name (condition->icon flag) :size 16}))))
        (if-let [label (token-label data)]
          ($ :text.scene-token-label {:y (/ grid-size 2)}
            label)))
      (let [radius (+ (* scale (/ grid-size 2)) 2)]
        ($ :circle.scene-token-ring {:cx 0 :cy 0 :style {:r radius}})))))

(def ^:private tokens-defs-query
  [[:user/type :default :host]
   {:user/camera
    [{:camera/scene
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
         {:scene/_initiative [:db/id :initiative/turn]}]}]}]}])

(defui ^:private tokens-defs []
  (let [result (use-query tokens-defs-query)
        tokens (-> result :user/camera :camera/scene :scene/tokens)]
    ($ :defs
      ($ :filter {:id "token-status-dead" :filterRes 1 :color-interpolation-filters "sRGB"}
        ($ :feColorMatrix {:in "SourceGraphic" :type "saturate" :values 0 :result "Next"})
        ($ :feComponentTransfer {:in "Next"}
          ($ :feFuncR {:type "linear" :slope 0.70})
          ($ :feFuncG {:type "linear" :slope 0.70})
          ($ :feFuncB {:type "linear" :slope 0.70})))
      ($ :linearGradient {:id "token-base-player" :x1 0 :y1 0 :x2 1 :y2 1}
        ($ :stop {:style {:stop-color "#fcd34d"} :offset "0%"})
        ($ :stop {:style {:stop-color "#b45309"} :offset "100%"}))
      ($ :pattern
        {:id "token-face-default"
         :style {:color "white"}
         :width "100%"
         :height "100%"
         :viewBox "-2 -2 16 16"
         :patternContentUnits "objectBoundingBox"}
        ($ :rect {:x -2 :y -2 :width 16 :height 16 :fill "var(--color-blues-700)"})
        ($ icon {:name "dnd" :size 12}))
      ($ TransitionGroup {:component nil}
        (for [checksum (into #{} (comp (map (comp :image/checksum :token/image)) (filter some?)) tokens)
              :let [node (create-ref)]]
          ($ Transition {:key checksum :nodeRef node :timeout 240}
            ($ :pattern
              {:id (str "token-face-" checksum)
               :ref node
               :width "100%"
               :height "100%"
               :patternContentUnits "objectBoundingBox"}
              ($ image {:checksum checksum}
                (fn [{:keys [data-url]}]
                  ($ :image {:href data-url :width 1 :height 1 :preserveAspectRatio "xMidYMin slice"})))))))
      ($ TransitionGroup {:component nil}
        (for [{id :db/id :as data} tokens :let [node (create-ref)]]
          ($ Transition {:key id :nodeRef node :timeout 240}
            ($ token {:node node :data data})))))))

(def ^:private tokens-query
  [{:root/user
    [:user/type
     [:bounds/self :default [0 0 0 0]]
     {:user/camera
      [:db/id
       :camera/selected
       [:camera/scale :default 1]
       [:camera/point :default [0 0]]
       {:camera/scene
        [[:scene/grid-origin :default [0 0]]
         [:scene/grid-align :default false]
         {:scene/tokens
          [:db/id
           [:token/point :default [0 0]]
           [:token/flags :default #{}]
           [:token/label :default ""]
           [:token/size :default 5]
           [:token/light :default 15]
           [:aura/radius :default 0]
           {:token/image [:image/checksum]}
           {:scene/_initiative [:db/id :initiative/turn]}]}]}]}]
    :root/session
    [{:session/conns
      [:user/uuid :user/color :user/dragging]}]}])

(defui ^:private tokens []
  (let [dispatch (use-dispatch)
        result   (use-query tokens-query [:db/ident :root])
        {{type :user/type
          [_ _ bw bh] :bounds/self
          {scale :camera/scale
           [cx cy] :camera/point
           selected :camera/selected
           {[ox oy] :scene/grid-origin
            tokens :scene/tokens
            align? :scene/grid-align}
           :camera/scene} :user/camera} :root/user
         {conns :session/conns} :root/session} result
        ox        (mod ox grid-size)
        oy        (mod oy grid-size)
        portal    (uix/use-ref nil)
        selected? (into #{} (map :db/id) selected)
        selected  (filter (comp selected? :db/id) tokens)
        dragging  (into {} invert-drag-data-xf conns)
        sorted    (->> tokens
                       (filter (fn [token] (or (= type :host) (not ((:token/flags token) :hidden)))))
                       (sort tokens-order-cmp))]
    (use-dnd-monitor
     #js {"onDragStart"
          (use-callback
           (fn [data]
             (let [class (getValueByKeys data "active" "data" "current" "class")]
               (if (or (= class "token") (= class "tokens"))
                 (let [idxs (getValueByKeys data "active" "data" "current" "id")]
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
    ($ :g.scene-tokens
      ($ :g {:ref portal :style {:outline "none"} :tab-index -1})
      ($ TransitionGroup {:component "g" :class-name "scene-tokens-unselected"}
        (for [{id :db/id size :token/size [tx ty :as point] :token/point} sorted :let [node (create-ref)]]
          ($ CSSTransition {:key id :nodeRef node :timeout 240}
            ($ :g.scene-token-transition {:ref node}
              (if (not (selected? id))
                (let [owner (dragging id)
                      focus (point-within-rect point [cx cy (+ (/ bw scale) cx) (+ (/ bh scale) cy)])]
                  ($ render-live {:owner (:user/uuid owner) :ox tx :oy ty}
                    (fn [rx ry]
                      ($ render-drag {:id id :idxs (list id) :class "token" :disabled (some? owner)}
                        (fn [options]
                          (let [dx (getValueByKeys options "transform" "x")
                                dy (getValueByKeys options "transform" "y")
                                ax (+ tx (or rx dx 0))
                                ay (+ ty (or ry dy 0))]
                            ($ :<>
                              (if (and align? (.-isDragging options) (or (not= dx 0) (not= dy 0)))
                                (let [rd (* (/ size 5) (/ grid-size 2))
                                      bx (round-grid ax rd ox)
                                      by (round-grid ay rd oy)]
                                  ($ :g.scene-token-ghost
                                    {:transform (str "translate(" bx ", " by ")")}
                                    ($ :rect {:x (- rd) :y (- rd) :width (* rd 2) :height (* rd 2)}))))
                              ($ :g.scene-token-position
                                {:ref              (.-setNodeRef options)
                                 :transform        (str "translate(" ax ", " ay ")")
                                 :tab-index        (if focus 0 -1)
                                 :data-id          id
                                 :data-type        "token"
                                 :data-color       (:user/color owner)
                                 :data-drag-local  (.-isDragging options)
                                 :data-drag-remote (some? owner)
                                 :on-pointer-down  (or (getValueByKeys options "listeners" "onPointerDown") stop-propagation)}
                                ($ :use {:href (str "#token-" id)}))))))))))))))
      ($ use-portal {:name (if (or (= type :host) (= type :conn)) :selected)}
        ($ render-drag
          {:id       "tokens"
           :class    "tokens"
           :idxs     (seq selected?)
           :disabled (boolean (seq (intersection (set (keys dragging)) selected?)))}
          (fn [options]
            (let [dx (getValueByKeys options "transform" "x")
                  dy (getValueByKeys options "transform" "y")]
              ($ :g.scene-tokens.scene-tokens-selected
                {:ref             (.-setNodeRef options)
                 :transform       (str "translate(" (or dx 0) ", " (or dy 0) ")")
                 :on-pointer-down (or (getValueByKeys options "listeners" "onPointerDown") stop-propagation)}
                ($ TransitionGroup {:component nil}
                  (for [{id :db/id size :token/size [tx ty] :token/point} sorted
                        :let [node (create-ref) owner (dragging id)]]
                    ($ CSSTransition {:key id :nodeRef node :timeout 240}
                      ($ :g.scene-token-transition {:ref node}
                        (if (selected? id)
                          ($ render-live {:owner (:user/uuid owner) :ox tx :oy ty}
                            (fn [rx ry]
                              ($ :<>
                                (if (and align? (.-isDragging options) (or (not= dx 0) (not= dy 0)))
                                  (let [rd (* (/ size 5) (/ grid-size 2))
                                        ax (+ tx (or rx dx 0))
                                        ay (+ ty (or ry dy 0))
                                        bx (round-grid ax rd ox)
                                        by (round-grid ay rd oy)]
                                    (dom/create-portal
                                     ($ :g.scene-token-ghost
                                       {:transform (str "translate(" bx ", " by ")")}
                                       ($ :rect {:x (- rd) :y (- rd) :width (* rd 2) :height (* rd 2)}))
                                     (.-current portal))))
                                ($ :g.scene-token-position
                                  {:transform        (str "translate(" (+ tx rx) ", " (+ ty ry) ")")
                                   :data-id          id
                                   :data-type        "token"
                                   :data-color       (:user/color owner)
                                   :data-drag-local  (.-isDragging options)
                                   :data-drag-remote (some? owner)}
                                  ($ :use {:href (str "#token-" id)}))))))))))
                (if (and (seq selected) (or (= type :host) (= type :conn)))
                  (let [[ax _ bx by] (bounding-rect (mapcat :token/point selected))
                        sz 400
                        tx (-> (+ ax bx) (* scale) (- 400) (/ 2) int)
                        ty (-> (+ by 56) (* scale) (- 24) int)]
                    ($ :foreignObject.context-menu-object
                      {:x tx :y ty :width sz :height sz :transform (str "scale(" (/ scale) ")")}
                      ($ token-context-menu {:tokens selected :type type}))))))))))))

(defui ^:private player-window-bounds []
  (let [result (use-query [:bounds/host :bounds/view])
        {[_ _ hw hh] :bounds/host
         [_ _ vw vh] :bounds/view} result
        [ox oy] [(/ (- hw vw) 2) (/ (- hh vh) 2)]]
    ($ :g.scene-bounds {:transform (str "translate(" ox " , " oy ")")}
      ($ :rect {:x 0 :y 0 :width vw :height vh :rx 8}))))

(def ^:private player-cursors-query
  [{:root/user [{:user/camera [:camera/scene]}]}
   {:root/session
    [[:session/share-cursors :default true]
     {:session/host [:user/uuid]}
     {:session/conns
      [:user/uuid
       [:user/share-cursor :default true]
       [:user/color :default "none"]
       {:user/camera [:camera/scene]}]}]}])

(defui ^:private player-cursors []
  (let [result (use-query player-cursors-query [:db/ident :root])
        conns  (key-by :user/uuid (:session/conns (:root/session result)))
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
                         (:user/share-cursor conn)
                         (= (:camera/scene (:user/camera conn))
                            (:camera/scene (:user/camera (:root/user result)))))]
          ($ use-portal {:key uuid :name (if (= (:user/uuid host) (:user/uuid conn)) :host-cursor)}
            ($ :g.scene-cursor
              {:transform (str "translate(" (- x 3) ", " (- y 3) ")") :data-color (:user/color conn)}
              ($ icon {:name "cursor-fill" :size 28}))))))))

(defui ^:private scene-camera-draggable
  [{:keys [children]}]
  (let [options (use-draggable #js {"id" "scene-camera" "data" #js {"class" "camera"}})
        dx (getValueByKeys options "transform" "x")
        dy (getValueByKeys options "transform" "y")]
    ($ :g.scene-draggable
      {:id "scene-drag"
       :ref (.-setNodeRef options)
       :style {:will-change "transform"}
       :transform (str "translate(" (or dx 0) ", " (or dy 0) ")")
       :data-dragging (.-isDragging options)
       :on-pointer-down (.. options -listeners -onPointerDown)}
      ($ :rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"})
      children)))

(defui ^:private scene-camera
  [{:keys [scale on-translate children]
    :or   {on-translate identity}}]
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
        ($ dnd-context
          #js {"modifiers" #js [scale-fn dnd-modifier-int]}
          children)))))

(def ^:private scene-elements
  (uix/memo
   (uix/fn []
     ($ :<>
       ;; Defines the standard square grid pattern for the scene.
       ($ grid-defs)

       ;; Defines clip paths and masks for tokens which emit
       ;; radial lighting.
       ($ mask-defs)

       ;; Defines the scene <image> element as well as a <rect>
       ;; which has the same dimensions as the image.
       ($ image-defs)

       ;; Defines token patterns, images, and the tokens themselves.
       ($ tokens-defs)

       ($ :g.scene-exterior
         ($ :use.scene-grid {:href "#scene-grid" :style {:clip-path "unset"}}))

       ;; When the scene is using the "Obscured" lighting option,
       ;; this element becomes visible for players. It renders a
       ;; darkened and desaturated version of the scene image
       ;; that is drawn underneath the foreground scene.
       ($ :g.scene-background
         ($ :use.scene-image {:href "#scene-image"})
         ($ :use.scene-grid {:href "#scene-grid"}))

       ;; The primary scene object contains all interactable objects.
       ;; This element is clipped twice: first by tokens which emit
       ;; light around them, and then second by user-created mask
       ;; areas.
       ($ :g.scene-foreground
         ($ :g.scene-interior
           ($ :use.scene-image {:href "#scene-image"})
           ($ :use.scene-grid {:href "#scene-grid"})
           ($ shapes)
           ($ tokens))

         ;; Portal target for the host's cursor which must remain obscured
         ;; by visibility controls so that nosey players don't get any
         ;; clues about what the host may be doing on the scene.
         ($ create-portal {:name :host-cursor}
           (fn [{:keys [ref]}]
             ($ :g {:ref ref :style {:outline "none"} :tab-index -1}))))

       ;; Masking elements that fully or partially obscure the scene.
       ($ :g.scene-mask
         ($ :g.scene-mask-primary
           ($ :use.scene-mask-fill {:href "#scene-image-cover"})
           ($ :use.scene-mask-pattern {:href "#scene-image-cover"})
           ($ :use.scene-grid {:href "#scene-grid"}))
         ($ :g.scene-mask-secondary
           ($ :use.scene-mask-fill {:href "#scene-image-cover"})
           ($ :use.scene-mask-pattern {:href "#scene-image-cover"})
           ($ :use.scene-grid {:href "#scene-grid"})))

       ;; Portal target for selected tokens and shapes. This brings
       ;; selected objects to the foreground so they are not obscured
       ;; by visibility.
       ($ create-portal {:name :selected}
         (fn [{:keys [ref]}]
           ($ :g {:ref ref :style {:outline "none"} :tab-index -1})))

       ;; Player cursors are rendered on top of everything else.
       ($ player-cursors)

       ;; Renders mask area boundaries, allowing the host to interact
       ;; with them individually.
       ($ mask-polys)))))

(def ^:private scene-query
  [:user/type
   [:user/privileged? :default false]
   [:bounds/host :default [0 0 0 0]]
   [:bounds/view :default [0 0 0 0]]
   {:user/camera
    [:db/id
     [:camera/point :default [0 0]]
     [:camera/scale :default 1]
     [:camera/draw-mode :default :select]
     {:camera/scene
      [[:scene/dark-mode :default false]
       [:scene/show-grid :default true]
       [:scene/lighting :default :revealed]
       [:scene/masked :default false]]}]}])

(defui scene []
  (let [dispatch (use-dispatch)
        result   (use-query scene-query)
        {user        :user/type
         [_ _ hw hh] :bounds/host
         [_ _ vw vh] :bounds/view
         {id      :db/id
          scene   :camera/scene
          scale   :camera/scale
          mode    :camera/draw-mode
          [cx cy] :camera/point} :user/camera} result
        cx (if (= user :view) (->> (- hw vw) (max 0) (* (/ -1 2 scale)) (- cx)) cx)
        cy (if (= user :view) (->> (- hh vh) (max 0) (* (/ -1 2 scale)) (- cy)) cy)
        multi-select? (use-key "shift")]
    ($ :svg.scene
      {:key id
       :data-user   (name user)
       :data-grid   (or (= mode :grid) (:scene/show-grid scene))
       :data-theme  (if (:scene/dark-mode scene) "dark" "light")
       :data-light  (name (:scene/lighting scene))
       :data-masked (:scene/masked scene)}
      ($ :g.scene-handler {:data-type "scene" :tab-index 0 :style {:outline "none"}})
      ($ scene-camera
        {:scale scale
         :on-translate
         (use-callback
          (fn [dx dy]
            (if (and (= dx 0) (= dy 0))
              (dispatch :selection/clear)
              (dispatch :camera/translate (- dx) (- dy))))
          [dispatch cx cy scale])}
        (if (and (= mode :select) multi-select?)
          ($ draw {:mode :select}))
        ($ :g {:transform (str "scale(" scale ") translate(" (- cx) ", " (- cy) ")")}
          ($ scene-elements)))
      ($ create-portal {:name :multiselect}
        (fn [{:keys [ref]}]
          ($ :g.scene-drawable.scene-drawable-select
            {:ref ref :style {:outline "none"} :tab-index -1})))
      (if (contains? draw-modes mode)
        ($ :g.scene-drawable {:class (str "scene-drawable-" (name mode))}
          ($ draw {:key mode :mode mode :node nil})))
      (if (:user/privileged? result) ($ player-window-bounds)))))
