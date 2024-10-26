(ns ogres.app.component.scene
  (:require [clojure.set :refer [difference]]
            [clojure.string :refer [join]]
            [goog.object :refer [getValueByKeys]]
            [ogres.app.component :refer [icon image]]
            [ogres.app.component.scene-draw :refer [draw]]
            [ogres.app.component.scene-objects :refer [objects]]
            [ogres.app.component.scene-pattern :refer [pattern]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.hooks :as hooks]
            [ogres.app.svg :refer [circle->path poly->path]]
            [ogres.app.util :refer [key-by]]
            [react-transition-group :refer [TransitionGroup Transition]]
            [uix.core :as uix :refer [defui $]]
            ["@rwh/react-keystrokes" :refer [useKey] :rename {useKey use-key}]
            ["@dnd-kit/core"
             :refer [DndContext useDraggable]
             :rename {DndContext dnd-context useDraggable use-draggable}]))

(defn ^:private token-light-xf [user]
  (comp (filter (fn [{radius :token/light flags :token/flags}]
                  (and (> radius 0) (or (= user :host) (not (flags :hidden))))))
        (map (fn [{[x y] :object/point radius :token/light}]
               [x y (+ (/ (* radius grid-size) 5) grid-size)]))))

(def ^:private mask-area-xf
  (comp (filter :mask/enabled?) (map :mask/vecs)))

(def ^:private draw-modes
  #{:grid :ruler :circle :rect :cone :line :poly :mask :note})

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

(def ^:private image-defs-query
  [{:user/camera
    [{:camera/scene
      [[:scene/grid-size :default 70]
       [:scene/lighting :default :revealed]
       {:scene/image [:image/hash :image/width :image/height]}]}]}])

(defui ^:private image-defs []
  (let [result (hooks/use-query image-defs-query)
        {{{{width  :image/width
            height :image/height
            hash :image/hash} :scene/image
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
      (if (some? hash)
        ($ image {:hash hash}
          (fn [url]
            ($ :image
              {:x 0 :y 0 :id "scene-image" :ref node :href url :width width :height height :transform transform}))))
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
         [:object/point :default [0 0]]]}]}]}])

(defui ^:private mask-defs []
  (let [result (hooks/use-query mask-defs-query)
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
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query mask-polys-query)
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
  (let [data (hooks/use-query grid-defs-query)
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
  (let [radius (- (/ grid-size 2) 2)
        scale  (/ (:token/size data) 5)
        hash   (:image/hash (:image/thumbnail (:token/image data)))
        fill   (if (some? hash)
                 (str "token-face-" hash)
                 (str "token-face-default"))]
    ($ :g.scene-token
      {:ref node :id (str "token" (:db/id data)) :data-flags (token-flags-attr data)}
      (let [radius (:token/aura-radius data)
            radius (if (> radius 0) (+ (* grid-size (/ radius 5)) (* scale (/ grid-size 2))) 0)]
        ($ :circle.scene-token-aura {:cx 0 :cy 0 :style {:r radius}}))
      ($ :g {:style {:transform (str "scale(" scale ")")}}
        ($ :circle.scene-token-shape {:cx 0 :cy 0 :r radius :fill (str "url(#" fill ")")})
        ($ :circle.scene-token-base {:cx 0 :cy 0 :r (+ radius 5)})
        (for [[deg flag] (mapv vector [-120 120 -65 65] (token-conditions data))
              :let [rn (* (/ js/Math.PI 180) deg)
                    cx (* (js/Math.sin rn) radius)
                    cy (* (js/Math.cos rn) radius)]]
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
         [:object/point :default [0 0]]
         [:token/flags :default #{}]
         [:token/label :default ""]
         [:token/size :default 5]
         [:token/light :default 15]
         [:token/aura-radius :default 0]
         {:token/image [{:image/thumbnail [:image/hash]}]}
         {:scene/_initiative [:db/id :initiative/turn]}]}]}]}])

(defui ^:private tokens-defs []
  (let [result (hooks/use-query tokens-defs-query)
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
        (let [xf (comp (map (comp :image/hash :image/thumbnail :token/image)) (filter some?))]
          (for [hash (into #{} xf tokens)
                :let [node (uix/create-ref)]]
            ($ Transition {:key hash :nodeRef node :timeout 240}
              ($ :pattern
                {:id (str "token-face-" hash)
                 :ref node
                 :width "100%"
                 :height "100%"
                 :patternContentUnits "objectBoundingBox"}
                ($ image {:hash hash}
                  (fn [url]
                    ($ :image {:href url :width 1 :height 1 :preserveAspectRatio "xMidYMin slice"}))))))))
      ($ TransitionGroup {:component nil}
        (for [{id :db/id :as data} tokens :let [node (uix/create-ref)]]
          ($ Transition {:key id :nodeRef node :timeout 240}
            ($ token {:node node :data data})))))))

(defui ^:private player-window-bounds []
  (let [result (hooks/use-query [:bounds/host :bounds/view])
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
  (let [result (hooks/use-query player-cursors-query [:db/ident :root])
        conns  (key-by :user/uuid (:session/conns (:root/session result)))
        share  (:session/share-cursors (:root/session result))
        host   (:session/host (:root/session result))
        [points set-points] (uix/use-state {})]
    (hooks/use-subscribe :cursor/moved
      (uix/use-callback
       (fn [uuid x y]
         (if share
           (set-points (fn [points] (assoc points uuid [x y]))))) [share]))
    (if share
      ($ :g.scene-cursors
        (for [[uuid [x y]] points
              :let [conn (conns uuid)]
              :when (and (some? conn)
                         (:user/share-cursor conn)
                         (= (:camera/scene (:user/camera conn))
                            (:camera/scene (:user/camera (:root/user result)))))]
          ($ hooks/use-portal {:key uuid :name (if (= (:user/uuid host) (:user/uuid conn)) :host-cursor)}
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
  (let [scale-fn (uix/use-memo (fn [] (dnd-modifier-scale scale)) [scale])]
    ($ dnd-context
      #js {"modifiers" #js [dnd-modifier-int]
           "onDragEnd"
           (uix/use-callback
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
           ($ objects))

         ;; Portal target for the host's cursor which must remain obscured
         ;; by visibility controls so that nosey players don't get any
         ;; clues about what the host may be doing on the scene.
         ($ hooks/create-portal {:name :host-cursor}
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
       ($ hooks/create-portal {:name :selected}
         (fn [{:keys [ref]}]
           ($ :g {:ref ref :style {:outline "none"} :tab-index -1})))

       ;; Player cursors are rendered on top of everything else.
       ($ player-cursors)

       ;; Renders mask area boundaries, allowing the host to interact
       ;; with them individually.
       ($ mask-polys)))))

(def ^:private scene-query
  [:user/type
   :user/sharing?
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
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query scene-query)
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
       :data-grid   (and (not= mode :grid) (:scene/show-grid scene))
       :data-theme  (if (:scene/dark-mode scene) "dark" "light")
       :data-light  (name (:scene/lighting scene))
       :data-masked (:scene/masked scene)}
      ($ :g.scene-handler {:data-type "scene" :tab-index 0 :style {:outline "none"}})
      ($ scene-camera
        {:scale scale
         :on-translate
         (uix/use-callback
          (fn [dx dy]
            (if (and (= dx 0) (= dy 0))
              (dispatch :selection/clear)
              (dispatch :camera/translate (- dx) (- dy))))
          [dispatch cx cy scale])}
        (if (and (= mode :select) multi-select?)
          ($ draw {:mode :select}))
        ($ :g {:transform (str "scale(" scale ") translate(" (- cx) ", " (- cy) ")")}
          ($ scene-elements)))
      ($ hooks/create-portal {:name :multiselect}
        (fn [{:keys [ref]}]
          ($ :g.scene-drawable.scene-drawable-select
            {:ref ref :style {:outline "none"} :tab-index -1})))
      (if (contains? draw-modes mode)
        ($ :g.scene-drawable {:class (str "scene-drawable-" (name mode))}
          ($ draw {:key mode :mode mode :node nil})))
      (if (and (= user :host) (:user/sharing? result))
        ($ player-window-bounds)))))
