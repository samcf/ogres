(ns ogres.app.component.scene
  (:require [clojure.set :refer [difference]]
            [clojure.string :refer [join]]
            [goog.object :refer [getValueByKeys]]
            [ogres.app.component :refer [icon image]]
            [ogres.app.component.scene-draw :refer [draw]]
            [ogres.app.component.scene-objects :refer [objects]]
            [ogres.app.component.scene-pattern :refer [pattern]]
            [ogres.app.const :refer [grid-size half-size]]
            [ogres.app.hooks :as hooks]
            [ogres.app.svg :refer [circle->path poly->path]]
            [ogres.app.util :refer [key-by]]
            [ogres.app.vec :as vec :refer [Vec2]]
            [react-transition-group :refer [TransitionGroup Transition CSSTransition]]
            [uix.core :as uix :refer [defui $]]
            ["@rwh/react-keystrokes" :refer [useKey] :rename {useKey use-key}]
            ["@dnd-kit/core"
             :refer [DndContext useDraggable]
             :rename {DndContext dnd-context useDraggable use-draggable}]))

(defn ^:private token-light-xf [user]
  (comp (filter
         (fn [token]
           (and (> (:token/light token) 0)
                (or (not (:object/hidden token))
                    (= user :host)))))
        (map
         (fn [token]
           [(.-x (:object/point token))
            (.-y (:object/point token))
            (+ (/ (* (:token/light token) grid-size) 5) grid-size)]))))

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
   :paralyzed     "cobra"
   :petrified     "gem"
   :poisoned      "poison-bottle"
   :prone         "tripwire"
   :restrained    "cobweb"
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
      [[:scene/grid-size :default grid-size]
       [:scene/grid-origin :default vec/zero]
       [:scene/lighting :default :revealed]
       {:scene/image [:image/hash :image/width :image/height]}]}]}])

(defui ^:private image-defs []
  (let [result (hooks/use-query image-defs-query)
        {{{{width  :image/width
            height :image/height
            hash :image/hash} :scene/image
           origin :scene/grid-origin
           size :scene/grid-size} :camera/scene} :user/camera} result
        transform
        (str (vec/rnd (vec/mul origin -1)) " "
             "scale(" (/ grid-size size) ")")]
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
              {:id "scene-image" :href url :width width :height height :transform transform}))))
      ($ :rect {:id "scene-image-cover" :width width :height height :transform transform})
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
         [:object/point :default vec/zero]
         [:object/hidden :default false]
         [:token/light :default 15]]}]}]}])

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

(defui ^:private grid-defs []
  ($ :defs
    ($ :pattern
      {:id "grid-pattern"
       :width grid-size
       :height grid-size
       :patternUnits "userSpaceOnUse"}
      ($ :path.scene-grid-path {:d "M 0 0 H 70 V 70"}))
    ($ :g {:id "scene-grid"}
      ($ :path
        {:d "M -100000 -100000 H 100000 V 100000 H -100000"
         :fill "url(#grid-pattern)"}))))

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
        exclu #{:player :dead}]
    (take 4 (filter (difference (token-flags data) exclu) order))))

(defui ^:private token [{:keys [node data]}]
  (let [radius (- half-size 2)
        scale (/ (:token/size data) 5)
        hash (:image/hash (:image/thumbnail (:token/image data)))
        fill (if (some? hash) (str "token-face-" hash) "token-face-default")]
    ($ :g.scene-token
      {:ref node
       :id (str "token" (:db/id data))
       :data-flags (token-flags-attr data)
       :data-hidden (:object/hidden data)}
      (let [radius (:token/aura-radius data)
            radius (if (> radius 0) (+ (* grid-size (/ radius 5)) (* scale half-size)) 0)]
        ($ :circle.scene-token-aura {:style {:r radius}}))
      ($ :g {:style {:transform (str "scale(" scale ")")}}
        ($ :circle.scene-token-shape {:r radius :fill (str "url(#" fill ")")})
        ($ :circle.scene-token-base {:r (+ radius 5)})
        (for [[deg flag] (mapv vector [-120 120 -65 65] (token-conditions data))
              :let [rn (* (/ js/Math.PI 180) deg)
                    cx (* (js/Math.sin rn) radius)
                    cy (* (js/Math.cos rn) radius)]]
          ($ :g.scene-token-flags {:key flag :data-flag flag :transform (str "translate(" cx ", " cy ")")}
            ($ :circle {:r 12})
            ($ :g {:transform (str "translate(" -8 ", " -8 ")")}
              ($ icon {:name (condition->icon flag) :size 16}))))
        (if-let [label (token-label data)]
          ($ :text.scene-token-label {:y half-size} label)))
      (let [radius (+ (* scale half-size) 2)]
        ($ :circle.scene-token-ring {:style {:r radius}})))))

(def ^:private tokens-defs-query
  [[:user/type :default :host]
   {:user/camera
    [{:camera/scene
      [{:scene/tokens
        [:db/id
         [:initiative/suffix :default nil]
         [:object/point :default vec/zero]
         [:object/hidden :default false]
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
                    (if (some? url)
                      ($ :image {:href url :width 1 :height 1 :preserveAspectRatio "xMidYMin slice"})
                      ($ :rect {:width 256 :height 256 :fill "var(--color-blues-900)"})))))))))
      ($ TransitionGroup {:component nil}
        (for [{id :db/id :as data} tokens :let [node (uix/create-ref)]]
          ($ Transition {:key id :nodeRef node :timeout 240}
            ($ token {:node node :data data})))))))

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
      ($ :rect {:width "100%" :height "100%" :fill "transparent"})
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
              (let [x (.. data -delta -x)
                    y (.. data -delta -y)]
                (on-translate
                 (Vec2. x y)))) [on-translate])}
      ($ scene-camera-draggable
        ($ dnd-context
          #js {"modifiers" #js [scale-fn dnd-modifier-int]}
          children)))))

(defui ^:private ^:memo scene-elements []
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
    ($ mask-polys)))

(def ^:private scene-query
  [:user/type
   {:user/camera
    [:db/id
     [:camera/point :default vec/zero]
     [:camera/scale :default 1]
     [:camera/draw-mode :default :select]
     {:camera/scene
      [:db/id
       [:scene/dark-mode :default false]
       [:scene/show-grid :default true]
       [:scene/lighting :default :revealed]
       [:scene/masked :default false]
       {:scene/image [:image/hash]}]}]}])

(defui ^:private scene-content [props]
  (let [dispatch (hooks/use-dispatch)
        {user :user/type
         {scene :camera/scene
          scale :camera/scale
          mode  :camera/draw-mode
          point :camera/point} :user/camera} (:data props)
        multi-select? (use-key "shift")]
    ($ :svg.scene
      {:ref (:ref props)
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
          (fn [delta]
            (if (= delta vec/zero)
              (dispatch :selection/clear)
              (dispatch :camera/translate (vec/mul delta -1))))
          [dispatch scale])}
        (if (and (= mode :select) multi-select?)
          ($ :g.scene-draw {:data-type "select"}
            ($ draw {:mode :select})))
        ($ :g {:transform  (str "scale(" scale ") " (vec/rnd (vec/mul point -1)))}
          (:children props)))
      ($ hooks/create-portal {:name :multiselect}
        (fn [{:keys [ref]}]
          ($ :g.scene-draw
            {:ref ref :data-type "select" :tab-index -1})))
      (if (contains? draw-modes mode)
        ($ :g.scene-draw {:data-type (name mode)}
          ($ draw {:key mode :mode mode :node nil}))))))

(defui ^:private scene-transition [props]
  (let [id   (-> props :data :user/camera :camera/scene :db/id)
        hash (-> props :data :user/camera :camera/scene :scene/image :image/hash)
        url  (hooks/use-image hash)]
    ($ CSSTransition
      {:key (str "id:" id "/" "hash:" hash)
       :nodeRef (:ref props)
       :in (or (nil? hash) (and (some? hash) (some? url)))
       :timeout 250
       :unmountOnExit true
       :mountOnEnter true
       :appear true}
      (:children props))))

(defui ^:memo scene []
  (let [res (hooks/use-query scene-query)
        ref (uix/use-ref)]
    ($ scene-transition {:ref ref :data res}
      ($ scene-content {:ref ref :data res}
        ($ scene-elements)))))
