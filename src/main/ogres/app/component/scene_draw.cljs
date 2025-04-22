(ns ogres.app.component.scene-draw
  (:require [clojure.string :refer [join]]
            [ogres.app.component :refer [icon]]
            [ogres.app.const :refer [grid-size half-size]]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [ogres.app.util :as util]
            [ogres.app.vec :as vec :refer [Segment Vec2]]
            [uix.core :as uix :refer [defui $]]
            ["@dnd-kit/core"
             :refer  [DndContext useDraggable useDndMonitor]
             :rename {DndContext    dnd-context
                      useDndMonitor use-dnd-monitor
                      useDraggable  use-draggable}]))

(defmulti align-identity type)
(defmethod align-identity Vec2 [a] a)
(defmethod align-identity Segment [s] s)

(defmulti align-grid type)
(defmethod align-grid Vec2 [a]
  (vec/rnd a grid-size))
(defmethod align-grid Segment [s]
  (vec/map align-grid s))

(defmulti align-grid-half type)
(defmethod align-grid-half Vec2 [a] (vec/rnd a half-size))
(defmethod align-grid-half Segment [s]
  (vec/map align-grid-half s))

(defmulti align-line type)
(defmethod align-line Vec2 [a] (align-grid-half a))
(defmethod align-line Segment [s]
  (let [src (align-grid-half (.-a s))
        dir (vec/sub (.-b s) src)
        len (vec/dist vec/zero dir)]
    (if (= len 0)
      (Segment. src src)
      (let [dst (-> (vec/div dir len) (vec/mul (util/round len grid-size)) (vec/add src))]
        (Segment. src dst)))))

(defmulti align-cone type)
(defmethod align-cone Vec2 [a] (align-grid a))
(defmethod align-cone Segment [s]
  (let [src (align-grid (.-a s))
        dir (vec/sub (.-b s) src)
        len (vec/dist vec/zero dir)]
    (if (= len 0)
      (Segment. src src)
      (let [dst (-> (vec/div dir len) (vec/mul (util/round len grid-size)) (vec/add src))]
        (Segment. src dst)))))

(defmulti proj-camera type)
(defmethod proj-camera Vec2 [a o t x]
  (vec/add (vec/div (vec/sub a o) x) t))
(defmethod proj-camera Segment [s o t x]
  (vec/map (fn [v] (proj-camera v o t x)) s))

(defmulti proj-canvas type)
(defmethod proj-canvas Vec2 [a t x]
  (vec/mul (vec/sub a t) x))
(defmethod proj-canvas Segment [s t x]
  (vec/map (fn [v] (proj-canvas v t x)) s))

(def ^:private points->poly
  (completing into (fn [xs] (join " " xs))))

(defn ^:private points->canvas [t x]
  (comp (map (fn [v] (proj-canvas v t x)))
        (map seq)))

(defn ^:private px->ft [len]
  (let [ft (* (/ len grid-size) 5)
        rd (js/Math.round ft)]
    (if (< (abs (- ft rd)) 0.001) rd
        (.toFixed ft 1))))

(defui ^:private text
  [{:keys [attrs children]}]
  ($ :text.scene-text attrs children))

(defui ^:private anchor [props]
  ($ :g {:transform (:transform props)}
    ($ :circle.scene-draw-anchor {:r 4})
    ($ :circle.scene-draw-anchor-ring {:r 6})))

(defui ^:private draw-segment-drag [props]
  (let [{:keys [children on-release use-cursor]} props
        [segment set-segment] (uix/use-state nil)
        [cursor   set-cursor] (uix/use-state nil)
        options (use-draggable #js {"id" "drawable"})
        on-down (.. options -listeners -onPointerDown)
        on-stop
        (fn []
          (when (not (nil? segment))
            (set-segment nil)
            (set-cursor nil)
            (on-release segment)))
        on-drag
        (uix/use-callback
         (fn [data]
           (let [dx (.-x (.-delta data))
                 dy (.-y (.-delta data))
                 mx (.-clientX (.-activatorEvent data))
                 my (.-clientY (.-activatorEvent data))]
             (set-segment
              (fn [s]
                (if (some? s)
                  (Segment. (.-a s) (vec/add (.-a s) (Vec2. dx dy)))
                  (Segment. (Vec2. mx my) (Vec2. mx my))))))) [])
        on-move
        (uix/use-callback
         (fn [event]
           (set-cursor (Vec2. (.-clientX event) (.-clientY event)))) [])]
    (use-dnd-monitor
     #js {"onDragMove" on-drag
          "onDragEnd"  on-stop})
    ($ :<>
      ($ :rect.scene-draw-surface
        {:ref (.-setNodeRef options)
         :on-pointer-down on-down
         :on-pointer-move (if (and use-cursor (nil? segment)) on-move)})
      (children segment cursor))))

(def ^:private query
  [[:bounds/self :default [0 0 0 0]]
   {:user/camera
    [[:camera/scale :default 1]
     [:camera/point :default [0 0]]
     {:camera/scene
      [[:scene/grid-align :default false]
       [:scene/grid-size :default grid-size]
       [:scene/grid-origin :default [0 0]]
       [:scene/show-object-outlines :default true]]}]}])

(defui ^:private draw-segment [props]
  (let [{:keys [children on-release tile-path align-fn]
         :or {on-release :default align-fn align-identity}} props
        result (hooks/use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale :camera/scale
          {grid-paths :scene/show-object-outlines
           grid-align :scene/grid-align}
          :camera/scene}
         :user/camera} result
        align (if grid-align align-fn align-identity)
        basis (Vec2. ox oy)
        shift (Vec2. tx ty)]
    ($ draw-segment-drag
      {:use-cursor (contains? props :align-fn)
       :on-release
       (uix/use-callback
        (fn [segment]
          (on-release (align (proj-camera segment basis shift scale))))
        [on-release align basis shift scale])}
      (uix/use-callback
       (fn [segment cursor]
         (cond (some? segment)
               (let [segment (align (proj-camera segment basis shift scale))]
                 ($ :<>
                   (if (and (fn? tile-path) grid-paths)
                     (let [path (tile-path segment)]
                       ($ :polygon.scene-draw-tile-path
                         {:points (transduce (points->canvas shift scale) points->poly [] path)})))
                   (children segment (proj-canvas segment shift scale))))
               (and grid-align (some? cursor))
               ($ anchor
                 {:transform
                  (-> (proj-camera cursor basis shift scale)
                      (align)
                      (proj-canvas shift scale)
                      (vec/to-translate))})))
       [children grid-paths grid-align tile-path align basis shift scale]))))

(defui ^:private polygon
  [{:keys [on-create]}]
  (let [result (hooks/use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale :camera/scale
          {align? :scene/grid-align} :camera/scene} :user/camera} result
        [points set-points] (uix/use-state [])
        [cursor set-cursor] (uix/use-state nil)
        closing? (and (seq points) (some? cursor) (< (vec/dist (first points) cursor) 32))
        basis (Vec2. ox oy)
        shift (Vec2. tx ty)]
    ($ :<>
      ($ :rect.scene-draw-surface
        {:on-pointer-down
         (fn [event]
           (.stopPropagation event))
         :on-pointer-move
         (fn [event]
           (-> (Vec2. (.-clientX event) (.-clientY event))
               (proj-camera basis shift scale)
               (vec/rnd (if align? half-size 1))
               (set-cursor)))
         :on-click
         (fn [event]
           (if (not closing?)
             (set-points (conj points cursor))
             (let [xs (geom/reorient (mapcat (fn [v] [(.-x v) (.-y v)]) points))
                   xf (comp (partition-all 2) (map (fn [[x y]] (Vec2. x y))))]
               (set-points [])
               (set-cursor nil)
               (on-create event (into [] xf xs)))))})
      (if (and align? (not closing?) (some? cursor))
        ($ anchor {:transform (vec/to-translate (proj-canvas cursor shift scale))}))
      (if (seq points)
        ($ :circle.scene-draw-point-ring
          {:transform (vec/to-translate (proj-canvas (first points) shift scale)) :r 6}))
      (for [point points :let [point (proj-canvas point shift scale)]]
        ($ :circle.scene-draw-point
          {:key point :transform (vec/to-translate point) :r 4}))
      (if (and (seq points) (some? cursor))
        ($ :polygon.scene-draw-shape
          {:points
           (transduce
            (comp (map (fn [v] (proj-canvas v (Vec2. tx ty) scale)))
                  (map (fn [v] [(.-x v) (.-y v)])))
            points->poly []
            (if (not closing?) (conj points cursor) points))})))))

(defui ^:private draw-select []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:on-release
       (uix/use-callback
        (fn [s]
          (dispatch :selection/from-rect s)) [dispatch])}
      (uix/use-callback
       (fn [_ canvas]
         (let [a (.-a canvas) b (.-b canvas)]
           ($ hooks/use-portal {:name :multiselect}
             ($ :path.scene-draw-shape
               {:d (join " " [\M (.-x a) (.-y a) \H (.-x b) \V (.-y b) \H (.-x a) \Z])})))) []))))

(defui ^:private draw-ruler []
  ($ draw-segment
    {:align-fn align-grid-half}
    (uix/use-callback
     (fn [camera canvas]
       (let [a (.-a canvas) b (.-b canvas)]
         ($ :<>
           ($ :line.scene-draw-shape
             {:x1 (.-x a)
              :y1 (.-y a)
              :x2 (.-x b)
              :y2 (.-y b)})
           ($ anchor {:transform (vec/to-translate a)})
           ($ anchor {:transform (vec/to-translate b)})
           ($ text
             {:attrs
              {:x (- (.-x b) 48)
               :y (- (.-y b) 8)}}
             (str (px->ft (vec/dist-cheb camera)) "ft."))))) [])))

(defui ^:private draw-circle []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:align-fn align-grid
       :on-release
       (uix/use-callback
        (fn [s]
          (dispatch :shape/create :circle (seq s))) [dispatch])
       :tile-path
       (uix/use-callback
        (fn [s]
          (let [r (vec/dist-cheb s)]
            (geom/tile-path-circle (.-a s) r))) [])}
      (uix/use-callback
       (fn [camera canvas]
         (let [src (.-a canvas)]
           ($ :<>
             ($ :circle.scene-draw-shape
               {:transform (vec/to-translate src) :r (vec/dist-cheb canvas)})
             ($ text {:attrs {:x (.-x src) :y (.-y src) :fill "white"}}
               (str (px->ft (vec/dist-cheb camera)) "ft. radius"))))) []))))

(defui ^:private draw-rect []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:align-fn align-grid
       :on-release
       (uix/use-callback
        (fn [s]
          (dispatch :shape/create :rect (seq s))) [dispatch])}
      (uix/use-callback
       (fn [camera canvas]
         (let [a (.-a camera) b (.-b camera)
               c (.-a canvas) d (.-b canvas)]
           ($ :<>
             ($ :path.scene-draw-shape
               {:d (join " " [\M (.-x c) (.-y c) \H (.-x d) \V (.-y d) \H (.-x c) \Z])})
             ($ text {:attrs {:x (+ (.-x c) 8) :y (- (.-y c) 8) :fill "white"}}
               (let [v (vec/abs (vec/sub a b))]
                 (str (px->ft (.-x v)) "ft. x "
                      (px->ft (.-y v)) "ft.")))))) []))))

(defui ^:private draw-line []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:align-fn align-line
       :on-release
       (uix/use-callback
        (fn [s]
          (dispatch :shape/create :line (seq s))) [dispatch])
       :tile-path
       (uix/use-callback
        (fn [s]
          (geom/tile-path-line (geom/line-points s))) [])}
      (uix/use-callback
       (fn [camera canvas]
         ($ :<>
           (let [points (geom/line-points canvas)]
             ($ :polygon.scene-draw-shape
               {:points (join " " (mapcat seq points))}))
           ($ text
             {:attrs
              {:x (.-x (.-a canvas))
               :y (.-y (.-a canvas))}}
             (str (px->ft (vec/dist camera)) "ft.")))) []))))

(defui ^:private draw-cone []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:align-fn align-cone
       :on-release
       (uix/use-callback
        (fn [s]
          (dispatch :shape/create :cone (seq s))) [dispatch])
       :tile-path
       (uix/use-callback
        (fn [s]
          (geom/tile-path-cone (geom/cone-points s))) [])}
      (uix/use-callback
       (fn [camera canvas]
         ($ :<>
           (let [points (geom/cone-points canvas)]
             ($ :polygon.scene-draw-shape
               {:points (join " " (mapcat seq points))}))
           ($ text
             {:attrs
              {:x (+ (.-x (.-b canvas)) 16)
               :y (+ (.-y (.-b canvas)) 16)}}
             (str (px->ft (vec/dist camera)) "ft.")))) []))))

(defui ^:private draw-poly []
  (let [dispatch (hooks/use-dispatch)]
    ($ polygon
      {:on-create
       (fn [_ points]
         (dispatch :shape/create :poly points))})))

(defui ^:private draw-mask []
  (let [dispatch (hooks/use-dispatch)]
    ($ polygon
      {:on-create
       (fn [_ points]
         (dispatch :mask/create (mapcat seq points)))})))

(defui ^:private draw-grid []
  (let [dispatch (hooks/use-dispatch)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale   :camera/scale
          {prev-size :scene/grid-size
           [px py] :scene/grid-origin}
          :camera/scene} :user/camera} (hooks/use-query query)
        [origin set-origin] (uix/use-state nil)
        [size     set-size] (uix/use-state prev-size)
        basis (Vec2. ox oy)
        shift (Vec2. tx ty)
        on-shift (fn [a] (fn [] (set-origin (fn [b] (vec/add a b)))))]
    ($ :g.grid-align
      ($ :rect.scene-draw-surface
        {:on-click
         (fn [event]
           (set-origin (Vec2. (.-clientX event) (.-clientY event))))})
      (if (some? origin)
        (let [rows 7
              draw (* size scale (/ grid-size prev-size))
              wide (* draw (inc rows))
              path (for [step (range (- rows) (inc rows))]
                     (str "M " (* step draw) " " (- wide)      " " \V " " wide " "
                          "M " (- wide)      " " (* step draw) " " \H " " wide " "))]
          ($ :g {:transform (vec/to-translate (vec/sub origin basis))}
            ($ :path.grid-align-path {:d (join path)})
            ($ :circle.grid-align-center {:r 6})
            ($ :foreignObject.grid-align-form
              {:x -128 :y -128 :width 256 :height 256}
              ($ :form
                {:on-submit
                 (fn [event]
                   (.preventDefault event)
                   (let [point (-> (vec/sub origin basis) (vec/div scale) (vec/add shift)
                                   (vec/add (Vec2. px py)) (vec/abs) (vec/mod grid-size)
                                   (vec/rnd 0.25))]
                     (dispatch :scene/apply-grid-options [(.-x point) (.-y point)] size)))}
                ($ :fieldset.grid-align-origin
                  ($ :button
                    {:type "button" :data-name "up" :on-click (on-shift (Vec2. 0 -1))}
                    ($ icon {:name "arrow-up-short" :size 20}))
                  ($ :button
                    {:type "button" :data-name "right" :on-click (on-shift (Vec2. 1 0))}
                    ($ icon {:name "arrow-right-short" :size 20}))
                  ($ :button
                    {:type "button" :data-name "down" :on-click (on-shift (Vec2. 0 1))}
                    ($ icon {:name "arrow-down-short" :size 20}))
                  ($ :button
                    {:type "button" :data-name "left" :on-click (on-shift (Vec2. -1 0))}
                    ($ icon {:name "arrow-left-short" :size 20}))
                  (if (and (not= px 0) (not= py 0))
                    ($ :button
                      {:type "button" :data-name "clear" :data-tooltip "Reset"
                       :on-click (fn [] (dispatch :scene/reset-grid-origin))}
                      ($ icon {:name "x-circle-fill" :size 16}))))
                ($ :fieldset.grid-align-size
                  ($ :button
                    {:type "button" :data-name "dec" :on-click (fn [] (set-size dec))}
                    ($ icon {:name "dash" :size 20}))
                  ($ :button
                    {:type "button" :data-name "inc" :on-click (fn [] (set-size inc))}
                    ($ icon {:name "plus" :size 20}))
                  ($ :input.text.text-ghost
                    {:type "number"
                     :value size
                     :style {:color "white"}
                     :on-change
                     (fn [event]
                       (let [n (js/Number (.. event -target -value))]
                         (if (number? n)
                           (set-size n))))})
                  ($ :button {:type "submit" :data-name "submit"}
                    ($ icon {:name "check"})))))))))))

(defui ^:private draw-note []
  (let [dispatch (hooks/use-dispatch)]
    ($ :rect.scene-draw-surface
      {:on-click
       (fn [event]
         (dispatch :note/create (.-clientX event) (.-clientY event)))})))

(defui draw [{:keys [mode] :as props}]
  ($ dnd-context
    (case mode
      :circle ($ draw-circle props)
      :cone   ($ draw-cone props)
      :grid   ($ draw-grid props)
      :line   ($ draw-line props)
      :mask   ($ draw-mask props)
      :note   ($ draw-note props)
      :poly   ($ draw-poly props)
      :rect   ($ draw-rect props)
      :ruler  ($ draw-ruler props)
      :select ($ draw-select props))))
