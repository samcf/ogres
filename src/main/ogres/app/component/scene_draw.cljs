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

(def ^:private epsilon 0.01)

(defn ^:private xf-identity
  ([a] a)
  ([a b] (Segment. a b)))

(defn ^:private xf-align
  ([a]   (vec/round a grid-size))
  ([a b] (Segment. (xf-align a) (xf-align b))))

(defn ^:private xf-align-half
  ([a]   (vec/round a half-size))
  ([a b] (Segment. (xf-align-half a) (xf-align-half b))))

(defn ^:private xf-align-line
  ([a] (xf-align-half a))
  ([a b]
   (let [src (xf-align-half a)
         dir (vec/sub b src)
         len (vec/dist vec/zero dir)]
     (if (= len 0)
       (Segment. src src)
       (let [dst (-> (vec/div dir len) (vec/mul (util/round len grid-size)) (vec/add src))]
         (Segment. src dst))))))

(defn ^:private xf-align-cone
  ([a] (xf-align a))
  ([a b]
   (let [src (xf-align a)
         dir (vec/sub b src)
         len (vec/dist vec/zero dir)]
     (if (= len 0)
       (Segment. src src)
       (let [dst (-> (vec/div dir len) (vec/mul (util/round len grid-size)) (vec/add src))]
         (Segment. src dst))))))

(defn ^:private xf-camera [a o t x]
  (vec/add (vec/div (vec/sub a o) x) t))

(defn ^:private xf-canvas [a t x]
  (vec/mul (vec/sub a t) x))

(def ^:private points->poly
  (completing into (fn [xs] (join " " xs))))

(defn ^:private points->canvas [t x]
  (comp
   (partition-all 2)
   (map (fn [[x y]] (Vec2. x y)))
   (map (fn [v] (xf-canvas v t x)))
   (map (fn [v] [(.-x v) (.-y v)]))))

(defn ^:private px->ft [len]
  (let [ft (* (/ len grid-size) 5)
        rd (js/Math.round ft)]
    (if (< (abs (- ft rd)) epsilon) rd
        (.toFixed ft 1))))

(defui ^:private text
  [{:keys [attrs children]}]
  ($ :text.scene-text attrs children))

(defui ^:private anchor
  []
  ($ :<>
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
          (set-segment nil)
          (set-cursor nil)
          (on-release segment))
        on-drag
        (uix/use-callback
         (fn [data]
           (let [dx (.-x (.-delta data))
                 dy (.-y (.-delta data))
                 mx (.-clientX (.-activatorEvent data))
                 my (.-clientY (.-activatorEvent data))]
             (set-segment
              (fn [^Segment s]
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
  (let [{:keys [children on-release tile-path transform]
         :or {on-release (fn [])
              transform xf-identity}} props
        result (hooks/use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale :camera/scale
          {grid-paths :scene/show-object-outlines
           grid-align :scene/grid-align}
          :camera/scene}
         :user/camera} result
        transform (if grid-align transform xf-identity)]
    ($ draw-segment-drag
      {:use-cursor (contains? props :transform)
       :on-release
       (uix/use-callback
        (fn [^Segment segment]
          (let [o (Vec2. ox oy)
                t (Vec2. tx ty)
                a (xf-camera (.-a segment) o t scale)
                b (xf-camera (.-b segment) o t scale)]
            (on-release (transform a b))))
        [on-release transform ox oy tx ty scale])}
      (uix/use-callback
       (fn [^Segment segment ^Vec2 cursor]
         (let [off (Vec2. ox oy)
               trs (Vec2. tx ty)]
           (cond (some? segment)
                 (let [seg (transform
                            (xf-camera (.-a segment) off trs scale)
                            (xf-camera (.-b segment) off trs scale))]
                   ($ :<>
                     (if (and (fn? tile-path) grid-paths)
                       (let [path (tile-path seg)]
                         ($ :polygon.scene-draw-tile-path
                           {:points (transduce (points->canvas trs scale) points->poly [] path)})))
                     (children
                      (Segment. (.-a seg) (.-b seg))
                      (Segment.
                       (xf-canvas (.-a seg) trs scale)
                       (xf-canvas (.-b seg) trs scale)))))
                 (and grid-align (some? cursor))
                 (let [v (-> cursor (xf-camera off trs scale) (transform) (xf-canvas trs scale))]
                   ($ :g {:transform (vec/to-translate v)}
                     ($ anchor))))))
       [children grid-paths grid-align tile-path transform ox oy tx ty scale]))))

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
               (xf-camera basis shift scale)
               (vec/round (if align? half-size 1))
               (set-cursor)))
         :on-click
         (fn [event]
           (if (not closing?)
             (set-points (conj points cursor))
             (let [xs (geom/reorient (mapcat (fn [v] [(.-x v) (.-y v)]) points))]
               (set-points [])
               (set-cursor nil)
               (on-create event xs))))})
      (if (and align? (not closing?) (some? cursor))
        (let [point (xf-canvas cursor shift scale)]
          ($ :g {:transform (vec/to-translate point)}
            ($ anchor))))
      (if (seq points)
        (let [point (xf-canvas (first points) shift scale)]
          ($ :circle.scene-draw-point-ring
            {:cx (.-x point) :cy (.-y point) :r 6})))
      (for [point points :let [point (xf-canvas point shift scale)]]
        ($ :circle.scene-draw-point
          {:key point :cx (.-x point) :cy (.-y point) :r 4}))
      (if (and (seq points) (some? cursor))
        ($ :polygon.scene-draw-shape
          {:points
           (transduce
            (comp (map (fn [v] (xf-canvas v (Vec2. tx ty) scale)))
                  (map (fn [v] [(.-x v) (.-y v)])))
            points->poly []
            (if (not closing?) (conj points cursor) points))})))))

(defui ^:private draw-select []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:on-release
       (uix/use-callback
        (fn [^Segment s]
          (dispatch :selection/from-rect (vec/to-seq s))) [dispatch])}
      (uix/use-callback
       (fn [_ canvas]
         (let [a (.-a canvas) b (.-b canvas)]
           ($ hooks/use-portal {:name :multiselect}
             ($ :path.scene-draw-shape
               {:d (join " " [\M (.-x a) (.-y a) \H (.-x b) \V (.-y b) \H (.-x a) \Z])})))) []))))

(defui ^:private draw-ruler []
  ($ draw-segment
    {:transform xf-align-half}
    (uix/use-callback
     (fn [camera canvas]
       (let [a (.-a canvas) b (.-b canvas)]
         ($ :<>
           ($ :line.scene-draw-shape
             {:x1 (.-x a)
              :y1 (.-y a)
              :x2 (.-x b)
              :y2 (.-y b)})
           ($ :g {:transform (vec/to-translate a)}
             ($ anchor))
           ($ :g {:transform (vec/to-translate b)}
             ($ anchor))
           ($ text
             {:attrs
              {:x (- (.-x b) 48)
               :y (- (.-y b) 8)
               :fill "white"}}
             (str (px->ft (vec/dist-cheb camera)) "ft."))))) [])))

(defui ^:private draw-circle []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:transform xf-align
       :on-release
       (uix/use-callback
        (fn [^Segment s]
          (dispatch :shape/create :circle (vec/to-seq s))) [dispatch])
       :tile-path
       (uix/use-callback
        (fn [^Segment s]
          (let [r (vec/dist-cheb s)]
            (geom/tile-path-circle (.-x (.-a s)) (.-y (.-a s)) r))) [])}
      (uix/use-callback
       (fn [^Segment camera
            ^Segment canvas]
         (let [src (.-a canvas)]
           ($ :<>
             ($ :circle.scene-draw-shape
               {:cx (.-x src)
                :cy (.-y src)
                :r (vec/dist-cheb canvas)})
             ($ text {:attrs {:x (.-x src) :y (.-y src) :fill "white"}}
               (str (px->ft (vec/dist-cheb camera)) "ft. radius"))))) []))))

(defui ^:private draw-rect []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:transform xf-align
       :on-release
       (uix/use-callback
        (fn [^Segment s]
          (dispatch :shape/create :rect (vec/to-seq s))) [dispatch])}
      (uix/use-callback
       (fn [^Segment camera
            ^Segment canvas]
         (let [a (.-a camera) b (.-b camera)
               c (.-a canvas) d (.-b canvas)]
           ($ :<>
             ($ :path.scene-draw-shape {:d (join " " [\M (.-x c) (.-y c) \H (.-x d) \V (.-y d) \H (.-x c) \Z])})
             ($ text {:attrs {:x (+ (.-x c) 8) :y (- (.-y c) 8) :fill "white"}}
               (let [v (vec/abs (vec/sub a b))]
                 (str (px->ft (.-x v)) "ft. x "
                      (px->ft (.-y v)) "ft.")))))) []))))

(defui ^:private draw-line []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:transform xf-align-line
       :on-release
       (uix/use-callback
        (fn [^Segment s]
          (dispatch :shape/create :line (vec/to-seq s))) [dispatch])
       :tile-path
       (uix/use-callback
        (fn [^Segment s]
          (let [a (.-a s) b (.-b s)]
            (-> (geom/line-points (.-x a) (.-y a) (.-x b) (.-y b) half-size)
                (geom/tile-path-line)))) [])}
      (uix/use-callback
       (fn [^Segment camera
            ^Segment canvas]
         (let [a (.-a camera) b (.-b camera)
               c (.-a canvas) d (.-b canvas)]
           ($ :<>
             (let [scale (/ (abs (- (.-x d) (.-x c))) (abs (- (.-x b) (.-x a))))]
               (if (not (js/isNaN scale))
                 (let [xs (geom/line-points (.-x c) (.-y c) (.-x d) (.-y d) (* scale half-size))]
                   ($ :polygon.scene-draw-shape {:points (transduce (partition-all 2) points->poly [] xs)}))))
             ($ text {:attrs {:x (+ (.-x c) 8) :y (- (.-y c) 8) :fill "white"}}
               (str (px->ft (vec/dist camera)) "ft."))))) []))))

(defui ^:private draw-cone []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:transform xf-align-cone
       :on-release
       (uix/use-callback
        (fn [^Segment s]
          (dispatch :shape/create :cone (vec/to-seq s))) [dispatch])
       :tile-path
       (uix/use-callback
        (fn [^Segment s]
          (let [a (.-a s) b (.-b s)]
            (-> (geom/cone-points (.-x a) (.-y a) (.-x b) (.-y b))
                (geom/tile-path-cone)))) [])}
      (uix/use-callback
       (fn [^Segment camera
            ^Segment canvas]
         (let [c (.-a canvas) d (.-b canvas)]
           ($ :<>
             ($ :polygon.scene-draw-shape
               {:points (join " " (geom/cone-points (.-x c) (.-y c) (.-x d) (.-y d)))})
             ($ text {:attrs {:x (+ (.-x d) 16) :y (+ (.-y d) 16) :fill "white"}}
               (str (px->ft (vec/dist camera)) "ft."))))) []))))

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
         (dispatch :mask/create points))})))

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
                                   (vec/round 0.25))]
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
