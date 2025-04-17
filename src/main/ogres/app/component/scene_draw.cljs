(ns ogres.app.component.scene-draw
  (:require [clojure.string :refer [join]]
            #_[ogres.app.component :refer [icon]]
            [ogres.app.const :refer [grid-size half-size]]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]
            [ogres.app.vec :as vec :refer [Segment Vec2]]
            ["@dnd-kit/core"
             :refer  [DndContext useDraggable useDndMonitor]
             :rename {DndContext    dnd-context
                      useDndMonitor use-dnd-monitor
                      useDraggable  use-draggable}]))

(defn ^:private xf-identity
  ([a] a)
  ([a b] (Segment. a b)))

(defn ^:private xf-align
  ([a]   (vec/round a grid-size))
  ([a b] (Segment. (vec/round a grid-size) (vec/round b grid-size))))

(defn ^:private xf-align-half
  ([a]   (vec/round a half-size))
  ([a b] (Segment. (vec/round a half-size) (vec/round b half-size))))

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
  (let [ft (* (/ len grid-size) 5)]
    (if (js/Number.isInteger ft) ft
        (.toFixed ft 1))))

(defui ^:private text
  [{:keys [attrs children]}]
  ($ :text.scene-text attrs children))

(defui ^:private anchor
  []
  ($ :<>
    ($ :circle.scene-draw-anchor {:cx 0 :cy 0 :r 4})
    ($ :circle.scene-draw-anchor-ring {:cx 0 :cy 0 :r 6})))

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
        {:x 0 :y 0
         :width "100%" :height "100%"
         :fill "transparent"
         :ref (.-setNodeRef options)
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
                   ($ :g {:transform (str "translate(" (.-x v) ", " (.-y v) ")")}
                     ($ anchor))))))
       [children grid-paths grid-align tile-path transform ox oy tx ty scale]))))

#_(defui ^:private polygon
    [{:keys [on-create]}]
    (let [result (hooks/use-query query)
          {[ox oy] :bounds/self
           {[tx ty] :camera/point
            scale :camera/scale} :user/camera} result
          [pairs set-pairs] (uix/use-state [])
          [mouse set-mouse] (uix/use-state [])
          [ax ay] pairs
          [mx my] mouse
          closed? (< (geom/euclidean-distance ax ay mx my) 32)]
      ($ :<>
        ($ :rect
          {:x 0 :y 0 :fill "transparent"
           :width "100%" :height "100%"
           :on-pointer-down
           (fn [event]
             (.stopPropagation event))
           :on-pointer-move
           (fn [event]
             (let [dst [(.-clientX event) (.-clientY event)]]
               (set-mouse (convert dst (+' (- ox) (- oy)) cat))))
           :on-click
           (fn [event]
             (if closed?
               (let [xs (convert pairs (*' (/ scale)) (+' tx ty) round cat)
                     xs (geom/reorient xs)]
                 (set-pairs [])
                 (on-create event xs))
               (set-pairs #(conj %1 mx my))))})
        ($ :circle {:cx mx :cy my :r 3 :style {:pointer-events "none" :fill "white"}})
        (if (seq pairs)
          ($ :circle
            {:cx ax :cy ay :r 6
             :style {:pointer-events "none" :stroke "white" :stroke-width 1 :stroke-dasharray "none"}}))
        (for [[x y] (partition 2 pairs)]
          ($ :circle {:key [x y] :cx x :cy y :r 3 :style {:pointer-events "none" :fill "white"}}))
        ($ :polygon
          {:points (join " " (if closed? pairs (into pairs mouse)))
           :style  {:pointer-events "none"}}))))

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
      {:transform xf-align-half
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
             (let [ln (vec/dist-cheb camera)]
               ($ text {:attrs {:x (+ (.-x c) 8) :y (- (.-y c) 8) :fill "white"}}
                 (str (px->ft ln) "ft.")))))) []))))

(defui ^:private draw-cone []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:transform xf-align
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

#_(defui ^:private draw-poly []
    (let [dispatch (hooks/use-dispatch)]
      ($ polygon
        {:on-create
         (fn [_ xs]
           (dispatch :shape/create :poly xs))})))

#_(defui ^:private draw-mask []
    (let [dispatch (hooks/use-dispatch)]
      ($ polygon
        {:on-create
         (fn [_ points]
           (dispatch :mask/create points))})))

#_(defui ^:private draw-grid []
    (let [dispatch (hooks/use-dispatch)
          {[bx by] :bounds/self
           {[tx ty] :camera/point
            scale   :camera/scale
            {prev-size :scene/grid-size
             [px py :as prev-origin] :scene/grid-origin}
            :camera/scene} :user/camera} (hooks/use-query query)
          [next-origin set-origin] (uix/use-state nil)
          [next-size     set-size] (uix/use-state prev-size)]
      ($ :g.grid-align
        ($ :rect
          {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"
           :on-click
           (fn [event]
             (let [x (.-clientX event) y (.-clientY event)]
               (set-origin [x y])))})
        (if (some? next-origin)
          (let [[x y] next-origin
                idxs 7
                draw (* next-size (/ grid-size prev-size) scale)
                wide (* (inc idxs) draw)
                path (for [indx (range (- idxs) (inc idxs))
                           path [[(- wide) "," (* indx draw) "H" wide]
                                 [(* indx draw) "," (- wide) "V" wide]]]
                       (apply str "M" path))]
            ($ :g {:transform (str "translate(" (- x bx) "," (- y by) ")")}
              ($ :path {:d (join path)})
              ($ :circle {:cx 0 :cy 0 :r 14})
              ($ :foreignObject.grid-align-form
                {:x -128 :y -128 :width 256 :height 256}
                ($ :form
                  {:on-submit
                   (fn [event]
                     (.preventDefault event)
                     (let [xf (comp (+' (- bx) (- by))
                                    (*' (/ scale))
                                    (+' tx ty)
                                    (+' px py)
                                    (*' (/ prev-size next-size))
                                    (map (fn [[x y]] [(mod (abs x) grid-size) (mod (abs y) grid-size)]))
                                    round
                                    cat)]
                       (dispatch :scene/apply-grid-options (convert next-origin xf) next-size)))}
                  ($ :fieldset.grid-align-origin
                    ($ :button
                      {:type "button" :data-name "up" :on-click #(set-origin [x (dec y)])}
                      ($ icon {:name "arrow-up-short" :size 20}))
                    ($ :button
                      {:type "button" :data-name "right" :on-click #(set-origin [(inc x) y])}
                      ($ icon {:name "arrow-right-short" :size 20}))
                    ($ :button
                      {:type "button" :data-name "down" :on-click #(set-origin [x (inc y)])}
                      ($ icon {:name "arrow-down-short" :size 20}))
                    ($ :button
                      {:type "button" :data-name "left" :on-click #(set-origin [(dec x) y])}
                      ($ icon {:name "arrow-left-short" :size 20}))
                    (if (some? prev-origin)
                      ($ :button
                        {:type "button" :data-name "clear" :data-tooltip "Reset"
                         :on-click #(dispatch :scene/reset-grid-origin)}
                        ($ icon {:name "x-circle-fill" :size 16}))))
                  ($ :fieldset.grid-align-size
                    ($ :button
                      {:type "button" :data-name "dec" :on-click #(set-size dec)}
                      ($ icon {:name "dash" :size 20}))
                    ($ :button
                      {:type "button" :data-name "inc" :on-click #(set-size inc)}
                      ($ icon {:name "plus" :size 20}))
                    ($ :input.text.text-ghost
                      {:type "number"
                       :value next-size
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
    ($ :rect
      {:x 0
       :y 0
       :width "100%"
       :height "100%"
       :fill "transparent"
       :on-click
       (fn [event]
         (let [x (.-clientX event)
               y (.-clientY event)]
           (dispatch :note/create x y)))})))

(defui draw [{:keys [mode] :as props}]
  ($ dnd-context
    (case mode
      :circle ($ draw-circle props)
      :cone   ($ draw-cone props)
      ;; :grid   ($ draw-grid props)
      :line   ($ draw-line props)
      ;; :mask   ($ draw-mask props)
      :note   ($ draw-note props)
      ;; :poly   ($ draw-poly props)
      :rect   ($ draw-rect props)
      :ruler  ($ draw-ruler props)
      :select ($ draw-select props))))
