(ns ogres.app.component.scene-draw
  (:require [clojure.string :refer [join]]
            [ogres.app.component :refer [icon]]
            [ogres.app.const :refer [grid-size half-size]]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [ogres.app.util :as util]
            [uix.core :as uix :refer [defui $]]
            ["@dnd-kit/core"
             :refer  [DndContext useDraggable useDndMonitor]
             :rename {DndContext    dnd-context
                      useDndMonitor use-dnd-monitor
                      useDraggable  use-draggable}]))

(defn ^:private xf-identity
  ([ax ay]       [ax ay])
  ([ax ay bx by] [ax ay bx by]))

(defn ^:private xf-align
  ([ax ay]
   [(util/round ax grid-size)
    (util/round ay grid-size)])
  ([ax ay bx by]
   [(util/round ax grid-size)
    (util/round ay grid-size)
    (util/round bx grid-size)
    (util/round by grid-size)]))

(defn ^:private xf-align-half [ax ay bx by]
  [(util/round ax half-size)
   (util/round ay half-size)
   (util/round bx half-size)
   (util/round by half-size)])

(def ^:private rf-points->poly
  (completing into (fn [xs] (join " " xs))))

(defn ^:private to-camera [ax ay ox oy tx ty scale]
  [(+ (/ (- ax ox) scale) tx)
   (+ (/ (- ay oy) scale) ty)])

(defn ^:private to-canvas [ax ay tx ty scale]
  [(* (- ax tx) scale)
   (* (- ay ty) scale)])

(defn ^:private xf-to-canvas
  [tx ty scale]
  (comp
   (partition-all 2)
   (map (fn [[ax ay]] (to-canvas ax ay tx ty scale)))))

(def ^:private round
  (map (fn [[x y]] [(js/Math.round x) (js/Math.round y)])))

(defn ^:private px->ft [px size]
  (let [ft (* (/ px size) 5)]
    (if (js/Number.isInteger ft) ft
        (.toFixed ft 1))))

(defn ^:private +' [x y]
  (map (fn [[ax ay]] [(+ ax x) (+ ay y)])))

(defn ^:private *' [n]
  (map (fn [[x y]] [(* x n) (* y n)])))

(defn ^:private convert [xs & xfs]
  (into [] (apply comp (partition-all 2) xfs) xs))

(defui ^:private ^:memo text [{:keys [attrs children]}]
  ($ :text.scene-text attrs children))

(defui ^:private ^:memo anchor []
  ($ :<>
    ($ :circle.scene-draw-anchor {:cx 0 :cy 0 :r 4})
    ($ :circle.scene-draw-anchor-ring {:cx 0 :cy 0 :r 6})))

(defui ^:private draw-segment-drag [props]
  (let [{:keys [children on-release use-cursor]} props
        [points set-points] (uix/use-state nil)
        [cursor set-cursor] (uix/use-state nil)
        options (use-draggable #js {"id" "drawable"})
        on-down (.. options -listeners -onPointerDown)
        on-stop
        (fn []
          (set-points nil)
          (set-cursor nil)
          (on-release points))
        on-drag
        (uix/use-callback
         (fn [data]
           (let [dx (.-x (.-delta data))
                 dy (.-y (.-delta data))]
             (set-points
              (fn [xs]
                (if (seq xs)
                  (let [[ax ay _ _] xs]
                    [ax ay (+ ax dx) (+ ay dy)])
                  (let [ax (.-clientX (.-activatorEvent data))
                        ay (.-clientY (.-activatorEvent data))]
                    [ax ay ax ay])))))) [])
        on-move
        (uix/use-callback
         (fn [event]
           (set-cursor [(.-clientX event) (.-clientY event)])) [])]
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
         :on-pointer-move (if (and use-cursor (not (seq points))) on-move)})
      (children points cursor))))

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
         :or   {transform xf-identity}} props
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
        (fn [[ax ay bx by]]
          (let [[ax ay] (to-camera ax ay ox oy tx ty scale)
                [bx by] (to-camera bx by ox oy tx ty scale)
                [ax ay bx by] (transform ax ay bx by)]
            (on-release ax ay bx by)))
        [on-release transform ox oy tx ty scale])}
      (uix/use-callback
       (fn [[ax ay bx by] [mx my]]
         (cond (or ax ay bx by)
               (let [[ax ay] (to-camera ax ay ox oy tx ty scale)
                     [bx by] (to-camera bx by ox oy tx ty scale)
                     [ax ay bx by] (transform ax ay bx by)
                     [cx cy] (to-canvas ax ay tx ty scale)
                     [dx dy] (to-canvas bx by tx ty scale)]
                 ($ :<>
                   (if (and (fn? tile-path) grid-paths)
                     (let [path (tile-path ax ay bx by)]
                       ($ :polygon.scene-draw-tile-path
                         {:points (transduce (xf-to-canvas tx ty scale) rf-points->poly [] path)})))
                   (children ax ay bx by cx cy dx dy)))
               (and grid-align (or mx my))
               (let [[cx cy] (to-camera mx my ox oy tx ty scale)
                     [cx cy] (transform cx cy)
                     [cx cy] (to-canvas cx cy tx ty scale)]
                 ($ :g {:transform (str "translate(" cx ", " cy ")")}
                   ($ anchor)))))
       [children grid-paths grid-align tile-path transform ox oy tx ty scale]))))

(defui ^:private polygon
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
        (fn [ax ay bx by]
          (dispatch :selection/from-rect [ax ay bx by])) [dispatch])}
      (uix/use-callback
       (fn [_ _ _ _ ax ay bx by]
         ($ hooks/use-portal {:name :multiselect}
           ($ :path.scene-draw-shape
             {:d (join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])}))) []))))

(defui ^:private draw-ruler []
  ($ draw-segment
    {:transform xf-align-half
     :on-release (uix/use-callback (fn []) [])}
    (uix/use-callback
     (fn [ax ay bx by cx cy dx dy]
       ($ :<>
         ($ :line.scene-draw-shape {:x1 cx :y1 cy :x2 dx :y2 dy})
         ($ text {:attrs {:x (- dx 48) :y (- dy 8) :fill "white"}}
           (-> (geom/chebyshev-distance ax ay bx by)
               (px->ft grid-size)
               (str "ft."))))) [])))

(defui ^:private draw-circle []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:transform xf-align
       :on-release
       (uix/use-callback
        (fn [ax ay bx by]
          (dispatch :shape/create :circle [ax ay bx by])) [dispatch])
       :tile-path
       (uix/use-callback
        (fn [ax ay bx by]
          (let [r (geom/chebyshev-distance ax ay bx by)]
            (geom/tile-path-circle ax ay r))) [])}
      (uix/use-callback
       (fn [ax ay bx by cx cy dx dy]
         (let [radius (geom/chebyshev-distance ax ay bx by)]
           ($ :<>
             ($ :circle.scene-draw-shape {:cx cx :cy cy :r (geom/chebyshev-distance cx cy dx dy)})
             ($ text {:attrs {:x cx :y cy :fill "white"}}
               (str (px->ft radius grid-size)
                    "ft. radius"))))) []))))

(defui ^:private draw-rect []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:transform xf-align
       :on-release
       (uix/use-callback
        (fn [ax ay bx by]
          (dispatch :shape/create :rect [ax ay bx by])) [dispatch])}
      (uix/use-callback
       (fn [cx cy dx dy ax ay bx by]
         ($ :<>
           ($ :path.scene-draw-shape {:d (join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])})
           ($ text {:attrs {:x (+ ax 8) :y (- ay 8) :fill "white"}}
             (let [w (px->ft (abs (- cx dx)) grid-size)
                   h (px->ft (abs (- cy dy)) grid-size)]
               (str w "ft. x " h "ft."))))) []))))

(defui ^:private draw-line []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:transform xf-align-half
       :on-release
       (uix/use-callback
        (fn [ax ay bx by]
          (dispatch :shape/create :line [ax ay bx by])) [dispatch])
       :tile-path
       (uix/use-callback
        (fn [ax ay bx by]
          (geom/tile-path-line (geom/line-points ax ay bx by half-size))) [])}
      (uix/use-callback
       (fn [ax ay bx by cx cy dx dy]
         ($ :<>
           (let [scale (/ (abs (- dx cx)) (abs (- bx ax)))]
             (if (not (js/isNaN scale))
               (let [xs (geom/line-points cx cy dx dy (* scale half-size))]
                 ($ :polygon.scene-draw-shape {:points (transduce (partition-all 2) rf-points->poly [] xs)}))))
           (let [ln (geom/chebyshev-distance ax ay bx by)]
             ($ text {:attrs {:x (+ cx 8) :y (- cy 8) :fill "white"}}
               (str (px->ft ln grid-size) "ft."))))) []))))

(defui ^:private draw-cone []
  (let [dispatch (hooks/use-dispatch)]
    ($ draw-segment
      {:transform xf-align
       :on-release
       (uix/use-callback
        (fn [ax ay bx by]
          (dispatch :shape/create :cone [ax ay bx by])) [dispatch])
       :tile-path
       (uix/use-callback
        (fn [ax ay bx by]
          (geom/tile-path-cone (geom/cone-points ax ay bx by))) [])}
      (uix/use-callback
       (fn [ax ay bx by cx cy dx dy]
         (let [radius (geom/euclidean-distance ax ay bx by)]
           ($ :<>
             ($ :polygon.scene-draw-shape {:points (join " " (geom/cone-points cx cy dx dy))})
             ($ text {:attrs {:x (+ dx 16) :y (+ dy 16) :fill "white"}}
               (str (px->ft radius grid-size) "ft."))))) []))))

(defui ^:private draw-poly []
  (let [dispatch (hooks/use-dispatch)]
    ($ polygon
      {:on-create
       (fn [_ xs]
         (dispatch :shape/create :poly xs))})))

(defui ^:private draw-mask []
  (let [dispatch (hooks/use-dispatch)]
    ($ polygon
      {:on-create
       (fn [_ points]
         (dispatch :mask/create points))})))

(defui ^:private draw-grid []
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
      :grid   ($ draw-grid props)
      :line   ($ draw-line props)
      :mask   ($ draw-mask props)
      :note   ($ draw-note props)
      :poly   ($ draw-poly props)
      :rect   ($ draw-rect props)
      :ruler  ($ draw-ruler props)
      :select ($ draw-select props))))
