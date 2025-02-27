(ns ogres.app.component.scene-draw
  (:require [clojure.string :refer [join]]
            [ogres.app.component :refer [icon]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.geom :refer [chebyshev-distance euclidean-distance cone-points reorient]]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]
            ["@dnd-kit/core"
             :refer  [DndContext useDraggable useDndMonitor]
             :rename {DndContext    dnd-context
                      useDndMonitor use-dnd-monitor
                      useDraggable  use-draggable}]))

(def ^:private round
  (map (fn [[x y]] [(js/Math.round x) (js/Math.round y)])))

(defn ^:private px->ft [px size]
  (js/Math.round (* (/ px size) 5)))

(defn ^:private +' [x y]
  (map (fn [[ax ay]] [(+ ax x) (+ ay y)])))

(defn ^:private *' [n]
  (map (fn [[x y]] [(* x n) (* y n)])))

(defn ^:private convert [xs & xfs]
  (into [] (apply comp (partition-all 2) xfs) xs))

(defui ^:private text [{:keys [attrs children]}]
  ($ :text.scene-text attrs children))

(defui ^:private drawable
  [{:keys [on-release children]}]
  (let [[points set-points] (uix/use-state [])
        drag (use-draggable #js {"id" "drawable"})]
    (use-dnd-monitor
     #js {"onDragMove"
          (fn [data]
            (let [dx (.. data -delta -x)
                  dy (.. data -delta -y)]
              (set-points
               (fn [[ax ay]]
                 (let [ax (if (nil? ax) (.. data -activatorEvent -clientX) ax)
                       ay (if (nil? ay) (.. data -activatorEvent -clientY) ay)]
                   [ax ay (+ ax dx) (+ ay dy)])))))
          "onDragEnd"
          (fn []
            (set-points [])
            (on-release points))})
    ($ :<>
      ($ :rect
        {:x 0
         :y 0
         :width "100%"
         :height "100%"
         :fill "transparent"
         :style {:will-change "transform"}
         :ref (.-setNodeRef drag)
         :on-pointer-down (.. drag -listeners -onPointerDown)})
      (if (seq points)
        (children points)))))

(def ^:private query
  [[:bounds/self :default [0 0 0 0]]
   {:user/camera
    [[:camera/scale :default 1]
     [:camera/point :default [0 0]]
     {:camera/scene
      [[:scene/grid-size :default grid-size]
       :scene/grid-origin]}]}])

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
        closed? (< (euclidean-distance ax ay mx my) 32)]
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
                   xs (reorient xs)]
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
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query)
        {[ox oy]  :bounds/self
         {[tx ty] :camera/point
          scale   :camera/scale} :user/camera} result]
    ($ drawable
      {:on-release
       (fn [points]
         (let [points (convert points (+' (- ox) (- oy)) (*' (/ scale)) (+' tx ty) cat)]
           (dispatch :selection/from-rect points)))}
      (fn [points]
        (let [[ax ay bx by] (convert points (+' (- ox) (- oy)) cat)]
          ($ hooks/use-portal {:name :multiselect}
            ($ :path {:d (join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])})))))))

(defui ^:private draw-ruler []
  (let [result (hooks/use-query query)
        {[ox oy] :bounds/self
         {scale  :camera/scale} :user/camera} result]
    ($ drawable
      {:on-release identity}
      (fn [points]
        (let [[ax ay bx by] (convert points (+' (- ox) (- oy)) cat)]
          ($ :g
            ($ :line {:x1 ax :y1 ay :x2 bx :y2 by})
            ($ text {:attrs {:x (- bx 48) :y (- by 8) :fill "white"}}
              (-> (chebyshev-distance ax ay bx by)
                  (px->ft (* grid-size scale))
                  (str "ft.")))))))))

(defui ^:private draw-circle []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale :camera/scale} :user/camera} result]
    ($ drawable
      {:on-release
       (fn [points]
         (let [points (convert points (+' (- ox) (- oy)) (*' (/ scale)) (+' tx ty) cat)]
           (dispatch :shape/create :circle points)))}
      (fn [points]
        (let [[ax ay bx by] (convert points (+' (- ox) (- oy)) cat)
              radius (chebyshev-distance ax ay bx by)]
          ($ :g
            ($ :circle {:cx ax :cy ay :r radius})
            ($ text {:attrs {:x ax :y ay :fill "white"}}
              (-> radius (px->ft (* grid-size scale)) (str "ft. radius")))))))))

(defui ^:private draw-rect []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale   :camera/scale}
         :user/camera} result]
    ($ drawable
      {:on-release
       (fn [points]
         (let [points (convert points (+' (- ox) (- oy)) (*' (/ scale)) (+' tx ty) cat)]
           (dispatch :shape/create :rect points)))}
      (fn [points]
        (let [[ax ay bx by] (convert points (+' (- ox) (- oy)) cat)]
          ($ :g
            ($ :path {:d (join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])})
            ($ text {:attrs {:x (+ ax 8) :y (- ay 8) :fill "white"}}
              (let [w (px->ft (js/Math.abs (- bx ax)) (* grid-size scale))
                    h (px->ft (js/Math.abs (- by ay)) (* grid-size scale))]
                (str w "ft. x " h "ft.")))))))))

(defui ^:private draw-line []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale   :camera/scale} :user/camera} result]
    ($ drawable
      {:on-release
       (fn [points]
         (let [points (convert points (+' (- ox) (- oy)) (*' (/ scale)) (+' tx ty) cat)]
           (dispatch :shape/create :line points)))}
      (fn [points]
        (let [[ax ay bx by] (convert points (+' (- ox) (- oy)) cat)]
          ($ :g
            ($ :line {:x1 ax :y1 ay :x2 bx :y2 by})
            ($ text {:attrs {:x (+ ax 8) :y (- ay 8) :fill "white"}}
              (-> (chebyshev-distance ax ay bx by)
                  (px->ft (* grid-size scale))
                  (str "ft.")))))))))

(defui ^:private draw-cone []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale   :camera/scale} :user/camera} result]
    ($ drawable
      {:on-release
       (fn [points]
         (let [points (convert points (+' (- ox) (- oy)) (*' (/ scale)) (+' tx ty) cat)]
           (dispatch :shape/create :cone points)))}
      (fn [points]
        (let [[ax ay bx by] (convert points (+' (- ox) (- oy)) cat)]
          ($ :g
            ($ :polygon {:points (join " " (cone-points ax ay bx by))})
            ($ text {:attrs {:x (+ bx 16) :y (+ by 16) :fill "white"}}
              (-> (euclidean-distance ax ay bx by)
                  (px->ft (* grid-size scale))
                  (str "ft.")))))))))

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
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale   :camera/scale
          {prev-size :scene/grid-size
           prev-origin :scene/grid-origin}
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
          ($ :g {:transform (str "translate(" (- x ox) "," (- y oy) ")")}
            ($ :path {:d (join path)})
            ($ :circle {:cx 0 :cy 0 :r 14})
            ($ :foreignObject.grid-align-form
              {:x -128 :y -128 :width 256 :height 256}
              ($ :form
                {:on-submit
                 (fn [event]
                   (.preventDefault event)
                   (let [xf (comp (+' (- ox) (- oy)) (*' (/ scale)) (+' tx ty) (*' (/ prev-size next-size)) round cat)]
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

(defui draw-note []
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
