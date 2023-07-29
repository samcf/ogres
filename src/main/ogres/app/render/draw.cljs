(ns ogres.app.render.draw
  (:require [clojure.string :refer [join]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.hooks :refer [use-dispatch use-portal use-query]]
            [ogres.app.geom :refer [chebyshev euclidean triangle]]
            [react-draggable]
            [uix.core :refer [defui $ use-state]]))

(defn ^:private px->ft [px size]
  (js/Math.round (* (/ px size) 5)))

(defn ^:private round [x n]
  (* (js/Math.round (/ x n)) n))

(defn ^:private +-xf [x y]
  (map (fn [[ax ay]] [(+ ax x) (+ ay y)])))

(defn ^:private *-xf [n]
  (map (fn [[x y]] [(* x n) (* y n)])))

(defn ^:private r-xf [n]
  (map (fn [[x y]] [(round x n) (round y n)])))

(defn ^:private xs-xfs [xs & xfs]
  (into [] (apply comp (partition-all 2) xfs) xs))

(defui ^:private text [{:keys [attrs children]}]
  ($ :text.scene-text attrs children))

(defui ^:private drawable [{:keys [transform on-release children]}]
  (let [[state set-state] (use-state [])]
    ($ :<>
      ($ react-draggable
        {:position #js {:x 0 :y 0}
         :on-start
         (fn [event]
           (.stopPropagation event)
           (let [src [(.-clientX event) (.-clientY event)]]
             (set-state [event (into src src)])))
         :on-drag
         (fn [event data]
           (set-state
            (fn [[_ [ax ay]]]
              [event (into [ax ay] [(+ ax (.-x data)) (+ ay (.-y data))])])))
         :on-stop
         (fn [event]
           (let [[_ xs] state]
             (set-state [])
             (on-release event (transform event xs))))}
        ($ :rect
          {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"
           :style {:will-change "transform"}}))
      (let [[event points] state]
        (if (seq points)
          (children {:event event :points (transform event points)}))))))

(def ^:private query
  [[:bounds/self :default [0 0 0 0]]
   {:local/camera
    [[:camera/scale :default 1]
     [:camera/point :default [0 0]]
     {:camera/scene
      [[:scene/snap-grid :default false]]}]}])

(defui ^:private polygon
  [{:keys [on-create]}]
  (let [result (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale   :camera/scale
          {align :scene/snap-grid} :camera/scene}
         :local/camera} result
        [pairs set-pairs] (use-state [])
        [mouse set-mouse] (use-state [])
        [ax ay] pairs
        [mx my] mouse
        closed? (< (euclidean ax ay mx my) 32)]
    ($ :<>
      ($ :rect
        {:x 0 :y 0 :fill "transparent"
         :width "100%" :height "100%"
         :on-mouse-down
         (fn [event]
           (.stopPropagation event))
         :on-mouse-move
         (fn [event]
           (let [dst [(.-clientX event) (.-clientY event)]]
             (if (not= align (.-metaKey event))
               (set-mouse (xs-xfs dst (+-xf (- ox) (- oy)) (*-xf (/ scale)) (+-xf (- tx) (- ty)) (r-xf grid-size) (+-xf tx ty) (*-xf scale) cat))
               (set-mouse (xs-xfs dst (+-xf (- ox) (- oy)) cat)))))
         :on-click
         (fn [event]
           (if closed?
             (let [xf (comp (*-xf (/ scale)) (+-xf (- tx) (- ty)))
                   xs (xs-xfs pairs xf cat)]
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
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {[ox oy]  :bounds/self
         {[tx ty] :camera/point
          scale   :camera/scale} :local/camera} result]
    ($ drawable
      {:transform
       (fn [_ xs]
         (xs-xfs xs (+-xf (- ox) (- oy)) (*-xf (/ scale)) (+-xf (- tx) (- ty)) cat))
       :on-release
       (fn [_ xs]
         (dispatch :selection/from-rect xs))}
      (fn [{:keys [points]}]
        (let [[ax ay bx by] (xs-xfs points (+-xf tx ty) (*-xf scale) cat)]
          ($ use-portal {:name :multiselect}
            (fn []
              ($ :path {:d (join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])}))))))))

(defui ^:private draw-ruler []
  (let [result (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale   :camera/scale
          {align :scene/snap-grid} :camera/scene}
         :local/camera} result]
    ($ drawable
      {:on-release identity
       :transform
       (fn [event xs]
         (if (not= align (.-metaKey event))
           (xs-xfs
            xs
            (+-xf (- ox) (- oy)) (*-xf (/ scale))
            (+-xf (- tx) (- ty)) (r-xf (/ grid-size 2))
            (+-xf tx ty) (*-xf scale) cat)
           (xs-xfs xs (+-xf (- ox) (- oy)) cat)))}
      (fn [{[ax ay bx by] :points}]
        ($ :g
          ($ :line {:x1 ax :y1 ay :x2 bx :y2 by})
          ($ text {:attrs {:x (- bx 48) :y (- by 8) :fill "white"}}
            (-> (chebyshev ax ay bx by)
                (px->ft (* grid-size scale))
                (str "ft."))))))))

(defui ^:private draw-circle []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale   :camera/scale
          {align :scene/snap-grid} :camera/scene}
         :local/camera} result]
    ($ drawable
      {:transform
       (fn [event xs]
         (let [[ax ay bx by] (xs-xfs xs (+-xf (- ox) (- oy)) (*-xf (/ scale)) (+-xf (- tx) (- ty)) cat)]
           (if (not= align (.-metaKey event))
             (let [[ax ay] (xs-xfs [ax ay] (r-xf (/ grid-size 2)) cat)
                   [bx by] (xs-xfs [bx by] (r-xf grid-size) cat)]
               [ax ay bx by])
             [ax ay bx by])))
       :on-release
       (fn [_ xs]
         (dispatch :shape/create :circle xs))}
      (fn [{:keys [points]}]
        (let [[ax ay bx by] (xs-xfs points (+-xf tx ty) (*-xf scale) cat)
              radius (chebyshev ax ay bx by)]
          ($ :g
            ($ :circle {:cx ax :cy ay :r radius})
            ($ text {:attrs {:x ax :y ay :fill "white"}}
              (-> radius (px->ft (* grid-size scale)) (str "ft. radius")))))))))

(defui ^:private draw-rect []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale   :camera/scale
          {align :scene/snap-grid} :camera/scene}
         :local/camera} result]
    ($ drawable
      {:transform
       (fn [event xs]
         (let [xf (comp (+-xf (- ox) (- oy)) (*-xf (/ scale)) (+-xf (- tx) (- ty)))]
           (if (not= align (.-metaKey event))
             (xs-xfs xs xf (r-xf (/ grid-size 2)) cat)
             (xs-xfs xs xf cat))))
       :on-release
       (fn [_ xs]
         (dispatch :shape/create :rect xs))}
      (fn [{:keys [points]}]
        (let [[ax ay bx by] (xs-xfs points (+-xf tx ty) (*-xf scale) cat)]
          ($ :g
            ($ :path {:d (join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])})
            ($ text {:attrs {:x (+ ax 8) :y (- ay 8) :fill "white"}}
              (let [w (px->ft (js/Math.abs (- bx ax)) (* grid-size scale))
                    h (px->ft (js/Math.abs (- by ay)) (* grid-size scale))]
                (str w "ft. x " h "ft.")))))))))

(defui ^:private draw-line []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale   :camera/scale
          {align :scene/snap-grid} :camera/scene}
         :local/camera} result]
    ($ drawable
      {:transform
       (fn [event xs]
         (let [xf (comp (+-xf (- ox) (- oy)) (*-xf (/ scale)) (+-xf (- tx) (- ty)))]
           (if (not= align (.-metaKey event))
             (xs-xfs xs xf (r-xf (/ grid-size 2)) cat)
             (xs-xfs xs xf cat))))
       :on-release
       (fn [_ xs]
         (dispatch :shape/create :line xs))}
      (fn [{:keys [points]}]
        (let [[ax ay bx by] (xs-xfs points (+-xf tx ty) (*-xf scale) cat)]
          ($ :g
            ($ :line {:x1 ax :y1 ay :x2 bx :y2 by})
            ($ text {:attrs {:x (+ ax 8) :y (- ay 8) :fill "white"}}
              (-> (chebyshev ax ay bx by)
                  (px->ft (* grid-size scale))
                  (str "ft.")))))))))

(defui ^:private draw-cone []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :camera/point
          scale   :camera/scale} :local/camera} result]
    ($ drawable
      {:transform
       (fn [_ xs]
         (let [xf (comp (+-xf (- ox) (- oy)) (*-xf (/ scale)) (+-xf (- tx) (- ty)))]
           (xs-xfs xs xf cat)))
       :on-release
       (fn [_ xs]
         (dispatch :shape/create :cone xs))}
      (fn [{:keys [points]}]
        (let [[ax ay bx by] (xs-xfs points (+-xf tx ty) (*-xf scale) cat)]
          ($ :g
            ($ :polygon {:points (join " " (triangle ax ay bx by))})
            ($ text {:attrs {:x (+ bx 16) :y (+ by 16) :fill "white"}}
              (-> (euclidean ax ay bx by)
                  (px->ft (* grid-size scale))
                  (str "ft.")))))))))

(defui ^:private draw-poly []
  (let [dispatch (use-dispatch)]
    ($ polygon
      {:on-create
       (fn [_ xs]
         (dispatch :shape/create :poly xs))})))

(defui ^:private draw-mask []
  (let [dispatch (use-dispatch)]
    ($ polygon
      {:on-create
       (fn [event xs]
         (dispatch :mask/create (not (.-shiftKey event)) xs))})))

(defui draw [{:keys [mode] :as props}]
  (let [fns {:circle draw-circle
             :cone   draw-cone
             :line   draw-line
             :mask   draw-mask
             :poly   draw-poly
             :rect   draw-rect
             :ruler  draw-ruler
             :select draw-select}]
    (if-let [component (fns mode)]
      ($ component props))))
