(ns ogres.app.render.draw
  (:require [clojure.string :refer [join]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.hooks :refer [use-dispatch use-portal use-query]]
            [ogres.app.geom :refer [chebyshev euclidean triangle]]
            [react-draggable]
            [uix.core.alpha :as uix]))

(defn px->ft [px size] (js/Math.round (* (/ px size) 5)))

(defn round [x n]
  (* (js/Math.round (/ x n)) n))

(defn +-xf [x y]
  (map (fn [[ax ay]] [(+ ax x) (+ ay y)])))

(defn *-xf [n]
  (map (fn [[x y]] [(* x n) (* y n)])))

(defn r-xf [n]
  (map (fn [[x y]] [(round x n) (round y n)])))

(defn xs-xfs [xs & xfs]
  (into [] (apply comp (partition-all 2) xfs) xs))

(defn text [attrs child]
  [:text.canvas-text attrs child])

(defn drawable [{:keys [transform on-release]} render-fn]
  (let [state (uix/state [])]
    [:<>
     [:> react-draggable
      {:position #js {:x 0 :y 0}
       :on-start
       (fn [event]
         (.stopPropagation event)
         (let [src [(.-clientX event) (.-clientY event)]]
           (reset! state [event (into src src)])))
       :on-drag
       (fn [event data]
         (swap! state
                (fn [[_ [ax ay]]]
                  [event (into [ax ay] [(+ ax (.-x data)) (+ ay (.-y data))])])))
       :on-stop
       (fn [event]
         (let [[_ xs] (deref state)]
           (reset! state [])
           (on-release event (transform event xs))))}
      [:rect
       {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"
        :style {:will-change "transform"}}]]
     (let [[event xs] (deref state)]
       (if (seq xs)
         (render-fn event (transform event xs))))]))

(def query
  [[:bounds/self :default [0 0 0 0]]
   {:local/window
    [[:window/scale :default 1]
     [:window/vec :default [0 0]]
     {:window/canvas
      [[:canvas/snap-grid :default false]]}]}])

(defn polygon [{:keys [on-create]}]
  (let [result (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :window/vec
          scale   :window/scale
          {align :canvas/snap-grid} :window/canvas}
         :local/window} result
        pairs   (uix/state [])
        mouse   (uix/state [])
        [ax ay] @pairs
        [mx my] @mouse
        closed? (< (euclidean ax ay mx my) 32)]
    [:<>
     [:rect
      {:x 0 :y 0 :fill "transparent"
       :width "100%" :height "100%"
       :on-mouse-down
       (fn [event]
         (.stopPropagation event))
       :on-mouse-move
       (fn [event]
         (let [dst [(.-clientX event) (.-clientY event)]]
           (if (not= align (.-metaKey event))
             (reset! mouse (xs-xfs dst (+-xf (- ox) (- oy)) (*-xf (/ scale)) (+-xf (- tx) (- ty)) (r-xf grid-size) (+-xf tx ty) (*-xf scale) cat))
             (reset! mouse (xs-xfs dst (+-xf (- ox) (- oy)) cat)))))
       :on-click
       (fn [event]
         (if closed?
           (let [xf (comp (*-xf (/ scale)) (+-xf (- tx) (- ty)))
                 xs (xs-xfs @pairs xf cat)]
             (reset! pairs [])
             (on-create event xs))
           (swap! pairs conj mx my)))}]
     [:circle {:cx mx :cy my :r 3 :style {:pointer-events "none" :fill "white"}}]
     (if (seq @pairs)
       [:circle
        {:cx ax :cy ay :r 6
         :style {:pointer-events "none" :stroke "white"
                 :stroke-width 1 :stroke-dasharray "none"}}])
     (for [[x y] (partition 2 @pairs)]
       [:circle {:key [x y] :cx x :cy y :r 3 :style {:pointer-events "none" :fill "white"}}])
     [:polygon
      {:points (join " " (if closed? @pairs (into @pairs @mouse)))
       :style  {:pointer-events "none"}}]]))

(defmulti draw :mode)

(defmethod draw :default [] nil)

(defmethod draw :select []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {[ox oy]  :bounds/self
         {[tx ty] :window/vec
          scale   :window/scale} :local/window} result]
    [drawable
     {:transform
      (fn [_ xs]
        (xs-xfs xs (+-xf (- ox) (- oy)) (*-xf (/ scale)) (+-xf (- tx) (- ty)) cat))
      :on-release
      (fn [_ xs]
        (dispatch :selection/from-rect xs))}
     (fn [_ xs]
       (let [[ax ay bx by] (xs-xfs xs (+-xf tx ty) (*-xf scale) cat)]
         [use-portal {:label :multiselect}
          [:path {:d (join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])}]]))]))

(defmethod draw :grid []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :window/vec
          scale   :window/scale} :local/window} result]
    [drawable
     {:transform
      (fn [_ xs]
        (xs-xfs xs (+-xf (- ox) (- oy)) (*-xf (/ scale)) (+-xf (- tx) (- ty)) cat))
      :on-release
      (fn [_ xs]
        (let [[ax ay bx by] (xs-xfs xs (+-xf tx ty) (*-xf scale) cat)
              size (js/Math.abs (min (- bx ax) (- by ay)))]
          (if (> size 0)
            (dispatch :canvas/draw-grid-size ax ay size))))}
     (fn [_ xs]
       (let [[ax ay bx by] (xs-xfs xs (+-xf tx ty) (*-xf scale) cat)
             size (min (- bx ax) (- by ay))]
         [:g
          [:path {:d (join " " ["M" ax ay "h" size "v" size "H" ax "Z"])}]
          [text {:x bx :y ay :fill "white"}
           (-> (/ size scale)
               (js/Math.abs)
               (js/Math.round)
               (str "px"))]]))]))

(defmethod draw :ruler []
  (let [result (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :window/vec
          scale   :window/scale
          {align :canvas/snap-grid} :window/canvas}
         :local/window} result]
    [drawable
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
     (fn [_ [ax ay bx by]]
       [:g
        [:line {:x1 ax :y1 ay :x2 bx :y2 by}]
        [text {:x (- bx 48) :y (- by 8) :fill "white"}
         (-> (chebyshev ax ay bx by)
             (px->ft (* grid-size scale))
             (str "ft."))]])]))

(defmethod draw :circle []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :window/vec
          scale   :window/scale
          {align :canvas/snap-grid} :window/canvas}
         :local/window} result]
    [drawable
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
     (fn [_ xs]
       (let [[ax ay bx by] (xs-xfs xs (+-xf tx ty) (*-xf scale) cat)
             radius (chebyshev ax ay bx by)]
         [:g
          [:circle {:cx ax :cy ay :r radius}]
          [text {:x ax :y ay :fill "white"}
           (-> radius (px->ft (* grid-size scale)) (str "ft. radius"))]]))]))

(defmethod draw :rect []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :window/vec
          scale   :window/scale
          {align :canvas/snap-grid} :window/canvas}
         :local/window} result]
    [drawable
     {:transform
      (fn [event xs]
        (let [xf (comp (+-xf (- ox) (- oy)) (*-xf (/ scale)) (+-xf (- tx) (- ty)))]
          (if (not= align (.-metaKey event))
            (xs-xfs xs xf (r-xf (/ grid-size 2)) cat)
            (xs-xfs xs xf cat))))
      :on-release
      (fn [_ xs]
        (dispatch :shape/create :rect xs))}
     (fn [_ xs]
       (let [[ax ay bx by] (xs-xfs xs (+-xf tx ty) (*-xf scale) cat)]
         [:g
          [:path {:d (join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])}]
          [text {:x (+ ax 8) :y (- ay 8) :fill "white"}
           (let [w (px->ft (js/Math.abs (- bx ax)) (* grid-size scale))
                 h (px->ft (js/Math.abs (- by ay)) (* grid-size scale))]
             (str w "ft. x " h "ft."))]]))]))

(defmethod draw :line []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :window/vec
          scale   :window/scale
          {align :canvas/snap-grid} :window/canvas}
         :local/window} result]
    [drawable
     {:transform
      (fn [event xs]
        (let [xf (comp (+-xf (- ox) (- oy)) (*-xf (/ scale)) (+-xf (- tx) (- ty)))]
          (if (not= align (.-metaKey event))
            (xs-xfs xs xf (r-xf (/ grid-size 2)) cat)
            (xs-xfs xs xf cat))))
      :on-release
      (fn [_ xs]
        (dispatch :shape/create :line xs))}
     (fn [_ xs]
       (let [[ax ay bx by] (xs-xfs xs (+-xf tx ty) (*-xf scale) cat)]
         [:g [:line {:x1 ax :y1 ay :x2 bx :y2 by}]
          [text {:x (+ ax 8) :y (- ay 8) :fill "white"}
           (-> (chebyshev ax ay bx by)
               (px->ft (* grid-size scale))
               (str "ft."))]]))]))

(defmethod draw :cone []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {[ox oy] :bounds/self
         {[tx ty] :window/vec
          scale   :window/scale} :local/window} result]
    [drawable
     {:transform
      (fn [_ xs]
        (let [xf (comp (+-xf (- ox) (- oy)) (*-xf (/ scale)) (+-xf (- tx) (- ty)))]
          (xs-xfs xs xf cat)))
      :on-release
      (fn [_ xs]
        (dispatch :shape/create :cone xs))}
     (fn [_ xs]
       (let [[ax ay bx by] (xs-xfs xs (+-xf tx ty) (*-xf scale) cat)]
         [:g
          [:polygon {:points (join " " (triangle ax ay bx by))}]
          [text {:x (+ bx 16) :y (+ by 16) :fill "white"}
           (-> (euclidean ax ay bx by)
               (px->ft (* grid-size scale))
               (str "ft."))]]))]))

(defmethod draw :poly []
  (let [dispatch (use-dispatch)]
    [polygon
     {:on-create
      (fn [_ xs]
        (dispatch :shape/create :poly xs))}]))

(defmethod draw :mask []
  (let [dispatch (use-dispatch)]
    [polygon
     {:on-create
      (fn [event xs]
        (dispatch :mask/create (not (.-shiftKey event)) xs))}]))
