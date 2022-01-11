(ns ogre.tools.render.draw
  (:require [clojure.string :as string]
            [ogre.tools.geom :refer [chebyshev euclidean triangle]]
            [ogre.tools.state :refer [use-query]]
            [ogre.tools.vec :as vec]
            [react-draggable :as draggable]
            [uix.core.alpha :as uix]
            [uix.dom.alpha :refer [create-portal]]))

(defn ft->px [ft size] (* (/ ft 5) size))
(defn px->ft [px size] (js/Math.round (* (/ px size) 5)))
(defn ->canvas [t s & vs] (mapv (fn [v] (vec/- (vec/s (/ s) v) t)) vs))
(defn ->screen [t s & vs] (mapv (fn [v] (vec/s s (vec/+ v t))) vs))

(defn text [attrs child]
  [:text.canvas-text attrs child])

(defn drawable [{:keys [transform on-release]} render-fn]
  (let [init  [nil nil nil]
        state (uix/state init)]
    [:<>
     [:> draggable
      {:position #js {:x 0 :y 0}
       :on-start
       (fn [event data]
         (.stopPropagation event)
         (let [src [(.-clientX event) (.-clientY event)]]
           (reset! state [event src src])))
       :on-drag
       (fn [event data]
         (swap! state
                (fn [[_ src _]]
                  (let [[ax ay] src]
                    [event src [(+ ax (.-x data)) (+ ay (.-y data))]]))))
       :on-stop
       (fn [event]
         (let [[_ src dst] (deref state)]
           (reset! state init)
           (apply on-release event (transform event src dst))))}
      [:rect
       {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"
        :style {:will-change "transform"}}]]
     (let [[event src dst] (deref state)]
       (if (seq src)
         (apply render-fn event (transform event src dst))))]))

(def draw-query
  {:pull
   [:bounds/self
    {:root/canvas
     [[:grid/size :default 70]
      [:grid/align :default false]
      [:zoom/scale :default 1]
      [:pos/vec :default [0 0]]]}]})

(defmulti draw :mode)
(defmethod draw :default [] nil)

(defmethod draw :select [{:keys [node]}]
  (let [[result dispatch] (use-query draw-query)
        {offset :bounds/self
         {trans :pos/vec
          scale :zoom/scale
          size  :grid/size} :root/canvas} result]
    [drawable
     {:transform
      (fn [event src dst]
        (let [[src dst] (mapv (fn [v] (vec/- v offset)) [src dst])
              [src dst] (->canvas trans scale src dst)]
          [src dst]))
      :on-release
      (fn [_ src dst]
        (dispatch :selection/from-rect (into src dst)))}
     (fn [_ src dst]
       (let [[src dst] (->screen trans scale src dst)
             [ax ay]   src
             [bx by]   dst]
         (create-portal
          [:path {:d (string/join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])}] @node)))]))

(defmethod draw :grid []
  (let [[result dispatch] (use-query draw-query)
        {offset :bounds/self
         {trans :pos/vec
          scale :zoom/scale
          size  :grid/size} :root/canvas} result]
    [drawable
     {:transform
      (fn [event src dst]
        (let [[src dst] (mapv (fn [v] (vec/- v offset)) [src dst])
              [src dst] (->canvas trans scale src dst)]
          [src dst]))
      :on-release
      (fn [_ src dst]
        (let [[src dst] (->screen trans scale src dst)
              [ax ay]   src
              size      (js/Math.abs (apply min (vec/- dst src)))]
          (if (> size 0)
            (dispatch :grid/draw ax ay size))))}
     (fn [_ src dst]
       (let [[src dst] (->screen trans scale src dst)
             [ax ay]   src
             [bx by]   dst
             size      (apply min (vec/- dst src))]
         [:g
          [:path {:d (string/join " " ["M" ax ay "h" size "v" size "H" ax "Z"])}]
          [text {:x bx :y ay :fill "white"}
           (-> (/ size scale)
               (js/Math.abs)
               (js/Math.round)
               (str "px"))]]))]))

(defmethod draw :ruler []
  (let [[result dispatch] (use-query draw-query)
        {offset :bounds/self
         {trans :pos/vec
          scale :zoom/scale
          align :grid/align
          size  :grid/size} :root/canvas} result]
    [drawable
     {:on-release identity
      :transform
      (fn [event src dst]
        (let [[src dst] (mapv (fn [v] (vec/- v offset)) [src dst])]
          (if (not= align (.-metaKey event))
            (let [[src dst] (->canvas trans scale src dst)
                  [src dst] (mapv (fn [v] (vec/r (/ size 2) v)) [src dst])
                  [src dst] (->screen trans scale src dst)]
              [src dst])
            [src dst])))}
     (fn [_ [ax ay] [bx by]]
       [:g
        [:line {:x1 ax :y1 ay :x2 bx :y2 by}]
        [text {:x (- bx 48) :y (- by 8) :fill "white"}
         (-> (chebyshev ax ay bx by)
             (px->ft (* size scale))
             (str "ft."))]])]))

(defmethod draw :circle []
  (let [[result dispatch] (use-query draw-query)
        {offset :bounds/self
         {trans :pos/vec
          scale :zoom/scale
          align :grid/align
          size  :grid/size} :root/canvas} result]
    [drawable
     {:transform
      (fn [event src dst]
        (let [[src dst] (mapv (fn [v] (vec/- v offset)) [src dst])
              [src dst] (->canvas trans scale src dst)]
          (if (not= align (.-metaKey event))
            (let [src (vec/r (/ size 2) src)
                  snp (vec/- src dst)
                  dst (vec/- dst (vec/- (vec/r size snp) snp))]
              [src dst])
            [src dst])))
      :on-release
      (fn [_ src dst]
        (dispatch :shape/create :circle (into src dst)))}
     (fn [_ src dst]
       (let [[src dst] (->screen trans scale src dst)
             radius    (apply chebyshev (into src dst))
             [ax ay]   src]
         [:g
          [:circle {:cx ax :cy ay :r radius}]
          [text {:x ax :y ay :fill "white"}
           (-> radius (px->ft (* size scale)) (str "ft. radius"))]]))]))

(defmethod draw :rect []
  (let [[result dispatch] (use-query draw-query)
        {offset :bounds/self
         {trans :pos/vec
          scale :zoom/scale
          align :grid/align
          size  :grid/size} :root/canvas} result]
    [drawable
     {:transform
      (fn [event src dst]
        (let [[src dst] (mapv (fn [v] (vec/- v offset)) [src dst])
              [src dst] (->canvas trans scale src dst)]
          (if (not= align (.-metaKey event))
            (let [[src dst] (mapv (fn [v] (vec/r (/ size 2) v)) [src dst])]
              [src dst])
            [src dst])))
      :on-release
      (fn [_ src dst]
        (dispatch :shape/create :rect (into src dst)))}
     (fn [_ src dst]
       (let [[src dst] (->screen trans scale src dst)
             [ax ay] src
             [bx by] dst]
         [:g
          [:path {:d (string/join " " ["M" ax ay "H" bx "V" by "H" ax "Z"])}]
          [text {:x (+ ax 8) :y (- ay 8) :fill "white"}
           (let [w (px->ft (js/Math.abs (- bx ax)) (* size scale))
                 h (px->ft (js/Math.abs (- by ay)) (* size scale))]
             (str w "ft. x " h "ft."))]]))]))

(defmethod draw :line []
  (let [[result dispatch] (use-query draw-query)
        {offset :bounds/self
         {trans :pos/vec
          scale :zoom/scale
          align :grid/align
          size  :grid/size} :root/canvas} result]
    [drawable
     {:transform
      (fn [event src dst]
        (let [[src dst] (mapv (fn [v] (vec/- v offset)) [src dst])
              [src dst] (->canvas trans scale src dst)]
          (if (not= align (.-metaKey event))
            (let [[src dst] (mapv (fn [v] (vec/r (/ size 2) v)) [src dst])]
              [src dst])
            [src dst])))
      :on-release
      (fn [_ src dst]
        (dispatch :shape/create :line (into src dst)))}
     (fn [_ src dst]
       (let [[src dst] (->screen trans scale src dst)
             [ax ay]   src
             [bx by]   dst]
         [:g [:line {:x1 ax :y1 ay :x2 bx :y2 by}]
          [text {:x (+ ax 8) :y (- ay 8) :fill "white"}
           (-> (chebyshev ax ay bx by)
               (px->ft (* size scale))
               (str "ft."))]]))]))

(defmethod draw :cone []
  (let [[result dispatch] (use-query draw-query)
        {offset :bounds/self
         {trans :pos/vec
          scale :zoom/scale
          align :grid/align
          size  :grid/size} :root/canvas} result]
    [drawable
     {:transform
      (fn [event src dst]
        (let [[src dst] (mapv (fn [v] (vec/- v offset)) [src dst])
              [src dst] (->canvas trans scale src dst)]
          (if (not= align (.-metaKey event))
            (let [src (vec/r size src)]
              [src dst])
            [src dst])))
      :on-release
      (fn [_ src dst]
        (dispatch :shape/create :cone (into src dst)))}
     (fn [_ src dst]
       (let [[src dst] (->screen trans scale src dst)
             [ax ay]   src
             [bx by]   dst]
         [:g
          [:polygon {:points (string/join " " (triangle ax ay bx by))}]
          [text {:x (+ bx 16) :y (+ by 16) :fill "white"}
           (-> (euclidean ax ay bx by)
               (px->ft (* size scale))
               (str "ft."))]]))]))
