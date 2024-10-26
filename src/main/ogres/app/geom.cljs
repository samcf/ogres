(ns ogres.app.geom
  (:require [ogres.app.const :refer [grid-size]]
            [ogres.app.util :refer [round]]))

(defn euclidean-distance
  "Returns the euclidean distance from [Ax Ay] to [Bx By]."
  [ax ay bx by]
  (js/Math.hypot (- bx ax) (- by ay)))

(defn chebyshev-distance
  "Returns the chebyshev distance from [Ax Ay] to [Bx By]."
  [ax ay bx by]
  (max (js/Math.abs (- ax bx))
       (js/Math.abs (- ay by))))

(defn cone-points
  "Returns the vertices of an isosceles triangle whose altitude is equal to
   the length of the base."
  [ax ay bx by]
  (let [alt (js/Math.hypot (- bx ax) (- by ay))
        hyp (js/Math.hypot alt (/ alt 2))
        rad (js/Math.atan2 (- by ay) (- bx ax))]
    [ax
     ay
     (+ ax (* hyp (js/Math.cos (+ rad 0.46))))
     (+ ay (* hyp (js/Math.sin (+ rad 0.46))))
     (+ ax (* hyp (js/Math.cos (- rad 0.46))))
     (+ ay (* hyp (js/Math.sin (- rad 0.46))))]))

(defn point-within-rect
  "Returns true if the point [Ax Ay] is within the given bounding
   rectangle [Bx By Cx Cy], false otherwise."
  [[ax ay] [bx by cx cy]]
  (and (> ax bx) (> ay by) (< ax cx) (< ay cy)))

(defn rect-intersects-rect
  "Returns true if any of the 4 corners of [Ax Ay Bx by] are within the
   rect [Cx Cy Dx Dy]. Each rect must be given as (Min, Max)."
  [[ax ay bx by] bounds]
  (some (fn [point] (point-within-rect point bounds))
        [[ax ay] [bx ay] [bx by] [ax by]]))

(defn bounding-rect
  "Returns an axis-aligned minimum bounding rectangle (AABB) of the set of
   points given in the form of [Ax Ay Bx By [...]] as points [Ax Ay Bx By]
   where A is the top-left corner and B is the bottom-right corner."
  [[ax ay :as points]]
  (loop [points points min-x ax min-y ay max-x ax max-y ay]
    (if (seq points)
      (let [[x y] points]
        (recur (rest (rest points)) (min min-x x) (min min-y y) (max max-x x) (max max-y y)))
      [min-x min-y max-x max-y])))

(defn alignment-xf
  "Returns a transducer which expects a collection of points in the
   form of [Ax Ay Bx By ...] and aligns those points to the nearest
   grid intersection given a drag delta (dx dy) and the current
   grid offset (ox oy)."
  [dx dy ox oy]
  (comp (partition-all 2)
        (mapcat
         (fn [[x y]]
           [(+ (round (- (+ x dx) ox) grid-size) ox)
            (+ (round (- (+ y dy) oy) grid-size) oy)]))))

(defn ^:private clockwise?
  "Returns true if the given polygon has a clockwise winding order, false
   otherwise. Points must be given in the form of [Ax Ay Bx By [...]]."
  [[ax ay :as xs]]
  (loop [[bx by cx cy :as xs] xs sum 0]
    (if (some? cx)
      (recur (rest (rest xs)) (+ (* (- cx bx) (+ cy by)) sum))
      (neg? (+ (* (- ax bx) (+ ay by)) sum)))))

(defn reorient
  "Returns the given polygon in its clockwise winding order."
  [xs]
  (if (clockwise? xs) xs
      (into [] cat (reverse (partition 2 xs)))))

(defmulti object-bounding-rect
  "Returns an axis-aligned minimum bounding rectangle (AABB) of the given
   object in the form of [Ax Ay Bx By] where A is the top-left corner and B
   is the bottom-right corner."
  :object/type)

;; Tokens are defined by their position {A} and size.
(defmethod object-bounding-rect :token/token
  [{[ax ay] :object/point size :token/size}]
  (let [rd (/ (* (or size 5) grid-size) 10)]
    [(- ax rd) (- ay rd)
     (+ ax rd) (+ ay rd)]))

;; Circles are defined by points {A, B} where A is the center and B is
;; some point on the circumference.
(defmethod object-bounding-rect :shape/circle
  [{[ax ay] :object/point [bx by] :shape/points}]
  (let [rd (chebyshev-distance 0 0 bx by)]
    [(- ax rd) (- ay rd)
     (+ ax rd) (+ ay rd)]))

;; Cones are isosceles triangles defined by points {A, B} where A is
;; the apex and B is the center of the base.
(defmethod object-bounding-rect :shape/cone
  [{[ax ay] :object/point [bx by] :shape/points}]
  (bounding-rect (cone-points ax ay (+ ax bx) (+ ay by))))

;; Rectangles are defined by points {A, B} where A and B are opposite and
;; opposing corners, such as top-left and bottom-right.
(defmethod object-bounding-rect :shape/rect
  [{[ax ay] :object/point [bx by] :shape/points}]
  (bounding-rect [ax ay (+ ax bx) (+ ay by)]))

;; Polygons are defined by points {A, B, C, [...]} where each point is
;; adjacent to its neighbors.
(defmethod object-bounding-rect :shape/poly
  [{[ax ay] :object/point points :shape/points}]
  (let [xf (comp (partition-all 2) (mapcat (fn [[bx by]] [(+ ax bx) (+ ay by)])))]
    (bounding-rect (into [ax ay] xf points))))

;; Lines are defined by points {A, B}, opposite ends of the segment.
(defmethod object-bounding-rect :shape/line
  [{[ax ay] :object/point [bx by] :shape/points}]
  (bounding-rect [ax ay (+ ax bx) (+ ay by)]))

;; Notes are defined by the point {A} and is fixed square bound.
(defmethod object-bounding-rect :note/note
  [{[ax ay] :object/point}]
  (bounding-rect [ax ay (+ ax 42) (+ ay 42)]))
