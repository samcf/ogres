(ns ogres.app.geom
  (:require [clojure.string :refer [join]]))

(def ^:private poly-path-xf
  (comp (partition-all 2) (mapcat (fn [[x y]] [x y \L]))))

(defn ^:private circle-path
  [[x y r]]
  (let [d (* r 2)]
    [\M x y
     \m r 0
     \a r r 0 1 0 (- d) 0
     \a r r 0 1 0 d 0 \z]))

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

(defn poly->path
  ([] [])
  ([path] (join " " path))
  ([path points]
   (into path (conj (pop (into [\M] poly-path-xf points)) \z))))

(defn circle->path
  ([] [])
  ([path] (join " " path))
  ([path circle]
   (into path (circle-path circle))))

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

(defmulti shape-bounding-rect
  "Returns an axis-aligned minimum bounding rectangle (AABB) of the given
   shape and its points in the form of [Ax Ay Bx By] where A is the
   top-left corner and B is the bottom-right corner."
  (fn [kind _] kind))

;; Circles are defined by points {A, B} where A is the center and B is
;; some point on the circumference.
(defmethod shape-bounding-rect :circle
  [_ [ax ay bx by]]
  (let [rd (euclidean-distance ax ay bx by)]
    [(- ax rd) (- ay rd)
     (+ ax rd) (+ ay rd)]))

;; Rectangles are defined by points {A, B} where A and B are opposite and
;; opposing corners, such as top-left and bottom-right.
(defmethod shape-bounding-rect :rect
  [_ points]
  (bounding-rect points))

;; Polygons are defined by points {A, B, C, [...]} where each point is
;; adjacent to its neighbors.
(defmethod shape-bounding-rect :poly
  [_ points]
  (bounding-rect points))

;; Lines are defined by points {A, B}, opposite ends of the segment.
(defmethod shape-bounding-rect :line
  [_ points]
  (bounding-rect points))

;; Cones are isosceles triangles defined by points {A, B} where A is
;; the apex perpendicular and B is the center of the base.
(defmethod shape-bounding-rect :cone
  [_ [ax ay bx by]]
  (bounding-rect (cone-points ax ay bx by)))
