(ns ogres.app.geom
  (:require [clojure.math :refer [floor ceil]]
            [ogres.app.const :refer [grid-size half-size]]
            [ogres.app.util :refer [round]]))

(def ^:const deg45->rad (/ js/Math.PI 4))
(def ^:const deg45->sin (js/Math.sin deg45->rad))

(defn euclidean-distance
  "Returns the euclidean distance from [Ax Ay] to [Bx By]."
  [ax ay bx by]
  (js/Math.hypot (- bx ax) (- by ay)))

(defn chebyshev-distance
  "Returns the chebyshev distance from [Ax Ay] to [Bx By]."
  [ax ay bx by]
  (max (js/Math.abs (- ax bx))
       (js/Math.abs (- ay by))))

(defn clockwise-triangle?
  "Returns true if the triangle {Ax Ay Bx By Cx Cy} is in
   clockwise winding order, false otherwise."
  [ax ay bx by cx cy]
  (pos? (- (* (- bx ax) (- cy ay))
           (* (- by ay) (- cx ax)))))

(defn point-within-circle?
  "Returns true if the point {Ax Ay} is within the circle
   {Cx Cy Radius}, false otherwise."
  [cx cy radius ax ay]
  (< (euclidean-distance ax ay cx cy) radius))

(defn point-within-triangle?
  "Returns true if the point {Sx Sy} is within the triangle
   {Ax Ay Bx By Cx Cy} (in clockwise order), false otherwise."
  [ax ay bx by cx cy sx sy]
  (and (clockwise-triangle? ax ay bx by sx sy)
       (clockwise-triangle? cx cy ax ay sx sy)
       (clockwise-triangle? bx by cx cy sx sy)))

(defn line-points
  "Returns a vector of points as [Ax Ay Bx By Cx Cy Dx Dy] for each
   corner of the oriented rectangle defined by the line {Ax Ay Bx By}
   in clock-wise winding order."
  [ax ay bx by ln]
  (if (= ay by)
    [ax (+ ay ln) bx (+ ay ln) bx (- ay ln) ax (- ay ln)]
    (let [ma (/ (- bx ax) (- ay by))
          mb (js/Math.sqrt (inc (* ma ma)))
          dx (* ln (/ mb))
          dy (* ln (/ ma mb))
          si (js/Math.sign (- ay by))]
      [(+ ax (* dx si -1))
       (+ ay (* dy si -1))
       (+ bx (* dx si -1))
       (+ by (* dy si -1))
       (+ bx (* dx si))
       (+ by (* dy si))
       (+ ax (* dx si))
       (+ ay (* dy si))])))

(defn rect-points
  "Returns a vector of points as [Ax Ay Bx By Cx Cy Dx Dy] for each corner
   of the axis-aligned rectangle defined by the top-left corner {Ax Ay} in
   clock-wise winding order."
  [[ax ay]]
  [ax ay (+ ax grid-size) ay (+ ax grid-size) (+ ay grid-size) ax (+ ay grid-size)])

(defn cone-points
  "Returns a vector of points as [Ax Ay Bx By Cx Cy] for each vertex of the
   isosceles triangle given as the segment {Ax Ay Bx By} which defines its
   altitude."
  [ax ay bx by]
  (let [alt (js/Math.hypot (- bx ax) (- by ay))
        hyp (js/Math.hypot alt (/ alt 2))
        rad (js/Math.atan2 (- by ay) (- bx ax))]
    [ax
     ay
     (+ ax (* hyp (js/Math.cos (- rad 0.46))))
     (+ ay (* hyp (js/Math.sin (- rad 0.46))))
     (+ ax (* hyp (js/Math.cos (+ rad 0.46))))
     (+ ay (* hyp (js/Math.sin (+ rad 0.46))))]))

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
   grid intersection given a drag delta (dx dy)."
  [dx dy]
  (comp (partition-all 2)
        (mapcat
         (fn [[x y]]
           [(round (+ x dx) grid-size)
            (round (+ y dy) grid-size)]))))

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

(defn ^:private tile-edge
  [ax ay bx by cx cy]
  (cond (= cy ay) 0
        (= cx bx) 1
        (= cy by) 2
        (= cx ax) 3))

(def ^:private tile-edge-path
  [[+1 +0 +0 +1]
   [+0 +1 -1 +0]
   [-1 +0 +0 -1]
   [+0 -1 +1 +0]])

(defn ^:private path-around-tiles
  [points]
  (if (not (seq points)) []
      (let [xs (into  [] (comp (partition-all 2) (mapcat rect-points)) points)
            vs (into #{} (partition-all 2) xs)
            [ax ay bx by] (bounding-rect xs)
            [sx sy] (first (filter (fn [[_ y]] (= y ay)) vs))]
        (loop [nx sx ny sy rs (transient []) ed 0]
          (if (and (= nx sx) (= ny sy) (> (count rs) 0)) (persistent! rs)
              (let [[ex ey fx fy] (tile-edge-path ed)
                    cx (+ nx (* ex grid-size))
                    cy (+ ny (* ey grid-size))]
                (if (contains? vs [cx cy])
                  (recur cx cy (conj! rs cx cy) (or (tile-edge ax ay bx by cx cy) ed))
                  (let [dx (+ nx (* fx grid-size))
                        dy (+ ny (* fy grid-size))]
                    (recur dx dy (conj! rs dx dy) (or (tile-edge ax ay bx by dx dy) ed))))))))))

(defn tile-path-circle
  [ax ay rd]
  (let [sz grid-size
        ln (* rd deg45->sin)
        cx (round floor (- ax rd) sz)
        cy (round floor (- ay rd) sz)
        dx (round ceil  (+ ax rd) sz)
        dy (round ceil  (+ ay rd) sz)
        ex (round ceil  (- ax ln) sz)
        ey (round ceil  (- ay ln) sz)
        fx (round floor (+ ax ln) sz)
        fy (round floor (+ ay ln) sz)]
    (loop [px cx py cy rs (transient [])]
      (cond (> px dx) (path-around-tiles (persistent! rs))
            (> py dy) (recur (+ px sz) cy rs)
              ;; points found within the inscribed square can be omitted
              ;; since they definitionally lie within the circle.
            (and (= py ey) (>= px ex) (< px fx)
                 (not (and (= ex fx) (= ey fy)))
                 (not (and (= px ex) (= py ey)))
                 (not (and (= px ex) (= py fy)))
                 (not (and (= px (- fx sz)) (= py ey)))
                 (not (and (= px (- fx sz)) (= py fy))))
            (recur px fy rs)
            (point-within-circle? ax ay rd (+ px half-size) (+ py half-size))
            (recur px (+ py sz) (conj! rs px py))
            :else
            (recur px (+ py sz) rs)))))

(defn tile-path-cone
  [[ax ay bx by cx cy :as xs]]
  (let [[yx yy zx zy] (bounding-rect xs)
        sz grid-size
        dx (round floor yx sz)
        dy (round floor yy sz)
        ex (round ceil  zx sz)
        ey (round ceil  zy sz)]
    (loop [px dx py dy rs (transient [])]
      (cond (> px ex) (path-around-tiles (persistent! rs))
            (> py ey)
            (recur (+ px sz) dy rs)
            (or (point-within-triangle? ax ay bx by cx cy (+ px 14) (+ py 14))
                (point-within-triangle? ax ay bx by cx cy (+ px 56) (+ py 14))
                (point-within-triangle? ax ay bx by cx cy (+ px 14) (+ py 56))
                (point-within-triangle? ax ay bx by cx cy (+ px 56) (+ py 56)))
            (recur px (+ py sz) (conj! rs px py))
            :else
            (recur px (+ py sz) rs)))))

(defn tile-path-line
  [[ax ay bx by cx cy dx dy :as xs]]
  (let [[ex ey fx fy] (bounding-rect xs)
        sz grid-size
        hz half-size
        ex (round floor ex sz)
        ey (round floor ey sz)
        fx (round ceil  fx sz)
        fy (round ceil  fy sz)]
    (loop [px ex py ey rs (transient [])]
      (cond (> px fx) (path-around-tiles (persistent! rs))
            (> py fy) (recur (+ px sz) ey rs)
            (or (point-within-triangle? ax ay bx by cx cy (+ px hz) (+ py hz))
                (point-within-triangle? ax ay cx cy dx dy (+ px hz) (+ py hz)))
            (recur px (+ py sz) (conj! rs px py))
            :else
            (recur px (+ py sz) rs)))))

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
  (bounding-rect (line-points ax ay (+ ax bx) (+ ay by) half-size)))

;; Notes are defined by the point {A} and is fixed square bound.
(defmethod object-bounding-rect :note/note
  [{[ax ay] :object/point}]
  (bounding-rect [ax ay (+ ax 42) (+ ay 42)]))

(defmulti object-tile-path
  "Returns a path in the form of [Ax Ay [Bx By ...]] of a perimeter around
   the given object. This perimeter is aligned with the grid tiles. This
   is useful for illustrating, for example, which tiles are affected by
   a circle of arbitrary position and radius."
  (fn [object _ _ _ _]
    (:object/type object)))

(defmethod object-tile-path :default [] [])

(defmethod object-tile-path :shape/circle
  [{[ax ay] :object/point [bx by] :shape/points} dx dy]
  (let [ax (+ ax dx)
        ay (+ ay dy)
        bx (+ bx ax)
        by (+ by ay)
        rd (chebyshev-distance ax ay bx by)]
    (tile-path-circle ax ay rd)))

(defmethod object-tile-path :shape/cone
  [{[ax ay] :object/point [bx by] :shape/points} dx dy]
  (let [ax (+ ax dx)
        ay (+ ay dy)
        bx (+ bx ax)
        by (+ by ay)]
    (tile-path-cone (cone-points ax ay bx by))))

(defmethod object-tile-path :shape/line
  [{[ax ay] :object/point [bx by] :shape/points} dx dy]
  (let [ax (+ ax dx)
        ay (+ ay dy)
        bx (+ bx ax)
        by (+ by ay)]
    (tile-path-line (line-points ax ay bx by half-size))))

(defn object-alignment [entity]
  (case (:object/type entity)
    :shape/cone half-size
    :shape/line half-size
    :shape/poly half-size
    grid-size))
