(ns ogres.app.geom
  (:require [clojure.math :refer [floor ceil]]
            [ogres.app.const :refer [grid-size half-size]]
            [ogres.app.vec :as vec :refer [Vec2 Segment]]
            [ogres.app.geom :as geom]))

(def ^:const deg45->rad (/ js/Math.PI 4))
(def ^:const deg45->sin (js/Math.sin deg45->rad))

(defn clockwise-triangle?
  [a b c]
  (> (- (* (- (.-x b) (.-x a)) (- (.-y c) (.-y a)))
        (* (- (.-y b) (.-y a)) (- (.-x c) (.-x a)))) 0))

(defn point-within-rect?
  [point segment]
  (and (< (.-x (.-a segment)) (.-x point) (.-x (.-b segment)))
       (< (.-y (.-a segment)) (.-y point) (.-y (.-b segment)))))

(defn point-within-circle?
  [point center radius]
  (< (vec/dist center point) radius))

(defn point-within-triangle?
  [a b c v]
  (and (clockwise-triangle? a b v)
       (clockwise-triangle? c a v)
       (clockwise-triangle? b c v)))

(defn line-points [segment]
  (let [ln half-size
        av (.-a segment)
        bv (.-b segment)
        ax (.-x (.-a segment))
        ay (.-y (.-a segment))
        bx (.-x (.-b segment))
        by (.-y (.-b segment))]
    (if (= ay by)
      [(vec/shift av 0 ln)
       (vec/shift bv 0 ln)
       (vec/shift bv 0 (- ay ln))
       (vec/shift av 0 (- ay ln))]
      (let [ma (/ (- bx ax) (- ay by))
            mb (js/Math.sqrt (inc (* ma ma)))
            si (js/Math.sign (- ay by))
            dv (Vec2. (* ln si (/ mb)) (* ln si (/ ma mb)))]
        [(vec/add av (vec/mul dv -1))
         (vec/add bv (vec/mul dv -1))
         (vec/add bv dv)
         (vec/add av dv)]))))

(defn cone-points [segment]
  (let [src (.-a segment)
        dst (.-b segment)
        alt (vec/dist src dst)
        hyp (js/Math.hypot alt (/ alt 2))
        rad (vec/heading (vec/sub dst src))
        ax (* hyp (js/Math.cos (- rad 0.46)))
        ay (* hyp (js/Math.sin (- rad 0.46)))
        bx (* hyp (js/Math.cos (+ rad 0.46)))
        by (* hyp (js/Math.sin (+ rad 0.46)))]
    [src (vec/add src (Vec2. ax ay)) (vec/add src (Vec2. bx by))]))

(defn tile-points [point]
  [point
   (vec/shift point grid-size 0)
   (vec/shift point grid-size)
   (vec/shift point 0 grid-size)])

(defn rect-intersects-rect [a b]
  (not (or (< (.-x (.-b a)) (.-x (.-a b)))
           (< (.-x (.-b b)) (.-x (.-a a)))
           (< (.-y (.-b a)) (.-y (.-a b)))
           (< (.-y (.-b b)) (.-y (.-a a))))))

(defn bounding-rect-rf
  ([] vec/zero-segment)
  ([s] s)
  ([s v]
   (if (identical? s vec/zero-segment)
     (Segment. v v)
     (Segment.
      (Vec2. (min (.-x (.-a s)) (.-x v)) (min (.-y (.-a s)) (.-y v)))
      (Vec2. (max (.-x (.-b s)) (.-x v)) (max (.-y (.-b s)) (.-y v)))))))

(defn bounding-rect
  [points]
  (reduce bounding-rect-rf (bounding-rect-rf) points))

(defn clockwise?
  [[ax ay :as xs]]
  (loop [[bx by cx cy :as xs] xs sum 0]
    (if (some? cx)
      (recur (rest (rest xs)) (+ (* (- cx bx) (+ cy by)) sum))
      (neg? (+ (* (- ax bx) (+ ay by)) sum)))))

(defn reorient
  [xs]
  (if (clockwise? xs) xs
      (into [] cat (reverse (partition 2 xs)))))

(defn tile-edge [a b c]
  (cond (= (.-y c) (.-y a)) 0
        (= (.-x c) (.-x b)) 1
        (= (.-y c) (.-y b)) 2
        (= (.-x c) (.-x a)) 3))

(def tile-edge-path
  [(Segment. (Vec2.  1 0) (Vec2. 0 1))
   (Segment. (Vec2.  0 1) (Vec2. -1 0))
   (Segment. (Vec2. -1 0) (Vec2. 0 -1))
   (Segment. (Vec2. 0 -1) (Vec2. 1 0))])

(defn path-around-tiles
  [points]
  (if (not (seq points)) []
      (let [corners (into [] (mapcat tile-points) points)
            visited (into #{} corners)
            bounds (bounding-rect corners)
            src (.-a bounds)
            dst (.-b bounds)
            start (first (filter (fn [v] (= (.-y v) (.-y src))) visited))]
        (loop [n start rs (transient []) ed 0]
          (if (and (= n start) (> (count rs) 0)) (persistent! rs)
              (let [s (tile-edge-path ed)
                    c (vec/add (vec/mul (.-a s) grid-size) n)]
                (if (contains? visited c)
                  (recur c (conj! rs c) (or (tile-edge src dst c) ed))
                  (let [d (vec/add (vec/mul (.-b s) grid-size) n)]
                    (recur d (conj! rs d) (or (tile-edge src dst d) ed))))))))))

(defn tile-path-circle
  [center radius]
  (let [sz grid-size
        hs half-size
        ln (* radius deg45->sin)
        av (vec/rnd (vec/shift center (- radius)) sz floor)
        bv (vec/rnd (vec/shift center radius) sz ceil)
        cv (vec/rnd (vec/shift center (- ln)) sz ceil)
        dv (vec/rnd (vec/shift center ln) sz floor)]
    (loop [x (.-x av) y (.-y av) rs (transient [])]
      (let [t (Vec2. x y)]
        (cond (> x (.-x bv)) (path-around-tiles (persistent! rs))
              (> y (.-y bv)) (recur (+ x sz) (.-y av) rs)
              (and (= y (.-y cv)) (>= x (.-x cv)) (< x (.-x dv))
                   (not (= cv dv))
                   (not (= cv t))
                   (not (and (= x (.-x cv)) (= y (.-y dv))))
                   (not (and (= x (- (.-x dv) sz)) (= y (.-y cv))))
                   (not (and (= x (- (.-x dv) sz)) (= y (.-y dv)))))
              (recur x (.-y dv) rs)
              (point-within-circle? (vec/shift t hs) center radius)
              (recur x (+ y sz) (conj! rs t))
              :else
              (recur x (+ y sz) rs))))))

(defn tile-path-cone
  [[a b c :as xs]]
  (let [sz grid-size
        rt (bounding-rect xs)
        tl (vec/rnd (.-a rt) sz floor)
        br (vec/rnd (.-b rt) sz ceil)]
    (loop [x (.-x tl) y (.-y tl) rs (transient [])]
      (let [t (Vec2. x y)]
        (cond (> x (.-x br)) (path-around-tiles (persistent! rs))
              (> y (.-y br))
              (recur (+ x sz) (.-y tl) rs)
              (or (point-within-triangle? a b c (vec/shift t 14 14))
                  (point-within-triangle? a b c (vec/shift t 56 14))
                  (point-within-triangle? a b c (vec/shift t 14 56))
                  (point-within-triangle? a b c (vec/shift t 56 56)))
              (recur x (+ y sz) (conj! rs t))
              :else
              (recur x (+ y sz) rs))))))

(defn tile-path-line
  [[a b c d :as xs]]
  (let [sz grid-size
        rt (bounding-rect xs)
        tl (vec/rnd (.-a rt) sz floor)
        br (vec/rnd (.-b rt) sz ceil)]
    (loop [x (.-x tl) y (.-y tl) rs (transient [])]
      (let [t (vec/shift (Vec2. x y) half-size)]
        (cond (> x (.-x br)) (path-around-tiles (persistent! rs))
              (> y (.-y br)) (recur (+ x sz) (.-y tl) rs)
              (or (point-within-triangle? a b c t)
                  (point-within-triangle? a c d t))
              (recur x (+ y sz) (conj! rs (Vec2. x y)))
              :else
              (recur x (+ y sz) rs))))))

(defmulti object-bounding-rect
  :object/type)

;; Tokens are defined by their position {A} and size.
(defmethod object-bounding-rect :token/token
  [{src :object/point size :token/size}]
  (let [rad (/ (* (or size 5) grid-size) 10)]
    (Segment. (vec/shift src (- rad)) (vec/shift src rad))))

;; Circles are defined by points {A, B} where A is the center and B is
;; some point on the circumference.
(defmethod object-bounding-rect :shape/circle
  [{src :object/point [dst] :shape/points}]
  (let [rad (vec/dist-cheb dst)]
    (Segment. (vec/shift src (- rad)) (vec/shift src rad))))

;; Cones are isosceles triangles defined by points {A, B} where A is
;; the apex and B is the center of the base.
(defmethod object-bounding-rect :shape/cone
  [{src :object/point [dst] :shape/points}]
  (let [dst (vec/add dst src)]
    (bounding-rect (cone-points (Segment. src dst)))))

;; Rectangles are defined by points {A, B} where A and B are opposite and
;; opposing corners, such as top-left and bottom-right.
(defmethod object-bounding-rect :shape/rect
  [{src :object/point [dst] :shape/points}]
  (let [dst (vec/add dst src)]
    (bounding-rect (list src dst))))

;; Polygons are defined by points {A, B, C, [...]} where each point is
;; adjacent to its neighbors.
(defmethod object-bounding-rect :shape/poly
  [{src :object/point points :shape/points}]
  (let [xfr (map (fn [v] (vec/add src v)))]
    (bounding-rect (list* src (sequence xfr points)))))

;; Lines are defined by points {A, B}, opposite ends of the segment.
(defmethod object-bounding-rect :shape/line
  [{src :object/point [dst] :shape/points}]
  (let [dst (vec/add dst src)]
    (bounding-rect (line-points (Segment. src dst)))))

;; Notes are defined by the point {A} and is fixed square bound.
(defmethod object-bounding-rect :note/note
  [{src :object/point}]
  (Segment. src (vec/shift src 42)))

(defmulti object-tile-path
  (fn [object _ _]
    (:object/type object)))

(defmethod object-tile-path :default [] [])

(defmethod object-tile-path :shape/circle
  [{src :object/point [dst] :shape/points} delta]
  (let [src (vec/add src delta)
        dst (vec/add dst src)
        rad (vec/dist-cheb src dst)]
    (tile-path-circle src rad)))

(defmethod object-tile-path :shape/cone
  [{src :object/point [dst] :shape/points} delta]
  (let [src (vec/add src delta)
        dst (vec/add dst src)]
    (tile-path-cone (cone-points (Segment. src dst)))))

(defmethod object-tile-path :shape/line
  [{src :object/point [dst] :shape/points} delta]
  (let [src (vec/add src delta)
        dst (vec/add dst src)]
    (tile-path-line (line-points (Segment. src dst)))))

(defn object-alignment [entity]
  (case (:object/type entity)
    :shape/cone half-size
    :shape/line half-size
    :shape/poly half-size
    grid-size))
