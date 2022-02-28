(ns ogre.tools.geom)

(def epsilon (/ 1 1000000))

(defn near-zero? [n]
  (< (js/Math.abs n) epsilon))

(defn euclidean
  "Returns the euclidean distance from [ax ay] to [bx by]."
  [ax ay bx by]
  (js/Math.hypot (- bx ax) (- by ay)))

(defn chebyshev
  "Returns the chebyshev distance from [ax ay] to [bx by]."
  [ax ay bx by]
  (max (js/Math.abs (- ax bx))
       (js/Math.abs (- ay by))))

(defn triangle
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

(defn normalize
  "Rearranges the two given vectors such that the first vector represents
   the top-left corner and the second represents the bottom-right corner."
  [[ax ay bx by]]
  [(min ax bx) (min ay by) (max ax bx) (max ay by)])

(defn within?
  "Returns true if the given vector [x y] is within the given bounds, false
   otherwise."
  [x y [ax ay bx by]]
  (and (> x ax) (> y ay) (< x bx) (< y by)))

(defn cross [ax ay bx by]
  (- (* ax by) (* ay bx)))

(defn intersection
  "Returns a description of the relationship between the given segments as a
   tuple whose first element is either :collinear, :parallel, :intersecting,
   or :non-intersecting.
   https://stackoverflow.com/a/565282"
  [px py rx ry qx qy sx sy]
  (let [ofx (- qx px)
        ofy (- qy py)
        rdx (- rx px)
        rdy (- ry py)
        sdx (- sx qx)
        sdy (- sy qy)
        rcs (cross rdx rdy sdx sdy)
        ocr (cross ofx ofy rdx rdy)]
    (if (near-zero? rcs)
      (if (near-zero? ocr)
        ;; collinear
        ;; r × s = 0 and (q − p) × r = 0
        [:collinear]

        ;; parallel
        ;; r × s = 0 and (q − p) × r ≠ 0
        [:parallel])

      ;; intersecting
      ;; r × s ≠ 0 and 0 ≤ t ≤ 1 and 0 ≤ u ≤ 1
      (if (<= 0 (/ ocr rcs) 1)
        (let [t (/ (cross ofx ofy sdx sdy) rcs)]
          (if (<= 0 t 1)

            ;; intersects at
            ;; p + t r
            [:intersecting (+ px (* t rdx)) (+ py (* t rdy))]
            [:non-intersecting]))
        [:non-intersecting]))))
