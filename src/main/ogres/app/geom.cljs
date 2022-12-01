(ns ogres.app.geom)

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

(defn bounding-box
  "Returns the bounding box of the given points as the top-left and bottom-right
   points in the form of [ax ay bx by]."
  [& vs]
  (let [[xs ys] (partition (count vs) (apply interleave vs))]
    [(apply min xs) (apply min ys) (apply max xs) (apply max ys)]))
