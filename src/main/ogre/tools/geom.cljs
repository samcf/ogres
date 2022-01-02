(ns ogre.tools.geom)

(defn euclidean
  "Returns the euclidean distance from [ax ay] to [bx by]."
  [ax ay bx by]
  (.hypot js/Math (- bx ax) (- by ay)))

(defn chebyshev
  "Returns the chebyshev distance from [ax ay] to [bx by]."
  [ax ay bx by]
  (max (.abs js/Math (- ax bx))
       (.abs js/Math (- ay by))))

(defn triangle
  "Returns the vertices of an isosceles triangle whose altitude is equal to
   the length of the base."
  [ax ay bx by]
  (let [alt (.hypot js/Math (- bx ax) (- by ay))
        hyp (.hypot js/Math alt (/ alt 2))
        rad (.atan2 js/Math (- by ay) (- bx ax))]
    [ax
     ay
     (+ ax (* hyp (.cos js/Math (+ rad 0.46))))
     (+ ay (* hyp (.sin js/Math (+ rad 0.46))))
     (+ ax (* hyp (.cos js/Math (- rad 0.46))))
     (+ ay (* hyp (.sin js/Math (- rad 0.46))))]))

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
