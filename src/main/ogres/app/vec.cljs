(ns ogres.app.vec
  (:refer-clojure :exclude [abs max]))

(deftype ^:export Vec2 [x y])
(deftype ^:export Segment [a b])

(defn ^:private rounded
  ([x]     (js/Math.round x))
  ([x n]   (* (js/Math.round (/ x n)) n))
  ([x n f] (* (f (/ x n)) n)))

(defn abs [a]
  (Vec2. (clojure.core/abs (.-x a)) (clojure.core/abs (.-y a))))

(defn add [a b]
  (Vec2. (+ (.-x a) (.-x b)) (+ (.-y a) (.-y b))))

(defn sub [a b]
  (Vec2. (- (.-x a) (.-x b)) (- (.-y a) (.-y b))))

(defn mul [a x]
  (Vec2. (* (.-x a) x) (* (.-y a) x)))

(defn div [a x]
  (Vec2. (/ (.-x a) x) (/ (.-y a) x)))

(defn mag [a]
  (js/Math.hypot (.-x a) (.-y a)))

(defn max [a]
  (clojure.core/max (.-x a) (.-y a)))

(defn round
  ([a]     (round a 1 js/Math.round))
  ([a x]   (round a x js/Math.round))
  ([a x f] (Vec2. (rounded (.-x a) x f) (rounded (.-y a) x f))))

(defn dist
  ([s]   (dist (.-a s) (.-b s)))
  ([a b] (mag (sub a b))))

(defn dist-cheb
  ([s]   (dist-cheb (.-a s) (.-b s)))
  ([a b] (max (abs (sub a b)))))

(defn to-seq
  ([s]   (to-seq (.-a s) (.-b s)))
  ([a b] (list (.-x a) (.-y a) (.-x b) (.-y b))))

(defn to-string [a]
  (str "x:" (.-x a) ",y:" (.-y a)))

(defn to-translate [a]
  (str "translate(" (.-x a) "," (.-y a) ")"))
