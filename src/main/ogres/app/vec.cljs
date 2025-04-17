(ns ogres.app.vec
  (:refer-clojure :exclude [abs max mod]))

(defn ^:private to-string-vec2 [this]
  (str "#vec2[" (.-x this) "," (.-y this) "]"))

(defn ^:private to-string-segment [this]
  (str "#segment["
       (.-x (.-a this)) ","
       (.-y (.-a this)) " "
       (.-x (.-b this)) ","
       (.-y (.-b this))
       "]"))

(deftype ^:export Vec2 [x y]
  Object
  (toString [this]
    (to-string-vec2 this))
  cljs.core/IPrintWithWriter
  (-pr-writer [this writer _]
    (-write writer (to-string-vec2 this))))

(deftype ^:export Segment [a b]
  Object
  (toString [this]
    (to-string-segment this))
  cljs.core/IPrintWithWriter
  (-pr-writer [this writer _]
    (-write writer (to-string-segment this))))

(defn ^:private rounded
  ([x]     (js/Math.round x))
  ([x n]   (* (js/Math.round (/ x n)) n))
  ([x n f] (* (f (/ x n)) n)))

(def zero (Vec2. 0 0))

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

(defn mod [a x]
  (Vec2. (clojure.core/mod (.-x a) x) (clojure.core/mod (.-y a) x)))

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
