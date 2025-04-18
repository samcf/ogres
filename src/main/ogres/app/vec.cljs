(ns ogres.app.vec
  (:refer-clojure :exclude [abs max mod]))

(defn ^:private to-string-vec2 [x y]
  (str "#vec2[" x "," y "]"))

(defn ^:private to-string-segment [a b]
  (str "#segment["
       (.-x a) "," (.-y a) " "
       (.-x b) "," (.-y b) "]"))

(deftype Vec2 [x y]
  Object
  (toString [_]
    (to-string-vec2 x y))
  cljs.core/IPrintWithWriter
  (-pr-writer [_ writer _]
    (-write writer (to-string-vec2 x y)))
  IEquiv
  (-equiv [_ v]
    (and (instance? Vec2 v) (= (.-x v) x) (= (.-y v) y)))
  IHash
  (-hash [_]
    (hash [x y]))
  ISeqable
  (-seq [_]
    (list x y)))

(deftype Segment [a b]
  Object
  (toString [_]
    (to-string-segment a b))
  cljs.core/IPrintWithWriter
  (-pr-writer [_ writer _]
    (-write writer (to-string-segment a b)))
  IEquiv
  (-equiv [_ s]
    (and (instance? Segment s) (= (.-a s) a) (= (.-b s) b)))
  IHash
  (-hash [_]
    (hash [a b]))
  ISeqable
  (-seq [_]
    (list (.-x a) (.-y a) (.-x b) (.-y b))))

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

(defn to-translate [a]
  (str "translate(" (.-x a) "," (.-y a) ")"))
