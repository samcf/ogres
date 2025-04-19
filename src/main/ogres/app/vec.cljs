(ns ogres.app.vec
  (:refer-clojure :exclude [abs max mod]))

(defn ^:private to-string-vec2 [x y]
  (str "#vec2[" x "," y "]"))

(defn ^:private to-string-segment [a b]
  (str "#segment["
       (.-x a) "," (.-y a) " "
       (.-x b) "," (.-y b) "]"))

(defprotocol IVec2
  (abs [a])
  (add [a b])
  (dist [a] [a b])
  (dist-cheb [a] [a b])
  (div [a x])
  (mag [a])
  (max [a])
  (mod [a x])
  (mul [a x])
  (round [a] [a x] [a x f])
  (sub [a b])
  (to-translate [a]))

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
    (list x y))
  IVec2
  (abs [_] (Vec2. (clojure.core/abs x) (clojure.core/abs y)))
  (add [_ b] (Vec2. (+ x (.-x b)) (+ y (.-y b))))
  (dist [_] (js/Math.hypot x y))
  (dist [a b] (dist (sub b a)))
  (dist-cheb [a] (max (abs a)))
  (dist-cheb [a b] (max (abs (sub a b))))
  (div [_ n] (Vec2. (/ x n) (/ y n)))
  (mag [_] (js/math.hypot x y))
  (max [_] (clojure.core/max x y))
  (mod [_ n] (Vec2. (clojure.core/mod x n) (clojure.core/mod y n)))
  (mul [_ n] (Vec2. (* x n) (* y n)))
  (round [_] (Vec2. (js/Math.round x) (js/Math.round y)))
  (round [_ n] (Vec2. (* (js/Math.round (/ x n)) n) (* (js/Math.round (/ y n)) n)))
  (round [_ n f] (Vec2. (* (f (/ x n)) n) (* (f (/ y n)) n)))
  (sub [_ b] (Vec2. (- x (.-x b)) (- y (.-y b))))
  (to-translate [_] (str "translate(" x "," y ")")))

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
    (list (.-x a) (.-y a) (.-x b) (.-y b)))
  IVec2
  (dist [_] (dist a b))
  (dist-cheb [_] (dist-cheb a b)))

(def zero (Vec2. 0 0))
