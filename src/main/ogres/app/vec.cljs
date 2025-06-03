(ns ogres.app.vec
  (:refer-clojure :exclude [abs max mod map]))

(def ^:private rad->deg (/ 180 js/Math.PI))
(def ^:private tau (* 2 js/Math.PI))

(defn ^:private to-string [x y]
  (str "#vec2[" x "," y "]"))

(defprotocol IVec2
  (abs [a])
  (add [a b])
  (angle [a])
  (dist [a] [a b])
  (dist-cheb [a] [a b])
  (div [a x])
  (heading [a])
  (max [a])
  (mod [a x])
  (mul [a x])
  (normalize [a])
  (rnd [a] [a x] [a x f])
  (shift [a n] [a x y])
  (sub [a b])
  (transform [a m]))

(deftype Vec2 [x y]
  Object
  (toString [_]
    (str "translate(" x ", " y ")"))
  cljs.core/IPrintWithWriter
  (-pr-writer [_ writer _]
    (-write writer (to-string x y)))
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
  (abs [_]
    (Vec2. (clojure.core/abs x) (clojure.core/abs y)))
  (add [_ b]
    (Vec2. (+ x (.-x b)) (+ y (.-y b))))
  (angle [_]
    (let [rad (js/Math.atan2 y x)]
      (if (neg? rad)
        (* rad->deg (+ rad tau))
        (* rad->deg rad))))
  (dist [_]
    (js/Math.hypot x y))
  (dist [a b]
    (dist (sub b a)))
  (dist-cheb [a]
    (max (abs a)))
  (dist-cheb [a b]
    (max (abs (sub a b))))
  (div [_ n]
    (Vec2. (/ x n) (/ y n)))
  (heading [_]
    (js/Math.atan2 y x))
  (normalize [a]
    (let [m (dist a)]
      (if (zero? m) a
          (div a m))))
  (max [_]
    (clojure.core/max x y))
  (mod [_ n]
    (Vec2. (clojure.core/mod x n) (clojure.core/mod y n)))
  (mul [_ n]
    (Vec2. (* x n) (* y n)))
  (rnd [_]
    (Vec2. (js/Math.round x) (js/Math.round y)))
  (rnd [_ n]
    (Vec2. (* (js/Math.round (/ x n)) n) (* (js/Math.round (/ y n)) n)))
  (rnd [_ n f]
    (Vec2. (* (f (/ x n)) n) (* (f (/ y n)) n)))
  (shift [_ n]
    (Vec2. (+ x n) (+ y n)))
  (shift [_ n m]
    (Vec2. (+ x n) (+ y m)))
  (sub [_ b]
    (Vec2. (- x (.-x b)) (- y (.-y b))))
  (transform [_ ^Matrix m]
    (let [t (.matrixTransform (js/DOMPointReadOnly. x y) (.-m m))]
      (Vec2. (.-x t) (.-y t)))))

(def zero (Vec2. 0 0))
