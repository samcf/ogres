(ns ogres.app.vec
  (:refer-clojure :exclude [abs max mod map identity]))

(declare zero)
(declare identity)

(defn ^:private to-string-vec2 [x y]
  (str "#vec2[" x "," y "]"))

(defn ^:private to-string-segment [a b]
  (str "#segment["
       "(" (or (.-x a) "nil") "," (or (.-y a) "nil") ") "
       "(" (or (.-x b) "nil") "," (or (.-y b) "nil") ")]"))

(defn ^:private to-string-matrix [m]
  (str "#matrix[" (.-a m) "," (.-b m) "," (.-c m) "," (.-d m) "," (.-e m) "," (.-f m) "]"))

(defprotocol IVec2
  (abs [a])
  (add [a b])
  (dist [a] [a b])
  (dist-cheb [a] [a b])
  (div [a x])
  (heading [a])
  (max [a])
  (mod [a x])
  (mul [a x])
  (rnd [a] [a x] [a x f])
  (shift [a n] [a x y])
  (sub [a b])
  (transform [a m]))

(defprotocol ISegment
  (width [s])
  (height [s])
  (midpoint [s])
  (rebase [s]))

(defprotocol IMatrix
  (inverse [m])
  (scale [m s])
  (translate [m v] [m x y]))

(deftype Vec2 [x y]
  Object
  (toString [_]
    (str "translate(" x ", " y ")"))
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
  (abs [_]
    (Vec2. (clojure.core/abs x) (clojure.core/abs y)))
  (add [_ b]
    (Vec2. (+ x (.-x b)) (+ y (.-y b))))
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
    (list a b))
  IVec2
  (dist [_]
    (dist a b))
  (dist-cheb [_]
    (dist-cheb a b))
  (add [_ v]
    (Segment. (add a v) (add b v)))
  (rnd [_]
    (Segment. (rnd a) (rnd b)))
  (rnd [_ n]
    (Segment. (rnd a n) (rnd b n)))
  (rnd [_ n f]
    (Segment. (rnd a n f) (rnd b n f)))
  (transform [_ m]
    (Segment. (transform a m) (transform b m)))
  ISegment
  (midpoint [_]
    (div (add a b) 2))
  (width [_]
    (clojure.core/abs (- (.-x b) (.-x a))))
  (height [_]
    (clojure.core/abs (- (.-y b) (.-y a))))
  (rebase [_]
    (Segment. zero (sub b a))))

(deftype Matrix [m]
  Object
  (toString [_]
    (.toString m))
  cljs.core/IPrintWithWriter
  (-pr-writer [_ writer _]
    (-write writer (to-string-matrix m)))
  IEquiv
  (-equiv [_ ^Matrix b]
    (and (instance? Matrix b)
         (= (.-a m) (.-a (.-m b)))
         (= (.-b m) (.-b (.-m b)))
         (= (.-c m) (.-c (.-m b)))
         (= (.-d m) (.-d (.-m b)))
         (= (.-e m) (.-e (.-m b)))
         (= (.-f m) (.-f (.-m b)))))
  IHash
  (-hash [_]
    (hash [(.-a m) (.-b m) (.-c m) (.-d m) (.-e m) (.-f m)]))
  IFn
  (-invoke [m a]
    (transform a m))
  ISeqable
  (-seq [_]
    (list (.-a m) (.-b m) (.-c m) (.-d m) (.-e m) (.-f m)))
  IMatrix
  (inverse [_]
    (Matrix. (.inverse m)))
  (scale [_ s]
    (Matrix. (.scale m s)))
  (translate [_ v]
    (Matrix. (.translate m (.-x v) (.-y v))))
  (translate [_ x y]
    (Matrix. (.translate m x y))))

(def zero (Vec2. 0 0))
(def zero-segment (Segment. zero zero))
(def identity (Matrix. (js/DOMMatrixReadOnly.)))

(defn DOMRect->Segment [rect]
  (Segment.
   (Vec2. (.-left rect) (.-top rect))
   (Vec2. (.-right rect) (.-bottom rect))))
