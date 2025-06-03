(ns ogres.app.segment
  (:require [ogres.app.vec :as vec]))

(defn ^:private to-string [a b]
  (str "#segment["
       "(" (or (.-x a) "nil") "," (or (.-y a) "nil") ") "
       "(" (or (.-x b) "nil") "," (or (.-y b) "nil") ")]"))

(defprotocol ISegment
  (width [s])
  (height [s])
  (midpoint [s])
  (rebase [s])
  (extend [s x]))

(deftype Segment [a b]
  Object
  (toString [_]
    (to-string a b))
  cljs.core/IPrintWithWriter
  (-pr-writer [_ writer _]
    (-write writer (to-string a b)))
  IEquiv
  (-equiv [_ s]
    (and (instance? Segment s) (= (.-a s) a) (= (.-b s) b)))
  IHash
  (-hash [_]
    (hash [a b]))
  ISeqable
  (-seq [_]
    (list a b))
  vec/IVec2
  (dist [_]
    (vec/dist a b))
  (dist-cheb [_]
    (vec/dist-cheb a b))
  (add [_ v]
    (Segment. (vec/add a v) (vec/add b v)))
  (rnd [_]
    (Segment. (vec/rnd a) (vec/rnd b)))
  (rnd [_ n]
    (Segment. (vec/rnd a n) (vec/rnd b n)))
  (rnd [_ n f]
    (Segment. (vec/rnd a n f) (vec/rnd b n f)))
  (transform [_ m]
    (Segment. (vec/transform a m) (vec/transform b m)))
  ISegment
  (extend [s x]
    (vec/add (.-b s) (vec/mul (vec/normalize (.-b (rebase s))) x)))
  (midpoint [_]
    (vec/div (vec/add a b) 2))
  (width [_]
    (clojure.core/abs (- (.-x b) (.-x a))))
  (height [_]
    (clojure.core/abs (- (.-y b) (.-y a))))
  (rebase [_]
    (Segment. vec/zero (vec/sub b a))))

(def zero (Segment. vec/zero vec/zero))

(defn DOMRect-> [rect]
  (Segment.
   (vec/Vec2. (.-left rect) (.-top rect))
   (vec/Vec2. (.-right rect) (.-bottom rect))))
