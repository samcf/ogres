(ns ogres.app.matrix
  (:refer-clojure :exclude [identity])
  (:require [ogres.app.vec :as vec]))

(defn ^:private to-string [m]
  (str
   "#matrix["
   (.-a m) "," (.-b m) "," (.-c m) ","
   (.-d m) "," (.-e m) "," (.-f m) "]"))

(defprotocol IMatrix
  (inverse [m])
  (scale [m s])
  (translate [m v] [m x y]))

(deftype Matrix [m]
  Object
  (toString [_]
    (.toString m))
  cljs.core/IPrintWithWriter
  (-pr-writer [_ writer _]
    (-write writer (to-string m)))
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
    (vec/transform a m))
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

(def identity (Matrix. (js/DOMMatrixReadOnly.)))
