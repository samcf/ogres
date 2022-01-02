(ns ogre.tools.vec
  (:refer-clojure :exclude [+ -] :rename {+ +' - -'}))

(defn + "Returns a vector with matching components summed."
  [& xs] (apply mapv +' xs))

(defn - "Returns a vector with matching components differenced."
  [& xs] (apply mapv -' xs))

(defn s "Returns the given vector scaled by n."
  [n v] (mapv (fn [x] (* x n)) v))

(defn r "Returns the given vector whose components are rounded to the nearest n."
  [n v] (mapv (fn [x] (* n (js/Math.round (/ x n)))) v))
