(ns ogres.app.svg
  (:require [clojure.string :refer [join]]))

(def ^:private poly-path-xf
  (comp (partition-all 2) (mapcat (fn [[x y]] [x y \L]))))

(defn ^:private circle-path
  [[x y r]]
  (let [d (* r 2)]
    [\M x y
     \m r 0
     \a r r 0 1 0 (- d) 0
     \a r r 0 1 0 d 0 \z]))

(defn poly->path
  "Reducing function which returns an SVG path string from a collection
   of polygons given as [Ax Ay Bx By Cx Cy ...]."
  ([] [])
  ([path]
   (join " " path))
  ([path points]
   (into path (conj (pop (into [\M] poly-path-xf points)) \z))))

(defn circle->path
  "Reducing function which returns an SVG path string from a collection
   of circles given as [Cx Cy Radius]."
  ([] [])
  ([path]
   (join " " path))
  ([path circle]
   (into path (circle-path circle))))
