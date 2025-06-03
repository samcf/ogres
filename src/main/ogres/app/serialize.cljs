(ns ogres.app.serialize
  (:require [cognitect.transit :as t]
            [datascript.transit :as t-ds]
            [ogres.app.segment :refer [Segment]]
            [ogres.app.vec :refer [Vec2]]))

(defn ^:private read-handler-vec [[x y]]
  (Vec2. x y))

(defn ^:private read-handler-segment [[ax ay bx by]]
  (Segment. (Vec2. ax ay) (Vec2. bx by)))

(defn ^:private write-handler-vec [^Vec2 v]
  [(.-x v) (.-y v)])

(defn ^:private write-handler-segment [^Segment s]
  [(.-x (.-a s)) (.-y (.-a s)) (.-x (.-b s)) (.-y (.-b s))])

(def ^:private read-handlers
  {"Vec2" (t/read-handler read-handler-vec)
   "Segment" (t/read-handler read-handler-segment)})

(def ^:private write-handlers
  {Vec2    (t/write-handler (constantly "Vec2")    write-handler-vec)
   Segment (t/write-handler (constantly "Segment") write-handler-segment)})

(defonce writer (t/writer :json {:handlers (merge t-ds/write-handlers write-handlers)}))
(defonce reader (t/reader :json {:handlers (merge t-ds/read-handlers read-handlers)}))
