(ns ogres.app.serialize
  (:require [cognitect.transit :as t]
            [datascript.transit :as t-ds]
            [ogres.app.vec :as vec]))

(defn ^:private read-handler-vec [[x y]]
  (vec/Vec2. x y))

(defn ^:private read-handler-segment [[ax ay bx by]]
  (vec/Segment. (vec/Vec2. ax ay) (vec/Vec2. bx by)))

(defn ^:private write-handler-vec [^vec/Vec2 v]
  [(.-x v) (.-y v)])

(defn ^:private write-handler-segment [^vec/Segment s]
  [(.-x (.-a s)) (.-y (.-a s)) (.-x (.-b s)) (.-y (.-b s))])

(def ^:private read-handlers
  {"Vec2" (t/read-handler read-handler-vec)
   "Segment" (t/read-handler read-handler-segment)})

(def ^:private write-handlers
  {vec/Vec2 (t/write-handler (constantly "Vec2")    write-handler-vec)
   vec/Segment (t/write-handler (constantly "Segment") write-handler-segment)})

(def writer (t/writer :json {:handlers (merge t-ds/write-handlers write-handlers)}))
(def reader (t/reader :json {:handlers (merge t-ds/read-handlers read-handlers)}))
