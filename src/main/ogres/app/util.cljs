(ns ogres.app.util
  (:require [goog.async.Debouncer]
            [ogres.app.const :refer [grid-size]]))

(defn debounce
  "Returns a debounced version of the given function f. Calls to this function
   are delayed for interval (ms), during which time repeated calls reset the
   timer. Only one call per interval will ever be executed."
  [f interval]
  (let [d (goog.async.Debouncer. f interval)]
    (fn [& args]
      (.apply (.-fire d) d (to-array args)))))

(defn key-by
  "Returns a map of the given `coll` whose keys are the result of calling `f`
   with each element in the collection and whose values are the element
   itself."
  [f coll]
  (into {} (map (juxt f identity) coll)))

(defn separate
  "Split coll into two sequences, one that matches pred and one that doesn't."
  [pred coll]
  (let [pcoll (map (juxt identity pred) coll)]
    (vec (for [f [filter remove]]
           (map first (f second pcoll))))))

(defn comp-fn
  "Returns a function which applies f with the result of calling key-fn on the
   given value and xs. Useful for passing to higher order functions like
   filter and map."
  [f key-fn & xs]
  (fn [x]
    (apply f (key-fn x) xs)))

(defn round
  "Round the scalar `x` to nearest `n` (default 1)."
  ([x]   (round x 1))
  ([x n] (* (js/Math.round (/ x n)) n)))

(defn round-grid
  "Returns the given scalar `x` rounded to the nearest `grid-size`, accounting
   for the radius `r` of the object and scalar offset `o`."
  [x r o]
  (+ (round (- x r o) grid-size) r o))

(defn display-size
  "Returns the given size in bytes as a string using the most appropriate
   unit affix.
   ```
   (display-size 12345)    ;; => \"12.06KB\"
   (display-size 12345678) ;; => \"11.77MB\"
   ```"
  [bytes]
  (let [idx (js/Math.floor (/ (js/Math.log bytes) (js/Math.log 1024)))
        aff ["B" "KB" "MB" "GB" "TB" "PB" "EB" "ZB" "YB"]]
    (str (.toFixed (/ bytes (js/Math.pow 1024, idx)) 2)
         (aff idx))))
