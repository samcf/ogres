(ns ogres.app.util)

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

(defn round
  "Round x to the nearest integer."
  ([x]     (js/Math.round x))
  ([x n]   (* (js/Math.round (/ x n)) n))
  ([f x n] (* (f (/ x n)) n)))

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
