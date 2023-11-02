(ns ogres.app.util
  (:require [goog.async.Debouncer]))

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

(defn with-ns
  "Updates the keys of the given map, qualifying them with the given string
   `s` if they are not qualified already."
  [m s]
  (update-keys
   m
   (fn [k]
     (if (and (keyword? k) (not (qualified-keyword? k)))
       (keyword (str s) (name k))
       k))))
