(ns ogre.tools.util)

(defn checked? [pred coll]
  (let [pass (filter pred coll)]
    (cond
      (= (count coll) (count pass)) true
      (= (count pass) 0) false
      :else :indeterminate)))

(defn every-value? [coll f]
  (let [sample (f (first coll))]
    (if (every? #(= (f %) sample) coll)
      [true sample]
      [false nil])))
