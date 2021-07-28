(ns ogre.tools.utils)

(defn css
  [& class-names]
  (->> (reduce (fn [names value]
                 (cond
                   (string?  value) (conj names (clojure.string/trim value))
                   (keyword? value) (conj names value)
                   (number?  value) (conj names (str value))
                   (vector?  value) (vec (concat names value))
                   (map?     value) (->> (reduce
                                          (fn [names [k v]]
                                            (if v (conj names k) names)) [] value)
                                         (concat names)
                                         vec)
                   :else            names)) [] class-names)
       (mapv name)
       (clojure.string/join " ")))
