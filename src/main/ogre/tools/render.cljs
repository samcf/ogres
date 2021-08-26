(ns ogre.tools.render
  (:require [clojure.string :as string]
            [uix.core.alpha :as uix :refer [defcontext]]
            [datascript.core :as ds]))

(defcontext context)

(defn css [& class-names]
  (->> (reduce (fn [names value]
                 (cond
                   (string?  value) (conj names (string/trim value))
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
       (string/join " ")))

(defn use-image [checksum]
  (let [url (uix/state nil)
        {:keys [data store]} (uix/context context)]
    (when (and (string? checksum) (nil? @url))
      (-> (.get (.table store "images") checksum)
          (.then
           (fn [record]
             (when record
               (reset! url (.-data record)))))))
    @url))
