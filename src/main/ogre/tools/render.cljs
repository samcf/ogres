(ns ogre.tools.render
  (:require [clojure.string :as string]
            [ogre.tools.state :refer [state]]
            [ogre.tools.storage :refer [storage]]
            [uix.core.alpha :as uix]))

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
        {:keys [data]} (uix/context state)
        {:keys [store]} (uix/context storage)]
    (when (and (string? checksum) (nil? @url))
      (-> (.get (.table store "images") checksum)
          (.then
           (fn [record]
             (when record
               (reset! url (.-data record)))))))
    @url))
