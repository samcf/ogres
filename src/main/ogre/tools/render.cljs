(ns ogre.tools.render
  (:require [clojure.string :as string]
            [datascript.core :as ds]
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

(defn button [props children]
  [:button (merge {:class "ogre-button" :type "button"} props) children])

(defn use-image
  ([checksum]
   (use-image checksum {:persist? false}))
  ([checksum {:keys [persist?] :or {persist? false}}]
   (let [result (uix/state {:loading? false :url nil})
         {:keys [data dispatch]} (uix/context state)
         {:keys [store]} (uix/context storage)]
     (when (string? checksum)
       (let [{:keys [image/url]} (ds/entity data [:image/checksum checksum])]
         (cond
           (string? url)
           url

           (:loading? @result)
           nil

           (string? (:url @result))
           (:url @result)

           :else
           (do (swap! result assoc :loading? true)
               (-> (.get (.table store "images") checksum)
                   (.then
                    (fn [record]
                      (when record
                        (if (not persist?)
                          (reset! result {:loading? false :url (.-data record)})
                          (-> (.fetch js/window (.-data record))
                              (.then (fn [r] (.blob r)))
                              (.then (fn [b]
                                       (let [file (js/File. #js [b] "image" #js {:type (.-type b)})
                                             url  (.createObjectURL js/URL file)]
                                         (dispatch :image/set-url checksum url)
                                         (reset! result {:loading? false :url url})))))))))))))))))
