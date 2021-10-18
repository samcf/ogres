(ns ogre.tools.image
  (:import goog.crypt.Md5))

(defn checksum [data]
  (let [hash (Md5.)]
    (.update hash data)
    (reduce
     (fn [s b]
       (str s (.slice (str "0" (.toString b 16)) -2))) "" (.digest hash))))

(defn load [file]
  (-> (js/Promise.
       (fn [resolve]
         (let [reader (js/FileReader.)]
           (.readAsDataURL reader file)
           (.addEventListener reader "load" resolve))))
      (.then
       (fn [event]
         (js/Promise.
          (fn [resolve]
            (let [data (.. event -target -result) image (js/Image.)]
              (.addEventListener image "load" (fn [] (this-as element (resolve [data element]))))
              (set! (.-src image) data))))))
      (.then
       (fn [[data element]]
         (.resolve js/Promise {:data data :filename (.-name file) :element element})))))
