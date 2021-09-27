(ns ogre.tools.image
  (:import goog.crypt.Md5))

(defn checksum [data]
  (let [hash (new Md5)]
    (.update hash data)
    (reduce
     (fn [s b]
       (str s (.slice (str "0" (.toString b 16)) -2))) "" (.digest hash))))

(defn load [file handler]
  (let [reader (new js/FileReader)]
    (.readAsDataURL reader file)
    (.addEventListener
     reader "load"
     (fn [event]
       (let [data  (.. event -target -result)
             image (new js/Image)]
         (.addEventListener
          image "load"
          (fn []
            (this-as img (handler {:data data :filename (.-name file) :img img}))))
         (set! (.-src image) data))))))
