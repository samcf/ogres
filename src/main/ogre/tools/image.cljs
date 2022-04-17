(ns ogre.tools.image
  (:import goog.crypt.Md5))

(defn checksum [bytes]
  (let [hash (Md5.)]
    (.update hash bytes)
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
            (let [url (.. event -target -result) image (js/Image.)]
              (.addEventListener image "load" (fn [] (this-as element (resolve [file url element]))))
              (set! (.-src image) url))))))))
