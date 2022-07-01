(ns ogre.tools.provider.image
  (:import goog.crypt.Md5)
  (:require [ogre.tools.provider.events :refer [use-publish subscribe!]]
            [ogre.tools.provider.storage :refer [use-store]]
            [uix.core.alpha :as uix]))

(def cache (atom {}))

(defn create-checksum [bytes]
  (let [hash (Md5.)]
    (.update hash bytes)
    (reduce
     (fn [s b]
       (str s (.slice (str "0" (.toString b 16)) -2))) "" (.digest hash))))

(defn create-data-url [file]
  (-> (js/Promise.
       (fn [resolve]
         (let [reader (js/FileReader.)]
           (.readAsDataURL reader file)
           (.addEventListener reader "load" resolve))))
      (.then (fn [event] (js/Promise.resolve (.. event -target -result))))))

(defn create-object-url [data-url]
  (-> (.fetch js/window data-url)
      (.then (fn [r] (.blob r)))
      (.then (fn [b] (->> (js/File. #js [b] "image" #js {:type (.-type b)})
                          (js/URL.createObjectURL))))))

(defn create-image-element [url]
  (js/Promise.
   (fn [resolve]
     (let [image (js/Image.)]
       (.addEventListener image "load" (fn [] (this-as element (resolve element))))
       (set! (.-src image) url)))))

(defn use-image-uploader []
  (let [publish (use-publish)]
    (fn [file]
      (-> (js/Promise.resolve {})
          (.then (fn [data]
                   (.then (create-data-url file)
                          (fn [data-url]
                            (js/Promise.resolve (assoc data :data-url data-url))))))
          (.then (fn [data]
                   (js/Promise.resolve
                    (assoc data :checksum (create-checksum (:data-url data))))))
          (.then (fn [data]
                   (let [{:keys [checksum data-url]} data]
                     (publish {:topic :storage/cache-image :args [checksum data-url]})
                     (js/Promise.resolve data))))
          (.then (fn [data]
                   (.then (create-image-element (:data-url data))
                          (fn [element]
                            (js/Promise.resolve (assoc data :width (.-width element) :height (.-height element)))))))))))

(defn use-image [checksum]
  (let [publish          (use-publish)
        store            (use-store)
        sentinel         (uix/state 0)
        watch-key        (deref (uix/state (random-uuid)))
        [loading cached] (get @cache checksum [false nil])]

    (if (not (or loading cached))
      (swap! cache assoc checksum [true nil]))

    (uix/effect!
     (fn []
       (if (string? checksum)
         (add-watch
          cache watch-key
          (fn [_ _ _ value]
            (if (not cached)
              (let [[_ cached] (get value checksum [false nil])]
                (if cached (swap! sentinel inc)))))))
       (fn [] (remove-watch cache watch-key))) [checksum cached])

    (subscribe!
     (fn [{[data-url] :args}]
       (let [checksum (create-checksum data-url)]
         (publish {:topic :storage/cache-image :args [checksum data-url]})
         (-> (create-object-url data-url)
             (.then #(swap! cache assoc checksum [false %1]))))) :image/cache [])

    (uix/effect!
     (fn []
       (if (and (string? checksum) (not (or loading cached)))
         (-> (.table store "images")
             (.get checksum)
             (.then (fn [rec] (create-object-url (.-data rec))))
             (.then (fn [url] (swap! cache assoc checksum [false url])))
             (.catch (fn [] (publish {:topic :image/request :args [checksum]}))))))
     [checksum])

    cached))
