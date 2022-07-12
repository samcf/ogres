(ns ogre.tools.provider.image
  (:import goog.crypt.Md5)
  (:require [ogre.tools.provider.events :refer [use-publish subscribe!]]
            [ogre.tools.provider.state :refer [use-dispatch]]
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

(defmulti process-image :type)

(defmethod process-image :token
  [{:keys [image]}]
  (let [canvas (js/document.createElement "canvas")
        ctx    (.getContext canvas "2d")
        sw     (.-width image)
        sh     (.-height image)
        sl     (min sw sh)
        sx     (if (> sw sh) (- (/ sw 2) (/ sl 2)) 0)
        dw     256
        dh     256]
    (set! (.-width canvas) dw)
    (set! (.-height canvas) dh)
    (.drawImage ctx image sx 0 sl sl 0 0 dw dh)
    canvas))

(defmethod process-image :scene
  [{:keys [image]}]
  (let [canvas (js/document.createElement "canvas")
        ctx    (.getContext canvas "2d")
        sw     (.-width image)
        sh     (.-height image)]
    (set! (.-width canvas) sw)
    (set! (.-height canvas) sh)
    (.drawImage ctx image 0 0)
    canvas))

(defn create-image-element [url]
  (js/Promise.
   (fn [resolve]
     (let [image (js/Image.)]
       (.addEventListener image "load" (fn [] (this-as element (resolve element))))
       (set! (.-src image) url)))))

(defn use-image-uploader [{:keys [type]}]
  (let [dispatch (use-dispatch)
        store    (use-store)]
    (uix/callback
     (fn [file]
       (-> (create-data-url file)
           (.then create-image-element)
           (.then (fn [image]
                    (let [canvas   (process-image {:type type :image image})
                          data-url (.toDataURL canvas "image/jpeg" 0.70)
                          data     {:data-url data-url
                                    :checksum (create-checksum data-url)
                                    :width    (.-width canvas)
                                    :height   (.-height canvas)}]
                      (js/Promise.resolve data))))
           (.then (fn [{:keys [checksum data-url] :as data}]
                    (let [record #js {:checksum checksum :data data-url :created-at (js/Date.now)}]
                      (-> (.table store "images")
                          (.put record)
                          (.then (fn [] (js/Promise.resolve data)))))))
           (.then (fn [{:keys [checksum width height] :as data}]
                    (case type
                      :token (dispatch :stamp/create checksum width height)
                      :scene (dispatch :scene/create checksum width height)
                      :else  nil)
                    (js/Promise.resolve data)))))
     [type])))

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
