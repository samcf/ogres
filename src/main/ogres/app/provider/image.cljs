(ns ogres.app.provider.image
  (:import goog.crypt.Md5)
  (:require [ogres.app.provider.events :refer [use-publish use-subscribe]]
            [ogres.app.provider.storage :refer [use-store]]
            [uix.core :refer [use-callback use-state use-effect]]))

(def ^:private cache (atom {}))

(defn ^:private create-data-url [file]
  (-> (js/Promise.
       (fn [resolve]
         (let [reader (js/FileReader.)]
           (.readAsDataURL reader file)
           (.addEventListener reader "load" resolve))))
      (.then (fn [event] (js/Promise.resolve (.. event -target -result))))))

(defn ^:private create-object-url [data-url]
  (-> (.fetch js/window data-url)
      (.then (fn [r] (.blob r)))
      (.then (fn [b] (->> (js/File. #js [b] "image" #js {:type (.-type b)})
                          (js/URL.createObjectURL))))))

(defmulti ^:private process-image :type)

(defmethod process-image :token
  [{:keys [image]}]
  (let [sw  (.-width image)
        sh  (.-height image)
        len (min sw sh)
        sx  (if (> sw sh) (- (/ sw 2) (/ len 2)) 0)

        ;; Sampling canvas which will be an intermediate canvas that will
        ;; be used to iteratively downsample the source image.
        src (js/document.createElement "canvas")
        srx (.getContext src "2d")

        ;; Destination canvas that will contain the resulting downsampled
        ;; image.
        dst (js/document.createElement "canvas")
        dsx (.getContext dst "2d")
        dsl 256

        ;; Number of iterations to downsample the source image.
        steps (js/Math.max 0 (js/Math.floor (/ (js/Math.log (/ len dsl)) (js/Math.log 2))))]

    ;; Initialize the destination canvas with the fixed dimensions that will
    ;; be used for the resulting image.
    (set! (.-width dst) dsl)
    (set! (.-height dst) dsl)

    ;; Initialize the sampling canvas with the dimensions of the source
    ;; image which has been cropped to a square with the length of its
    ;; smallest dimension.
    (set! (.-width src) len)
    (set! (.-height src) len)
    (.drawImage srx image sx 0 len len 0 0 len len)

    ;; Iteratively downsample the sample canvas by halves to minimize
    ;; noise, then draw the sample canvas onto the destination canvas
    ;; and return it.
    (loop [length len iter steps]
      (if (> iter 0)
        (let [halved (/ length 2)]
          (.drawImage srx src 0 0 length length 0 0 halved halved)
          (recur halved (dec iter))) length))

    ;; Finally draw the sampling canvas onto the destination canvas with
    ;; the appropriate dimensions.
    (let [length (/ len (js/Math.pow 2 steps))]
      (.drawImage dsx src 0 0 length length 0 0 dsl dsl)
      dst)))

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

(defn create-checksum [bytes]
  (let [hash (Md5.)]
    (.update hash bytes)
    (reduce
     (fn [s b]
       (str s (.slice (str "0" (.toString b 16)) -2))) "" (.digest hash))))

(defn use-image-uploader [{:keys [type]}]
  (let [publish (use-publish)
        store   (use-store)]
    (use-callback
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
           (.then (fn [data]
                    (publish {:topic :image/create :args [type data]})))))
     [publish store type])))

(defn use-image [checksum]
  (let [publish          (use-publish)
        store            (use-store)
        [_ set-sentinel] (use-state 0)
        [watch-key]      (use-state (random-uuid))
        [loading cached] (get @cache checksum [false nil])]

    (if (not (or loading cached))
      (swap! cache assoc checksum [true nil]))

    (use-effect
     (fn []
       (if (string? checksum)
         (add-watch
          cache watch-key
          (fn [_ _ _ value]
            (if (not cached)
              (let [[_ cached] (get value checksum [false nil])]
                (if cached (set-sentinel inc)))))))
       (fn [] (remove-watch cache watch-key))) [watch-key checksum cached])

    (use-subscribe :image/cache
      (use-callback
       (fn [{[checksum data-url] :args}]
         (-> (create-object-url data-url)
             (.then #(swap! cache assoc checksum [false %1])))) []))

    (use-effect
     (fn []
       (if (and (string? checksum) (not (or loading cached)))
         (-> (.table store "images")
             (.get checksum)
             (.then (fn [rec] (create-object-url (.-data rec))))
             (.then (fn [url] (swap! cache assoc checksum [false url])))
             (.catch (fn [] (publish {:topic :image/request :args [checksum]})))))) ^:lint/disable [checksum])

    cached))
