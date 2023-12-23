(ns ogres.app.provider.image
  (:import goog.crypt.Md5)
  (:require [goog.object :as go]
            [ogres.app.provider.events :refer [use-publish use-subscribe]]
            [ogres.app.provider.storage :refer [use-store]]
            [uix.core :refer [defui $ create-context use-callback use-state use-context use-effect]]))

(def ^:private image
  (create-context {{} (constantly nil)}))

(defn ^:private create-data-url [file]
  (-> (js/Promise.
       (fn [resolve]
         (let [reader (js/FileReader.)]
           (.readAsDataURL reader file)
           (.addEventListener reader "load" resolve))))
      (.then (fn [event] (js/Promise.resolve (.. event -target -result))))))

(defn ^:private create-object-url [data-url]
  (let [mime (-> (.split data-url ",") (aget 0) (.split ":") (aget 1) (.split ";") (aget 0))
        rest (-> (.split data-url ",") (aget 1) (js/atob))]
    (loop [i 0 data (js/Uint8Array. (count rest))]
      (if (< i (count rest))
        (do (aset data i (.charCodeAt rest i))
            (recur (inc i) data))
        (-> (js/Blob. #js [data] #js {"type" mime})
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

(defui provider [props]
  (let [publish       (use-publish)
        store         (use-store)
        loading       (atom {})
        [urls update] (use-state {})
        on-request    (use-callback
                       (fn [checksum]
                         (let [url (get urls checksum)]
                           (if (not url)
                             (when (not (get @loading checksum))
                               (swap! loading assoc checksum true)
                               (-> (.table store "images")
                                   (.get checksum)
                                   (.then (fn [rec] (create-object-url (go/get rec "data-url"))))
                                   (.then (fn [url] (update (fn [urls] (assoc urls checksum url)))))
                                   (.catch (fn [] (publish {:topic :image/request :args [checksum]})))))
                             url))) ^:lint/disable [])]
    (use-subscribe :image/cache
      (use-callback
       (fn [{[checksum data-url] :args}]
         (let [url (create-object-url data-url)]
           (update (fn [urls] (assoc urls checksum url)))))
       []))
    ($ (.-Provider image) {:value [urls on-request]}
      (:children props))))

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
                          data     {:name     (.-name file)
                                    :size     (count data-url)
                                    :data-url data-url
                                    :checksum (create-checksum data-url)
                                    :width    (.-width canvas)
                                    :height   (.-height canvas)}]
                      (js/Promise.resolve data))))
           (.then (fn [data]
                    (let [keys [:name :size :data-url :checksum]
                          record (select-keys data keys)
                          record (assoc record :created-at (js/Date.now))
                          record (clj->js record)]
                      (-> (.table store "images")
                          (.put record)
                          (.then (fn [] (js/Promise.resolve data)))))))
           (.then (fn [data] (publish {:topic :image/create :args [type data]})))))
     [publish store type])))

(defn use-image [checksum]
  (let [[urls on-request] (use-context image)]
    (use-effect
     (fn [] (on-request checksum)) [on-request checksum])
    (get urls checksum)))
