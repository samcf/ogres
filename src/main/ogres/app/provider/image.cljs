(ns ogres.app.provider.image
  (:require [ogres.app.provider.dispatch :refer [use-dispatch]]
            [ogres.app.provider.events :refer [use-publish use-subscribe]]
            [ogres.app.provider.state :refer [use-query]]
            [ogres.app.provider.storage :refer [use-store]]
            [uix.core :refer [defui $ create-context use-callback use-state use-context use-effect]]))

(def ^:private image
  (create-context {{} (constantly nil)}))

(defn ^:private decode-hash
  [hash]
  (.. (js/Array.from hash)
      (map (fn [s] (.. s (toString 16) (padStart 2 "0"))))
      (join "")))

(defn ^:private create-thumbnail
  "Returns a <canvas> element with the contents of the src <context> cropped
   and resized to the given dimensions. Accepts two points given as {Ax Ay}
   and {Bx By} which define a square region of the source image to use as
   the crop. When no region is given, the crop is centered horizontally and
   to the top."
  ([src len]
   (let [cx (.-width src) cy (.-height src)]
     (cond (or (= cx cy) (and (<= cx len) (<= cy len)))
           (create-thumbnail src len 0 0 cx cy)
           (> cx cy)
           (create-thumbnail src len (/ (- cx cy) 2) 0 (/ (+ cx cy) 2) cy)
           (> cy cx)
           (create-thumbnail src len 0 0 cx cx))))
  ([src len ax ay bx by]
   (let [dst (js/document.createElement "canvas")
         dsx (.getContext dst "2d")]
     (set! (.-width dst) len)
     (set! (.-height dst) len)
     (set! (.-fillStyle dsx) "rgb(24, 33, 37)")
     (.fillRect dsx 0 0 len len)
     (.drawImage dsx src ax ay (- bx ax) (- by ay) 0 0 len len)
     dst)))

(defn ^:private create-canvas
  "Returns a <canvas> with the contents of the given image drawn on it."
  [image]
  (let [cnv (js/document.createElement "canvas")
        ctx (.getContext cnv "2d")
        wdt (.-width image)
        hgt (.-height image)]
    (set! (.-width cnv) wdt)
    (set! (.-height cnv) hgt)
    (set! (.-fillStyle ctx) "rgb(24, 33, 37)")
    (.fillRect ctx 0 0 wdt hgt)
    (.drawImage ctx image 0 0 wdt hgt)
    cnv))

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
                                   (.then (fn [rec] (js/URL.createObjectURL (.-data rec))))
                                   (.then (fn [url] (update (fn [urls] (assoc urls checksum url)))))
                                   (.catch (fn [] (publish {:topic :image/request :args [checksum]})))))
                             url))) ^:lint/disable [])]
    ($ image {:value [urls on-request]}
      (:children props))))

(defn ^:private extract-image
  "Returns the image and metadata associated with the <canvas> element passed
   given an image type and quality ratio in the form of a map whose keys are
   'data', 'hash', 'width', and 'height'."
  ([canvas]
   (extract-image canvas "image/jpeg" 0.80))
  ([canvas type quality]
   (js/Promise.
    (fn [resolve]
      (.toBlob
       canvas
       (fn [blob]
         (.then
          (.arrayBuffer blob)
          (fn [data]
            (.then
             (js/crypto.subtle.digest "SHA-1" data)
             (fn [hash]
               (resolve
                {:data   blob
                 :hash   (decode-hash (js/Uint8Array. hash))
                 :width  (.-width canvas)
                 :height (.-height canvas)})))))) type quality)))))

(defn ^:private process-file
  "Returns a Promise.all which resolves with three elements:
     1. The filename
     2. The image extracted from the file
     3. The image, cropped and resized to be used as a thumbnail"
  [file]
  (.then (js/createImageBitmap file)
         (fn [image]
           (let [canvas (create-canvas image)]
             (js/Promise.all
              #js [(js/Promise.resolve (.-name file))
                   (extract-image canvas)
                   (extract-image (create-thumbnail canvas 256))])))))

(defn ^:private create-store-records
  "Returns a two-element vector containing the Javascript objects which
   will be persisted to IndexedDB. Each object contains the image data
   represented as a Blob and a digest of the image represented as a
   SHA-1 string."
  [[_ image thumb]]
  [#js {"checksum" (:hash image) "data" (:data image)}
   #js {"checksum" (:hash thumb) "data" (:data thumb)}])

(defn ^:private create-state-record
  "Returns a description of an image to be persisted to DataScript state,
   containing the digest, filename, size in kb, width, and height."
  [filename image]
  {:checksum (:hash image)
   :name     filename
   :size     (.-size (:data image))
   :width    (:width image)
   :height   (:height image)})

(defn ^:private create-state-records
  "Returns a two-element vector containing descriptions of images to be
   persisted to DataScript state. The first is the original image and
   the second is the thumbnail."
  [[filename image thumb]]
  [(create-state-record filename image)
   (create-state-record filename thumb)])

(defn use-image-uploader
  "React hook which provides a function that accepts one or more uploaded
   File objects, processes those files to be used as images, and persists
   them to storage and state."
  [{:keys [type]}]
  (let [dispatch (use-dispatch)
        result   (use-query [:user/type] [:db/ident :user])
        store    (use-store)]
    (case (:user/type result)
      :host
      (use-callback
       (fn [files]
         (-> (js/Promise.all (apply array (mapv process-file files)))
             (.then
              (fn [files]
                (let [records (apply array (mapcat create-store-records files))]
                  (.then (.bulkPut (.table store "images") records)
                         (constantly files)))))
             (.then
              (fn [files]
                (let [records (sequence (map create-state-records) files)]
                  (case type
                    :token (dispatch :token-images/create-many records :private)
                    :scene (dispatch :scene-images/create-many records :private))))))) [dispatch store type])
      :conn
      (use-callback (fn [x] x) []))))

(defn use-image
  "React hook which accepts a string that uniquely identifies an image
   and returns a URL string once the image has been retrieved from
   local storage or from over the network. Calls to this hook are cached
   and simultaneous calls do not result in duplicate retrievals.
   ```
   (let [url (use-image 'ded8f539f84b9eb1bce78836aa017affae7661c3')]
     ($ :img {:src url}))
   ```"
  [checksum]
  (let [[urls on-request] (use-context image)]
    (use-effect
     (fn [] (on-request checksum)) [on-request checksum])
    (get urls checksum)))
