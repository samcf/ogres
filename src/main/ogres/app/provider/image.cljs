(ns ogres.app.provider.image
  (:require [ogres.app.provider.dispatch :refer [use-dispatch]]
            [ogres.app.provider.events :refer [use-publish use-subscribe]]
            [ogres.app.provider.state :as state :refer [use-query]]
            [ogres.app.provider.storage :refer [use-store]]
            [datascript.core :as ds]
            [uix.core :refer [defui $ create-context use-callback use-state use-context use-effect]]))

(def ^:private hash-fn "SHA-1")

(def ^:private image
  (create-context {{} (constantly nil)}))

(defn ^:private decode-digest
  "Returns a hash string of the given digest array."
  [^js/Uint8Array digest]
  (.. (js/Array.from digest)
      (map (fn [s] (.. s (toString 16) (padStart 2 "0"))))
      (join "")))

(defn ^:private create-hash
  "Returns a Promise which resolves with a hash string for the
   image data given as a Blob object."
  [^js/Blob image]
  (-> (.arrayBuffer image)
      (.then (fn [buf] (js/crypto.subtle.digest hash-fn buf)))
      (.then (fn [dig] (js/Uint8Array. dig)))
      (.then decode-digest)))

(defn ^:private create-thumbnail
  "Returns a <canvas> element with the contents of the src <canvas> cropped
   and resized to the given dimensions. Accepts two points given as {Ax Ay}
   and {Bx By} which define a square region of the source image to use as
   the crop. When no region is given, the crop is centered horizontally and
   to the top."
  ([^js/HTMLCanvasElement src len]
   (let [cx (.-width src) cy (.-height src)]
     (cond (or (= cx cy) (and (<= cx len) (<= cy len)))
           (create-thumbnail src len 0 0 cx cy)
           (> cx cy)
           (create-thumbnail src len (/ (- cx cy) 2) 0 (/ (+ cx cy) 2) cy)
           (> cy cx)
           (create-thumbnail src len 0 0 cx cx))))
  ([^js/HTMLCanvasElement src len ax ay bx by]
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
  [^js/ImageBitmap image]
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

(defn ^:private extract-image
  "Returns the image and metadata associated with the <canvas> element passed
   given an image type and quality ratio in the form of a map whose keys are
   'data', 'hash', 'width', and 'height'."
  ([^js/HTMLCanvasElement canvas]
   (extract-image canvas "image/jpeg" 0.80))
  ([^js/HTMLCanvasElement canvas type quality]
   (js/Promise.
    (fn [resolve]
      (.toBlob
       canvas
       (fn [blob]
         (.then
          (create-hash blob)
          (fn [hash]
            (resolve
             {:data   blob
              :hash   hash
              :width  (.-width canvas)
              :height (.-height canvas)})))) type quality)))))

(defui provider [props]
  (let [conn           (use-context state/context)
        store          (use-store)
        loading        (atom {})
        publish        (use-publish)
        dispatch       (use-dispatch)
        [urls set-url] (use-state {})
        on-request
        (use-callback
         (fn [hash]
           (let [url (get urls hash)]
             (if (not url)
               (when (not (get @loading hash))
                 (swap! loading assoc hash true)
                 (-> (.table store "images")
                     (.get hash)
                     (.then (fn [rec] (js/URL.createObjectURL (.-data rec))))
                     (.then (fn [url] (set-url (fn [urls] (assoc urls hash url)))))
                     (.catch (fn [] (publish {:topic :image/request :args [hash]})))))
               url))) ^:lint/disable [])]
    (use-subscribe :image/create-token
      (use-callback
       (fn [{[hash blob] :args}]
         (.then
          (js/createImageBitmap blob)
          (fn [image]
            (let [record {:hash hash :name "" :size (.-size blob) :width (.-width image) :height (.-height image)}]
              (dispatch :token-images/create-many [[record record]] :public))))) [dispatch]))
    (use-subscribe :image/cache
      (use-callback
       (fn [{[image callback] :args}]
         (-> (create-hash image)
             (.then (fn [hash] (.put (.table store "images") #js {"checksum" hash "data" image})))
             (.then (fn [hash] (set-url (fn [urls] (assoc urls hash (js/URL.createObjectURL image)))) hash))
             (.then (fn [hash] (when (fn? callback) (callback hash)))))) [store]))
    (use-subscribe :image/change-thumbnail
      (use-callback
       (fn [{[hash [ax ay bx by :as rect]] :args}]
         (let [entity (ds/entity (ds/db conn) [:image/hash hash])
               images (.table store "images")]
           (-> (.get images hash)
               (.then (fn [rec] (js/createImageBitmap (.-data rec))))
               (.then
                (fn [src]
                  (-> (create-canvas src)
                      (create-thumbnail 256 ax ay bx by)
                      (extract-image))))
               (.then
                (fn [out]
                  (js/Promise.all
                   #js [(.put images #js {"checksum" (:hash out) "data" (:data out)})
                        (.delete images (:image/hash (:image/thumbnail entity)))
                        out])))
               (.then
                (fn [[_ _ data]]
                  (dispatch :token-images/change-thumbnail hash data rect)))))) [dispatch conn store]))
    ($ image {:value [urls on-request]}
      (:children props))))

(defn ^:private process-file
  "Returns a Promise.all which resolves with three elements:
     1. The filename
     2. The image extracted from the file
     3. The image, cropped and resized to be used as a thumbnail"
  [^js/File file]
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
  {:hash   (:hash image)
   :name   filename
   :size   (.-size (:data image))
   :width  (:width image)
   :height (:height image)})

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
        publish  (use-publish)
        store    (use-store)
        user     (use-query [:user/type] [:db/ident :user])]
    (case (:user/type user)
      :host
      (use-callback
       (fn [files]
         (-> (js/Promise.all (into-array (into [] (map process-file) files)))
             (.then
              (fn [files]
                (let [records (into-array (into [] (mapcat create-store-records) files))]
                  (.then (.bulkPut (.table store "images") records)
                         (constantly files)))))
             (.then
              (fn [files]
                (let [records (into [] (map create-state-records) files)]
                  (case type
                    :token (dispatch :token-images/create-many records :private)
                    :scene (dispatch :scene-images/create-many records :private))))))) [dispatch store type])
      :conn
      (use-callback
       (fn [files]
         (.then (js/Promise.all (into-array (into [] (map process-file) files)))
                (fn [files]
                  (doseq [[_ _ thumbnail] files]
                    (publish {:topic :image/create :args [(:data thumbnail)]}))))) [publish]))))

(defn use-image
  "React hook which accepts a string that uniquely identifies an image
   and returns a URL string once the image has been retrieved from
   local storage or from over the network. Calls to this hook are cached
   and simultaneous calls do not result in duplicate retrievals.
   ```
   (let [url (use-image 'ded8f539f84b9eb1bce78836aa017affae7661c3')]
     ($ :img {:src url}))
   ```"
  [hash]
  (let [[urls on-request] (use-context image)]
    (use-effect
     (fn [] (on-request hash)) [on-request hash])
    (get urls hash)))
