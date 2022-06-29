(ns ogre.tools.provider.image
  (:require [datascript.core :as ds :refer [squuid]]
            [ogre.tools.image :as image]
            [ogre.tools.provider.events :refer [subscribe!]]
            [ogre.tools.provider.state :refer [use-dispatch]]
            [ogre.tools.provider.storage :refer [use-store]]
            [uix.core.alpha :as uix]))

(def cache (atom {}))

(defn create-object-url [data-url]
  (-> (.fetch js/window data-url)
      (.then (fn [r] (.blob r)))
      (.then (fn [b]
               (->> (js/File. #js [b] "image" #js {:type (.-type b)})
                    (js/URL.createObjectURL))))))

(defn use-image [checksum]
  (let [dispatch         (use-dispatch)
        store            (use-store)
        sentinel         (uix/state 0)
        watch-key        (deref (uix/state (squuid)))
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
       (let [hash   (image/checksum data-url)
             record #js {:checksum hash :data data-url :created-at (.now js/Date)}
             result (create-object-url data-url)]
         (.then result (fn [] (.put (.table store "images") record)))
         (.then result (fn [url] (swap! cache assoc hash [false url]))))) :image/cache [])

    (uix/effect!
     (fn []
       (if (and (string? checksum) (not (or loading cached)))
         (-> (.table store "images")
             (.get checksum)
             (.then (fn [rec] (create-object-url (.-data rec))))
             (.then (fn [url] (swap! cache assoc checksum [false url])))
             (.catch (fn [] (dispatch :image/request checksum))))))
     [checksum])

    cached))
