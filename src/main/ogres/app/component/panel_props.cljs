(ns ogres.app.component.panel-props
  (:require [ogres.app.component :refer [icon image]]
            [ogres.app.hooks :as hooks]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:root/props-images
    [:image/hash
     :image/name
     :image/width
     :image/height
     {:image/thumbnail
      [:image/hash]}]}])

(defui ^:private images []
  (let [result (hooks/use-query query [:db/ident :root])
        images (:root/props-images result)]
    (prn images)
    ($ :.props-gallery
      ($ :.props-gallery-page
        (for [image-data images]
          ($ image
            {:key (:image/hash image-data)
             :hash (:image/hash (:image/thumbnail image-data))}
            (fn [url]
              ($ :.props-gallery-image
                {:style {:background-image (str "url(" url ")")}}))))))))

(defui gallery []
  ($ :.form-props
    ($ :header ($ :h2 "Props"))
    ($ images)
    ($ :.form-notice
      "Props are images you can upload and use in the scene as decoration,
       vehicles, or effects. Once placed within the scene, they can be
       resized, rotated, and moved around freely.")))

(defui upload []
  (let [upload (hooks/use-image-uploader {:type :props})
        input (uix/use-ref)]
    ($ :<>
      ($ :input
        {:ref input
         :type "file"
         :hidden true
         :accept "image/*"
         :multiple true
         :on-change
         (fn [event]
           (upload (.. event -target -files))
           (set! (.. event -target -value) ""))})
      ($ :button.button.button-neutral
        {:on-click (fn [] (.click @input))}
        ($ icon {:name "camera-fill" :size 16})
        "Upload prop images")
      ($ :button.button.button-danger
        ($ icon {:name "trash3-fill" :size 16})))))
