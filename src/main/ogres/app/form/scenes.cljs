(ns ogres.app.form.scenes
  (:require [ogres.app.hooks :refer [use-dispatch use-image use-image-uploader use-query]]
            [ogres.app.render :refer [icon pagination]]
            [uix.core :as uix :refer [defui $ use-ref use-state]]))

(def ^:private per-page 6)

(def ^:private header-query
  [{:root/scene-images [:image/checksum]}])

(def ^:private query
  [{:root/scene-images [:image/checksum]}])

(defui thumbnail [{:keys [checksum children]}]
  (let [data-url (use-image checksum)]
    (children {:data-url data-url})))

(defui form []
  (let [dispatch (use-dispatch)
        result   (use-query query [:db/ident :root])
        data     (vec (:root/scene-images result))
        pages    (int (js/Math.ceil (/ (count data) per-page)))
        [page set-page] (use-state 1)]
    (if (seq data)
      (let [src (* (dec (min page pages)) per-page)
            dst (min (+ src per-page) (count data))
            seq (->> (repeat per-page :placeholder)
                     (into (subvec data src dst))
                     (take per-page)
                     (map-indexed vector))]
        ($ :section.scenes
          (for [[idx data] seq]
            (if-let [checksum (:image/checksum data)]
              ($ thumbnail {:key idx :checksum checksum}
                (fn [{:keys [data-url]}]
                  ($ :figure.scenes-scene
                    {:style {:background-image (str "url(" data-url ")")}
                     :on-click
                     (fn [event]
                       (.stopPropagation event)
                       (dispatch :scene/change-image checksum))}
                    ($ :button
                      {:type "button" :title "Remove"
                       :on-click
                       (fn [event]
                         (.stopPropagation event)
                         (dispatch :scene-images/remove checksum))}
                      ($ icon {:name "trash3-fill" :size 16})))))
              ($ :figure.scenes-placeholder {:key idx})))
          ($ pagination
            {:pages pages
             :value (min page pages)
             :on-change set-page})))
      ($ :section.scenes
        ($ :div.prompt
          "Upload one or more images from your"
          ($ :br) "computer to use as a map. Its best"
          ($ :br) "to use files below 2MB for an optimal"
          ($ :br) "multiplayer experience.")))))

(defui footer []
  (let [dispatch (use-dispatch)
        result   (use-query header-query [:db/ident :root])
        scenes   (sequence (map :image/checksum) (:root/scene-images result))
        upload   (use-image-uploader {:type :scene})
        input    (use-ref)]
    ($ :<>
      ($ :button.button.button-neutral
        {:type     "button"
         :title    "Upload map image"
         :on-click #(.click (deref input))}
        ($ :input
          {:type "file" :hidden true :accept "image/*" :multiple true :ref input
           :on-change
           (fn [event]
             (doseq [file (.. event -target -files)]
               (upload file))
             (set! (.. event -target -value) ""))})
        ($ icon {:name "camera-fill" :size 16}) "Select Files")
      ($ :button.button.button-danger
        {:type     "button"
         :title    "Remove all"
         :disabled (empty? scenes)
         :on-click #(dispatch :scene-images/remove-all scenes)}
        ($ icon {:name "trash3-fill" :size 16})))))
