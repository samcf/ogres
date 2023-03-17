(ns ogres.app.form.scenes
  (:require [ogres.app.form.render :as render]
            [ogres.app.hooks :refer [use-dispatch use-image use-image-uploader use-query]]
            [ogres.app.render :refer [icon pagination]]
            [uix.core.alpha :as uix]))

(def ^:private per-page 6)

(def ^:private header-query
  '[{(:root/scenes :limit 1) [:image/checksum]}])

(def ^:private query
  [{:root/scenes [:image/checksum]}])

(defn- thumbnail [checksum render-fn]
  (render-fn (use-image checksum)))

(defn- header []
  (let [dispatch (use-dispatch)
        result   (use-query header-query [:db/ident :root])
        upload   (use-image-uploader {:type :scene})
        input    (uix/ref)]
    [:<>
     [:button.upload
      {:type     "button"
       :title    "Upload map image"
       :on-click (fn [event]
                   (.stopPropagation event)
                   (.click (deref input)))}
      [:input
       {:type "file" :hidden true :accept "image/*" :multiple true :ref input
        :on-click  (fn [event] (.stopPropagation event))
        :on-change (fn [event]
                     (doseq [file (.. event -target -files)]
                       (upload file)))}]
      [icon {:name "camera-fill" :size 16}] "Upload"]
     [:button.remove
      {:type     "button"
       :title    "Remove all"
       :disabled (empty? (:root/scenes result))
       :on-click (fn [event]
                   (.stopPropagation event)
                   (dispatch :scene/remove-all))}
      [icon {:name "trash3-fill" :size 16}]]]))

(defn- form []
  (let [dispatch (use-dispatch)
        result   (use-query query [:db/ident :root])
        data     (vec (:root/scenes result))
        page     (uix/state 1)
        pages    (int (js/Math.ceil (/ (count data) per-page)))]
    (if (seq data)
      (let [src (* (dec (min @page pages)) per-page)
            dst (min (+ src per-page) (count data))
            seq (->> (repeat per-page :placeholder)
                     (into (subvec data src dst))
                     (take per-page)
                     (map-indexed vector))]
        [:section.scenes
         (for [[idx data] seq]
           (if-let [checksum (:image/checksum data)]
             ^{:key checksum}
             [thumbnail checksum
              (fn [url]
                [:figure.scenes-scene
                 {:style {:background-image (str "url(" url ")")}
                  :on-click #(dispatch :canvas/change-scene checksum)}
                 [:button
                  {:type "button"
                   :title "Remove"
                   :on-click #(dispatch :scene/remove checksum)}
                  [icon {:name "trash3-fill" :size 16}]]])]
             [:figure.scenes-placeholder
              {:key idx}]))
         [pagination
          {:pages pages
           :value (min @page pages)
           :on-change (partial reset! page)}]])
      [:section.scenes
       [:div.prompt
        "Upload one or more images from your"
        [:br] "computer to use as a map. Its best"
        [:br] "to use files below 2MB for an optimal"
        [:br] "multiplayer experience."]])))

(defmethod render/header :scenes [] header)
(defmethod render/form :scenes [] form)
