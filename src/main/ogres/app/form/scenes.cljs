(ns ogres.app.form.scenes
  (:require [ogres.app.form.render :as render]
            [ogres.app.hooks :refer [use-dispatch use-image use-image-uploader use-query]]
            [ogres.app.render :refer [icon pagination]]
            [uix.core.alpha :as uix]))

(def ^:private query
  [{:root/scenes [:image/checksum]}])

(defn- thumbnail [checksum render-fn]
  (render-fn (use-image checksum)))

(defn- form []
  (let [page-num (uix/state 1)
        dispatch (use-dispatch)
        result   (use-query query [:db/ident :root])
        scenes   (into [] (:root/scenes result))]
    [:<>
     [:section.scenes
      (let [src (* (dec @page-num) 6)
            dst (min (+ src 6) (count scenes))]
        (for [{:image/keys [checksum]} (subvec scenes src dst)]
          ^{:key checksum}
          [thumbnail checksum
           (fn [url]
             [:figure
              {:style {:background-image (str "url(" url ")")}
               :on-click #(dispatch :canvas/change-scene checksum)}
              [:button
               {:type "button"
                :title "Remove"
                :on-click #(dispatch :scene/remove checksum)}
               [icon {:name "x-circle" :size 20}]]])]))]
     (if (seq scenes)
       [:section {:style {:display "flex" :justify-content "center"}}
        [pagination
         {:value     @page-num
          :pages     (int (js/Math.ceil (/ (count scenes) 6)))
          :on-change (partial reset! page-num)}]]
       [:section
        [:div.prompt
         [:br] "Upload one or more images from your"
         [:br] "computer to use as a map. Its best"
         [:br] "to use files below 2MB for an optimal"
         [:br] "multiplayer experience."]])]))

(defn- header []
  (let [input  (uix/ref)
        upload (use-image-uploader {:type :scene})]
    [:button.upload-button
     {:type "button"
      :on-click
      (fn [event]
        (.stopPropagation event)
        (.click @input))}
     [:input
      {:type "file" :hidden true :accept "image/*" :multiple true
       :ref input
       :on-click
       (fn [event]
         (.stopPropagation event))
       :on-change
       (fn [event]
         (doseq [file (.. event -target -files)]
           (upload file)))}]
     [icon {:name "camera-fill" :size 18}]
     "Upload"]))

(defmethod render/header :scenes [] header)
(defmethod render/form :scenes [] form)
