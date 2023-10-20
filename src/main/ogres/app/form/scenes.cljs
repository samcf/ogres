(ns ogres.app.form.scenes
  (:require [ogres.app.hooks :refer [use-dispatch use-image use-image-uploader use-query]]
            [ogres.app.render :refer [icon pagination]]
            [uix.core :as uix :refer [defui $ use-ref use-state]]))

(def ^:private options-vis
  [["Revealed" :revealed] ["Obscured" :dimmed] ["Hidden" :hidden]])

(def ^:private options-day
  [["Day" :none] ["Dusk" :dusk] ["Midnight" :midnight]])

(def ^:private per-page 6)

(def ^:private header-query
  [{:root/scene-images [:image/checksum]}])

(def ^:private query
  [{:root/scene-images [:image/checksum]}
   {:root/local
    [{:local/camera
      [[:camera/label :default ""]
       [:camera/draw-mode :default :select]
       {:camera/scene
        [:db/key
         [:scene/grid-size]
         [:scene/dark-mode :default false]
         [:scene/lighting :default :revealed]
         [:scene/show-grid :default true]
         [:scene/timeofday :default :none]
         {:scene/image
          [:image/checksum]}]}]}]}])

(defui ^:private thumbnail
  [{:keys [checksum children]}]
  (let [data-url (use-image checksum)]
    (children {:data-url data-url})))

(defui form []
  (let [dispatch (use-dispatch)
        result   (use-query query [:db/ident :root])
        data     (vec (reverse (:root/scene-images result)))
        pages    (int (js/Math.ceil (/ (count data) per-page)))
        {{{scene :camera/scene} :local/camera
          camera :local/camera} :root/local} result
        [page set-page] (use-state 1)]
    ($ :<>
      ($ :section
        ($ :header "Options")
        ($ :div.scene-options
          ($ :fieldset.box {:style {:grid-area "visi-revealed / visi-revealed / visi-hidden / visi-hidden"}})
          ($ :fieldset.box {:style {:grid-area "tofd-none / tofd-none / tofd-midnight / tofd-midnight"}})
          ($ :fieldset.text {:style {:grid-area "title"}}
            ($ :input
              {:type "text"
               :title "Scene name"
               :maxLength 36
               :spellCheck false
               :placeholder "Name"
               :value (:camera/label camera)
               :on-change
               (fn [event]
                 (let [value (.. event -target -value)]
                   (dispatch :camera/change-label value)))}))
          ($ :fieldset.text {:style {:grid-area "tile-size"}}
            ($ :input
              {:type "number"
               :title "Tile size (px)"
               :value (or (:scene/grid-size scene) "")
               :disabled (nil? (:scene/image scene))
               :placeholder "Tile size (px)"
               :on-change
               (fn [event]
                 (let [value (.. event -target -value)
                       value (js/Number value)]
                   (if (= value 0)
                     (dispatch :scene/retract-grid-size)
                     (dispatch :scene/change-grid-size value))))}))
          ($ :fieldset.option {:style {:grid-area "show-grid"}}
            ($ :input
              {:id "show-grid"
               :type "checkbox"
               :checked (:scene/show-grid scene)
               :on-change #(dispatch :scene/toggle-show-grid (.. % -target -checked))})
            ($ :label {:for "show-grid"} "Show grid"))
          ($ :fieldset.option {:style {:grid-area "dark-grid"}}
            ($ :input
              {:id "dark-grid"
               :type "checkbox"
               :checked (:scene/dark-mode scene)
               :on-change #(dispatch :scene/toggle-dark-mode (.. % -target -checked))})
            ($ :label {:for "dark-grid"} "Use dark grid"))
          ($ :fieldset.option {:style {:grid-area "grid-align"}}
            ($ :input
              {:id "grid-align"
               :type "checkbox"
               :checked false
               :disabled true})
            ($ :label {:for "grid-align"} "Align to grid"))
          (for [[label value] options-vis
                :let [on-change #(dispatch :scene/change-lighting value)]]
            ($ :fieldset.option {:key value :style {:grid-area (str "visi-" (name value))}}
              ($ :input
                {:id (name value)
                 :type "radio"
                 :name "visi"
                 :value value
                 :checked (= (:scene/lighting scene) value)
                 :disabled (nil? (:scene/image scene))
                 :on-change on-change})
              ($ :label {:for (name value)} label)))
          (for [[label value] options-day
                :let [on-change #(dispatch :scene/change-time-of-day value)]]
            ($ :fieldset.option {:key value :style {:grid-area (str "tofd-" (name value))}}
              ($ :input
                {:id (name value)
                 :type "radio"
                 :name "tofd"
                 :value value
                 :checked (= (:scene/timeofday scene) value)
                 :disabled (nil? (:scene/image scene))
                 :on-change on-change})
              ($ :label {:for (name value)} label)))))
      (if (seq data)
        (let [src (* (dec (min page pages)) per-page)
              dst (min (+ src per-page) (count data))
              seq (->> (repeat per-page :placeholder)
                       (into (subvec data src dst))
                       (take per-page)
                       (map-indexed vector))]
          ($ :section
            ($ :header "Gallery")
            ($ :.scene-gallery
              (for [[idx data] seq]
                (if-let [checksum (:image/checksum data)]
                  ($ thumbnail {:key idx :checksum checksum}
                    (fn [{:keys [data-url]}]
                      ($ :figure.scene-gallery-item
                        {:style {:background-image (str "url(" data-url ")")}
                         :data-type "image"
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
                  ($ :figure.scene-gallery-item {:key idx :data-type "placeholder"})))
              ($ pagination
                {:pages pages
                 :value (min page pages)
                 :on-change set-page}))))
        ($ :section
          ($ :header "Gallery")
          ($ :.prompt
            "Upload one or more images from your"
            ($ :br) "computer to use as a map. Its best"
            ($ :br) "to use files below 2MB for an optimal"
            ($ :br) "multiplayer experience."))))))

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
