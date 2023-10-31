(ns ogres.app.form.scenes
  (:require [clojure.string :refer [blank?]]
            [ogres.app.hooks :refer [use-dispatch use-image use-image-uploader use-query]]
            [ogres.app.render :refer [icon pagination]]
            [uix.core :as uix :refer [defui $ use-ref use-state]]
            [uix.dom :refer [create-portal]]))

(def ^:private options-vis
  [["Revealed" :revealed] ["Obscured" :dimmed] ["Hidden" :hidden]])

(def ^:private options-day
  [["Day" :none] ["Dusk" :dusk] ["Midnight" :midnight]])

(def ^:private per-page 6)

(defn ^:private remove-prompt [label]
  (str "Are you sure you want to remove "
       (if (blank? label)
         (str "this scene")
         (str "'" label "'")) "?"))

(def ^:private query
  [{:root/scene-images [:image/checksum]}
   {:root/session
    [{:session/conns
      [{:local/camera
        [{:camera/scene [:db/key]}]}]}]}
   {:root/local
    [{:local/cameras
      [:db/key
       :camera/label
       {:camera/scene
        [{:scene/image [:image/checksum]}
         :db/key
         :scene/tokens
         :scene/initiative]}]}
     {:local/camera
      [:db/key
       [:camera/label :default ""]
       {:camera/scene
        [:db/key
         [:scene/grid-size]
         [:scene/dark-mode :default false]
         [:scene/lighting :default :revealed]
         [:scene/show-grid :default true]
         [:scene/timeofday :default :none]
         {:scene/image [:image/checksum]}]}]}]}])

(defui ^:private thumbnail
  [{:keys [checksum children]}]
  (let [data-url (use-image checksum)]
    (children {:data-url data-url})))

(defui form []
  (let [[preview set-preview] (use-state nil)
        dispatch (use-dispatch)
        uploader (use-image-uploader {:type :scene})
        input    (use-ref)
        data     (use-query query [:db/ident :root])
        {{{scene :camera/scene} :local/camera
          camera :local/camera} :root/local} data
        [page set-page] (use-state 1)]
    ($ :<>
      ($ :section.scene-options
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
            ($ :label {:for (name value)} label))))
      (let [img (vec (:root/scene-images data))
            pgs (int (js/Math.ceil (/ (count img) per-page)))
            src (* (max (dec (min page pgs)) 0) per-page)
            dst (min (+ src per-page) (count img))
            pag (->> (repeat per-page :placeholder)
                     (into (subvec img src dst))
                     (take per-page)
                     (map-indexed vector))]
        ($ :section.scene-images
          ($ :.scene-images-label "Background images" " " "[" (count img) "]")
          ($ :fieldset.scene-gallery
            (for [[idx data] pag
                  :let [selected
                        (= (:image/checksum data)
                           (:image/checksum (:scene/image scene)))]]
              (if-let [checksum (:image/checksum data)]
                ($ thumbnail {:key idx :checksum checksum}
                  (fn [{:keys [data-url]}]
                    ($ :figure.scene-gallery-thumbnail
                      {:style {:background-image (str "url(" data-url ")")}
                       :data-type "image"
                       :data-selected selected
                       :on-click
                       (fn [event]
                         (.stopPropagation event)
                         (dispatch :scene/change-image checksum))}
                      ($ :button.button.button-neutral
                        {:type "button"
                         :on-click
                         (fn [event]
                           (.stopPropagation event)
                           (set-preview checksum))}
                        ($ icon {:name "zoom-in" :size 18}))
                      ($ :button.button.button-danger
                        {:type "button"
                         :on-click
                         (fn [event]
                           (.stopPropagation event)
                           (dispatch :scene-images/remove checksum))}
                        ($ icon {:name "trash3-fill" :size 18})))))
                ($ :figure.scene-gallery-thumbnail
                  {:key idx :data-type "placeholder"}))))
          ($ :fieldset.scene-gallery-form
            ($ :button.button.button-neutral
              {:on-click #(.click (deref input))}
              ($ :input
                {:ref input
                 :type "file"
                 :hidden true
                 :accept "image/*"
                 :multiple true
                 :on-change
                 (fn [event]
                   (doseq [file (.. event -target -files)]
                     (uploader file))
                   (set! (.. event -target -value) ""))})
              ($ icon {:name "camera-fill" :size 16}) "Upload images")
            (if (> pgs 1)
              ($ pagination
                {:pages pgs
                 :value (min page pgs)
                 :on-change set-page})))
          (if (not (nil? preview))
            (if-let [node (js/document.querySelector "#root")]
              (create-portal
               ($ :.scene-gallery-modal
                 ($ :.scene-gallery-modal-container
                   ($ thumbnail {:checksum preview}
                     (fn [{:keys [data-url]}]
                       ($ :figure.scene-gallery-modal-preview
                         {:style {:background-image (str "url(" data-url ")")}})))
                   ($ :.scene-gallery-modal-footer
                     ($ :button.button.button-danger
                       {:style {:margin-right "auto"}
                        :on-click (fn [] (set-preview nil) (dispatch :scene-images/remove preview))}
                       ($ icon {:name "trash3-fill" :size 16}))
                     ($ :button.button.button-neutral
                       {:on-click #(set-preview nil)}
                       "Close")
                     ($ :button.button.button-primary
                       {:on-click (fn [] (set-preview nil) (dispatch :scene/change-image preview))}
                       "Change background"))))
               node)))))
      (let [{{cameras :local/cameras} :root/local
             {players :session/conns} :root/session} data
            viewing (frequencies (map (comp :db/key :camera/scene :local/camera) players))]
        ($ :section.scene-scenes
          ($ :header "Scenes" " " "[" (count cameras) "]")
          ($ :ul.scene-list
            (for [entity cameras
                  :let [on-select  #(dispatch :scenes/change (:db/key entity))
                        on-remove  #(dispatch :scenes/remove (:db/key entity))
                        scene      (:camera/scene entity)
                        sum-tokens (count (:scene/tokens scene))
                        sum-initiv (count (:scene/initiative scene))
                        sum-playrs (viewing (:db/key scene))]]
              ($ :li.scene-list-item
                {:key (:db/key entity) :data-selected (= (:db/key entity) (:db/key camera))}
                (if-let [checksum (:image/checksum (:scene/image (:camera/scene entity)))]
                  ($ thumbnail {:checksum checksum}
                    (fn [{:keys [data-url]}]
                      ($ :.scene-list-item-image
                        {:style {:background-image (str "url(" data-url ")")}
                         :on-click on-select
                         :data-image "image"})))
                  ($ :.scene-list-item-image
                    {:on-click on-select :data-image "empty"}))
                ($ :.scene-list-item-content
                  (if-let [label (:camera/label entity)]
                    ($ :.scene-list-item-label label)
                    ($ :.scene-list-item-label "New scene"))
                  ($ :fieldset.scene-list-item-badges {:style {:grid-area "badges"}}
                    (if (> sum-tokens 0)
                      ($ :.scene-list-item-badge {:data-tooltip "Tokens"}
                        ($ icon {:name "person-circle" :size 14}) sum-tokens))
                    (if (> sum-initiv 0)
                      ($ :.scene-list-item-badge {:data-tooltip "Initiative"}
                        ($ icon {:name "hourglass-split" :size 14}) sum-initiv))
                    (if (> sum-playrs 0)
                      ($ :.scene-list-item-badge {:data-tooltip "Players"}
                        ($ icon {:name "people-fill" :size 14}) sum-playrs))))
                ($ :button.scene-list-item-remove.button.button-neutral
                  {:on-click
                   (fn []
                     (if (js/confirm (remove-prompt (:camera/label entity)))
                       (on-remove)))}
                  ($ icon {:name "trash3-fill" :size 16}))))
            ($ :li.scene-list-item
              {:data-placeholder true}
              ($ :.scene-list-item-image {:data-image "placeholder"})
              ($ :.scene-list-item-label)
              ($ :button.scene-list-item-remove.button.button-neutral
                {:on-click #(dispatch :scenes/create)}
                ($ icon {:name "plus" :size 24})))))))))
