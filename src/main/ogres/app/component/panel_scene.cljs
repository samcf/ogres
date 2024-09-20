(ns ogres.app.component.panel-scene
  (:require [clojure.string :refer [replace]]
            [ogres.app.component :refer [icon pagination image]]
            [ogres.app.hooks :refer [use-dispatch use-image-uploader use-query]]
            [ogres.app.util :refer [display-size]]
            [uix.core :as uix :refer [defui $ use-ref use-state]]
            [uix.dom :refer [create-portal]]))

(def ^:private options-vis
  [["Revealed" :revealed "sun-fill"]
   ["Obscured" :dimmed "cloud-sun-fill"]
   ["Hidden" :hidden "moon-fill"]])

(def ^:private per-page 6)

(def ^:private filesize-limit 8e6)

(def ^:private filename-re #"\d+x\d+|[^\w ]|.[^.]+$")

(defn ^:private render-scene-name [camera]
  (if-let [label (:camera/label camera)]
    label
    (if-let [filename (-> camera :camera/scene :scene/image :image/name)]
      (-> filename (replace filename-re "") (replace  #"\s{2,}" " "))
      "Untitled scene")))

(def ^:private query
  [{:root/scene-images
    [:image/hash
     :image/name
     :image/size
     {:image/thumbnail
      [:image/hash]}]}
   {:root/user
    [{:user/camera
      [:db/id
       :camera/label
       {:camera/scene
        [:db/id
         [:scene/grid-size :default 70]
         [:scene/show-grid :default true]
         [:scene/grid-align :default false]
         [:scene/dark-mode :default false]
         [:scene/lighting :default :revealed]
         {:scene/image
          [:image/hash
           :image/name
           {:image/thumbnail
            [:image/hash]}]}]}]}]}])

(defui form []
  (let [[preview set-preview] (use-state nil)
        dispatch (use-dispatch)
        upload   (use-image-uploader {:type :scene})
        input    (use-ref)
        data     (use-query query [:db/ident :root])
        {{{scene :camera/scene} :user/camera
          camera :user/camera} :root/user} data
        [page set-page] (use-state 1)]
    ($ :form.form-scenes
      {:on-submit (fn [event] (.preventDefault event))}
      ($ :header ($ :h2 "Scene"))
      ($ :fieldset.fieldset
        ($ :legend "Name")
        ($ :input.text.text-ghost
          {:type "text"
           :maxLength 36
           :spellCheck false
           :placeholder (render-scene-name camera)
           :value (or (:camera/label camera) "")
           :on-change
           (fn [event]
             (let [value (.. event -target -value)]
               (if (not= value "")
                 (dispatch :camera/change-label value)
                 (dispatch :camera/remove-label))))}))
      (let [img (vec (:root/scene-images data))
            pgs (int (js/Math.ceil (/ (count img) per-page)))
            src (* (max (dec (min page pgs)) 0) per-page)
            dst (min (+ src per-page) (count img))
            pag (->> (repeat per-page :placeholder)
                     (into (subvec img src dst))
                     (take per-page)
                     (map-indexed vector))]
        ($ :fieldset.fieldset
          ($ :legend "Background image")
          ($ :.scene-gallery
            (for [[idx data] pag
                  :let [hash (:image/hash data)
                        curr (:image/hash (:scene/image scene))]]
              (if-let [thumbnail (:image/hash (:image/thumbnail data))]
                ($ image {:key idx :hash thumbnail}
                  (fn [url]
                    ($ :fieldset.scene-gallery-thumbnail
                      {:data-type "image" :style {:background-image (str "url(" url ")")}}
                      ($ :label {:aria-label (:image/name data)}
                        ($ :input
                          {:type "radio"
                           :name "background-image"
                           :value hash
                           :checked (= hash curr)
                           :on-change
                           (fn [event]
                             (let [value (.. event -target -value)]
                               (dispatch :scene/change-image value)))}))
                      ($ :button.button.button-neutral
                        {:type "button"
                         :name "info"
                         :aria-label "Preview"
                         :on-click
                         (fn [event]
                           (.stopPropagation event)
                           (set-preview hash))}
                        ($ icon {:name "zoom-in" :size 18}))
                      ($ :button.button.button-danger
                        {:type "button"
                         :name "remove"
                         :aria-label "Remove"
                         :on-click
                         (fn [event]
                           (.stopPropagation event)
                           (dispatch :scene-images/remove hash thumbnail))}
                        ($ icon {:name "trash3-fill" :size 18}))
                      (if (> (:image/size data) filesize-limit)
                        ($ :button.button.button-warning
                          {:type "button"
                           :name "warn"
                           :aria-label "Exceeds filesize limit"
                           :data-tooltip "Exceeds filesize limit"
                           :on-click
                           (fn [event]
                             (.stopPropagation event)
                             (set-preview hash))}
                          ($ icon {:name "exclamation-triangle-fill" :size 18}))))))
                ($ :.scene-gallery-thumbnail {:key idx :data-type "placeholder"}))))
          ($ :fieldset.scene-gallery-form
            ($ :button.button.button-neutral
              {:type "button" :on-click #(.click (deref input))}
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
              ($ icon {:name "camera-fill" :size 16}) "Upload images")
            (if (> pgs 1)
              ($ pagination
                {:name "scenes-gallery"
                 :pages pgs
                 :value (min page pgs)
                 :on-change set-page})))
          (if (some? preview)
            (let [node (js/document.querySelector "#root")
                  data (first (filter (comp #{preview} :image/hash) (:root/scene-images data)))]
              (create-portal
               ($ :.scene-gallery-modal
                 ($ :.scene-gallery-modal-container
                   ($ image {:hash preview}
                     (fn [url]
                       ($ :figure.scene-gallery-modal-preview
                         {:style {:background-image (str "url(" url ")")}}
                         ($ :dl
                           ($ :dt "Filename")
                           ($ :dd (:image/name data))
                           ($ :dt "Size")
                           ($ :dd (display-size (:image/size data)))
                           (if (> (:image/size data) filesize-limit)
                             ($ :<>
                               ($ :dt ($ icon {:name "exclamation-triangle-fill" :size 12}) "Warning")
                               ($ :dd
                                 "This image exceeds the maximum image filesize (8MB)"
                                 " that can be used for multiplayer games."
                                 " Decreasing its dimensions, converting it to a JPG,"
                                 " and lowering its image quality may help.")))))))
                   ($ :.scene-gallery-modal-footer
                     ($ :button.button.button-danger
                       {:style {:margin-right "auto"}
                        :on-click
                        (fn []
                          (let [thumbnail (:image/hash (:image/thumbnail data))]
                            (set-preview nil)
                            (dispatch :scene-images/remove preview thumbnail)))}
                       ($ icon {:name "trash3-fill" :size 16}))
                     ($ :button.button.button-neutral
                       {:on-click #(set-preview nil)}
                       "Close")
                     ($ :button.button.button-primary
                       {:on-click (fn [] (set-preview nil) (dispatch :scene/change-image preview))}
                       "Change background"))))
               node)))))
      ($ :fieldset.fieldset
        ($ :legend "Tile size ( px )")
        ($ :input.text.text-ghost
          {:type "number"
           :name "Tile size"
           :value (:scene/grid-size scene)
           :placeholder "70px"
           :on-change
           (fn [event]
             (let [value (.. event -target -value)
                   value (js/Number value)]
               (if (= value 0)
                 (dispatch :scene/retract-grid-size)
                 (dispatch :scene/change-grid-size value))))})
        ($ :details
          ($ :summary "More Information")
          "The tile size is the width, in pixels, of one square in the
                         selected background image. Changes to this value will scale the
                         image such that each square will take up the width of one token."))
      ($ :fieldset.fieldset
        ($ :legend "Grid options")
        ($ :.input-group
          ($ :label.checkbox
            ($ :input
              {:type "checkbox"
               :checked (:scene/show-grid scene)
               :on-change #(dispatch :scene/toggle-show-grid (.. % -target -checked))})
            ($ icon {:name "check" :size 20})
            "Show grid")
          ($ :label.checkbox
            ($ :input
              {:type "checkbox"
               :checked (:scene/grid-align scene)
               :on-change #(dispatch :scene/toggle-grid-align (.. % -target -checked))})
            ($ icon {:name "check" :size 20})
            "Align to grid")
          ($ :label.checkbox
            ($ :input
              {:type "checkbox"
               :checked (:scene/dark-mode scene)
               :on-change #(dispatch :scene/toggle-dark-mode (.. % -target -checked))})
            ($ icon {:name "check" :size 20})
            "Use dark grid")))
      ($ :fieldset.fieldset.fieldset--radio
        ($ :legend "Lighting")
        ($ :.input-group
          (for [[label value icon-name] options-vis
                :let [on-change #(dispatch :scene/change-lighting value)]]
            ($ :<> {:key value}
              ($ :label.radio
                ($ :input
                  {:type "radio"
                   :name "visi"
                   :value value
                   :checked (= (:scene/lighting scene) value)
                   :on-change on-change})
                ($ icon {:name icon-name :size 16})
                label))))
        ($ :details
          ($ :summary "More Information")
          "When set to " ($ :strong "Obscured") " or " ($ :strong "Hidden")
          ", the scene will be shrouded in darkness and tokens will emit a
           radius of light around themselves. The light radius of each token
           can be customized."))
      ($ :fieldset.fieldset.scene-gallery-grid-align
        ($ :legend "Grid alignment")
        ($ :div.form-notice
          ($ :button
            {:on-click #(dispatch :camera/change-mode :grid)}
            ($ icon {:name "compass" :size 22}))
          "Use the grid alignment tool to manually pick a point that will serve
           as the origin of the grid. Use this tool when the grid in your scene
           image is offset from the edges or is unevenly distributed.")
        ($ :details
          ($ :summary "How To Use")
          ($ :ol
            ($ :li "Select the grid alignment tool.")
            ($ :li "Select a corner of one of the tiles in the scene.")
            ($ :li "Adjust its position carefully if necessary.")
            ($ :li "Adjust the tile size until the grid lines match up with the lines on the scene."))
          "Sometimes the widths of the tiles in the image are not whole
           numbers; it may be necessary to use this tool again in another
           part of the image as the adventure progresses there.")))))
