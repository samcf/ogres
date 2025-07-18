(ns ogres.app.component.panel-scene
  (:require [clojure.string :refer [replace]]
            [ogres.app.component :as component :refer [icon]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.hooks :as hooks]
            [ogres.app.util :refer [display-size]]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query
  [{:root/scene-images
    [:db/id
     :image/hash
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
         [:scene/grid-size :default grid-size]
         [:scene/show-grid :default true]
         [:scene/grid-align :default false]
         [:scene/dark-mode :default false]
         [:scene/show-object-outlines :default true]
         [:scene/lighting :default :revealed]
         {:scene/image
          [:image/hash
           :image/name
           {:image/thumbnail
            [:image/hash]}]}]}]}]}])

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

(def ^:private query-scenes
  [{:root/scene-images
    [:db/id
     :image/hash
     :image/name
     :image/size
     {:image/thumbnail
      [:image/hash]}]}])

(defui ^:private scene-editor [props]
  (let [result (hooks/use-query query-scenes [:db/ident :root])
        scene  (first (filter (comp #{(:id props)} :db/id) (:root/scene-images result)))]
    ($ component/fullscreen-dialog
      {:on-close (:on-close props)}
      ($ :.scene-editor
        ($ :.scene-editor-workspace
          ($ component/image {:hash (:image/hash scene)}
            (fn [url]
              ($ :img.scene-editor-image
                {:src url})))
          ($ :dl
            ($ :dt "Filename")
            ($ :dd (:image/name scene))
            ($ :dt "Size")
            ($ :dd (display-size (:image/size scene)))
            (if (> (:image/size scene) filesize-limit)
              ($ :<>
                ($ :dt ($ icon {:name "exclamation-triangle-fill" :size 12}) "Warning")
                ($ :dd
                  "This image exceeds the maximum image filesize (8MB)"
                  " that can be used for multiplayer games."
                  " Reducing its dimensions and lowering its image quality "
                  " may help.")))))
        ($ :.scene-editor-gallery
          ($ component/paginated
            {:data (:root/scene-images result) :page-size 12}
            (fn [{:keys [data pages page on-change]}]
              ($ :.scene-editor-gallery-paginated
                ($ :.scene-editor-gallery-thumbnails
                  (for [scene data
                        :let [thumb (:image/hash (:image/thumbnail scene))
                              check (= (:db/id scene) (:id props))]]
                    ($ component/image {:key thumb :hash thumb}
                      (fn [url]
                        ($ :label
                          {:style {:background-image (str "url(" url ")")}}
                          ($ :input
                            {:type "radio"
                             :name "scene-image-editor"
                             :value (:db/id scene)
                             :checked check
                             :on-change
                             (fn [event]
                               ((:on-change props) (js/Number (.. event -target -value))))}))))))
                (if (> pages 1)
                  ($ component/pagination
                    {:name "scenes-editor"
                     :label "Scene editor images pages"
                     :class-name "dark"
                     :pages pages
                     :value page
                     :on-change on-change})))))
          ($ :button.token-editor-button
            {:on-click (:on-close props)}
            "Exit"))))))

(defui ^:memo panel []
  (let [[preview set-preview] (uix/use-state nil)
        dispatch (hooks/use-dispatch)
        upload   (hooks/use-image-uploader {:type :scene})
        input    (uix/use-ref)
        data     (hooks/use-query query [:db/ident :root])
        {{{scene :camera/scene} :user/camera
          camera :user/camera} :root/user} data]
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
      ($ :fieldset.fieldset
        ($ :legend "Background image")
        ($ component/paginated
          {:data (:root/scene-images data) :page-size 6}
          (fn [{:keys [data pages page on-change]}]
            (let [data (->> (repeat per-page :placeholder) (into data) (take per-page) (map-indexed vector))]
              ($ :<>
                ($ :.scene-gallery
                  (for [[idx data] data
                        :let [hash (:image/hash data)
                              curr (:image/hash (:scene/image scene))]]
                    (if-let [thumbnail (:image/hash (:image/thumbnail data))]
                      ($ component/image {:key idx :hash thumbnail}
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
                                 (set-preview (:db/id data)))}
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
                                   (set-preview (:db/id data)))}
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
                  (if (> pages 1)
                    ($ component/pagination
                      {:name "scenes-gallery"
                       :label "Scene image pages"
                       :pages pages
                       :value page
                       :on-change on-change})))))))
        (if (some? preview)
          ($ scene-editor
            {:id preview
             :on-change set-preview
             :on-close (fn [] (set-preview nil))})))
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
               :checked (:scene/show-object-outlines scene)
               :on-change #(dispatch :scene/toggle-object-outlines (.. % -target -checked))})
            ($ icon {:name "check" :size 20})
            "Show shape outlines")
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
