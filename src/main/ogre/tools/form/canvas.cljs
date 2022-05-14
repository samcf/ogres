(ns ogre.tools.form.canvas
  (:require [clojure.string :refer [capitalize]]
            [ogre.tools.form.render :refer [form]]
            [ogre.tools.image :as image]
            [ogre.tools.render :refer [button checkbox icon use-image]]
            [ogre.tools.state :refer [use-query]]
            [ogre.tools.storage :refer [storage]]
            [uix.core.alpha :as uix]))

(defn thumbnail [{:keys [checksum selected on-select on-remove]}]
  (let [url (use-image checksum)]
    [:div
     {:key checksum
      :css {:selected selected}
      :style {:background-image (str "url(" url ")")}
      :on-click
      (fn [event]
        (.stopPropagation event)
        (on-select))}
     [:div
      {:on-click
       (fn [event]
         (.stopPropagation event)
         (on-remove))} "×"]]))

(defn preview [{:keys [checksum]}]
  (let [url (use-image checksum)]
    [:div {:style {:background-image (str "url(" url ")")}}]))

(def query
  [{:root/scenes [:image/checksum]}
   {:root/local
    [{:local/window
      [[:window/mode :default :select]
       [:grid/show :default true]
       [:grid/align :default false]
       {:window/canvas
        [:entity/key
         [:element/name :default ""]
         {:canvas/scene [:image/checksum]}
         [:canvas/theme :default :light]
         [:canvas/visibility :default :revealed]
         [:canvas/color :default :none]
         [:grid/size :default 70]]}]}]}])

(defn canvas []
  (let [[result dispatch] (use-query query [:db/ident :root])
        {:keys [store]}   (uix/context storage)
        show-images       (uix/state false)
        file-upload       (uix/ref)
        {scenes :root/scenes
         {{show-grid   :grid/show
           align-grid  :grid/align
           mode        :window/mode
           {key        :entity/key
            label      :element/name
            theme      :canvas/theme
            visibility :canvas/visibility
            color      :canvas/color
            grid-size  :grid/size
            {checksum  :image/checksum}
            :canvas/scene}
           :window/canvas}
          :local/window}
         :root/local} result]
    [:<>
     [:section
      [:header "Canvas Options"]]
     [:section.form-canvas-profile
      [:button
       {:type "button"
        :on-click #(swap! show-images not)
        :disabled (not (seq scenes))}
       (if checksum
         [preview {:checksum checksum}]
         [icon {:name "images" :size 32}])]
      [button {:on-click #(.click @file-upload)} "Choose File(s)"]
      [:input
       {:type "text"
        :placeholder "New Canvas"
        :maxLength 36
        :spellCheck "false"
        :value label
        :on-change
        (fn [event]
          (let [value (.. event -target -value)]
            (dispatch :element/update [key] :element/name value)))}]]
     [:section
      (if (and (seq scenes) @show-images)
        [:fieldset.thumbnails
         (for [scene scenes :let [value (:image/checksum scene)]]
           ^{:key value}
           [thumbnail
            {:checksum  value
             :selected  (= value checksum)
             :on-select (fn [] (dispatch :canvas/change-scene value))
             :on-remove (fn []
                          (.delete (.-images store) value)
                          (dispatch :map/remove value))}])])
      [:input
       {:type "file"
        :ref file-upload
        :accept "image/*"
        :multiple true
        :style {:display "none"}
        :on-change
        #(doseq [file (.. % -target -files)]
           (-> (image/load file)
               (.then
                (fn [[file data-url element]]
                  (let [checks (image/checksum data-url)
                        record #js {:checksum checks :data data-url :created-at (.now js/Date)}
                        args   [checks (.-name file) (.-width element) (.-height element)]]
                    (-> (.put (.-images store) record)
                        (.then
                         (fn [] (apply dispatch :scene/create args)))))))))}]]
     [:section
      [:legend "Options"]
      [:fieldset.setting
       [:label "Theme"]
       (for [value [:light :dark] :let [checked? (= value theme)]]
         ^{:key value}
         [checkbox
          {:checked checked?
           :on-change
           (fn []
             (if (not checked?)
               (dispatch :canvas/change-theme value)))}
          (capitalize (name value))])]
      [:fieldset.setting
       [:label "Visibility"]
       (for [value [:revealed :dimmed :hidden] :let [checked (= value visibility)]]
         ^{:key value}
         [checkbox
          {:checked checked :on-change #(dispatch :canvas/change-visibility value)}
          (capitalize (name value))])]
      [:fieldset.setting
       [:label "Filter"]
       (for [value [:none :dusk :midnight] :let [checked? (= value color)]]
         ^{:key value}
         [checkbox
          {:checked checked? :on-change #(dispatch :canvas/change-color value)}
          (capitalize (name value))])]]
     [:section.form-canvas-grid
      [:legend "Grid Configuration"]
      [:fieldset.setting
       [:label "Show Grid"]
       (for [value [false true] :let [checked? (= show-grid value)]]
         ^{:key value}
         [checkbox
          {:checked checked?
           :on-change
           (fn []
             (if (not checked?)
               (dispatch :grid/toggle value)))}
          (if value "Yes" "No")])]
      [:fieldset.setting
       [:label "Align to Grid"]
       (for [value [false true] :let [checked? (= align-grid value)]]
         ^{:key value}
         [checkbox
          {:checked checked?
           :on-change
           (fn []
             (if (not checked?)
               (dispatch :grid/align value)))}
          (if value "Yes" "No")])]
      [:hr]
      [:fieldset.group
       [:input
        {:type "number"
         :placeholder "Grid size"
         :value (or grid-size 0)
         :on-change
         (fn [event]
           (dispatch :grid/change-size (.. event -target -value)))}]
       [checkbox
        {:checked (= mode :grid)
         :on-change
         (fn [checked]
           (if checked
             (dispatch :canvas/change-mode :grid)
             (dispatch :canvas/change-mode :select)))}
        [icon {:name "crop" :size 16}]]]
      [:p {:style {:margin-top 4}}
       "Manually enter the grid size or click the button to draw a square
        that represents 5ft. on the map so the application knows how to
        measure distance and how big to make tokens."]]]))

(defmethod form :canvas []
  canvas)
