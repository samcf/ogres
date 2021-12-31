(ns ogre.tools.form.canvas
  (:require [clojure.string :refer [capitalize]]
            [ogre.tools.form.render :refer [form]]
            [ogre.tools.image :as image]
            [ogre.tools.render :refer [button checkbox icon use-image]]
            [ogre.tools.state :refer [use-query]]
            [ogre.tools.storage :refer [storage]]
            [uix.core.alpha :as uix]))

(def query
  {:pull
   [{:root/scenes [:db/id :image/checksum]}
    {:root/canvas
     [:db/id
      :element/name
      {:canvas/scene
       [:image/checksum]}
      [:canvas/theme :default :light]
      [:canvas/lighting :default :bright]
      [:canvas/color :default :none]
      [:canvas/mode :default :select]
      [:grid/show :default true]
      [:grid/size :default 70]
      [:grid/align :default false]]}]})

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

(defn canvas []
  (let [[result dispatch] (use-query query)
        {:keys [store]}   (uix/context storage)
        show-images       (uix/state false)
        file-upload       (uix/ref)
        {canvas :root/canvas
         scenes :root/scenes} result]
    [:<>
     [:section
      [:header "Canvas Options"]]
     [:section.form-canvas-profile
      [:button
       {:type "button"
        :on-click #(swap! show-images not)
        :disabled (not (seq scenes))}
       (if (:canvas/scene canvas)
         [preview {:checksum (-> canvas :canvas/scene :image/checksum)}]
         [icon {:name "images" :size 32}])]
      [button {:on-click #(.click @file-upload)} "Choose File(s)"]
      [:input
       {:type "text"
        :placeholder "New Canvas"
        :maxLength 36
        :spellCheck "false"
        :value (or (:element/name canvas) "")
        :on-change
        (fn [event]
          (let [value (.. event -target -value)]
            (dispatch :element/update [(:db/id canvas)] :element/name value)))}]]
     [:section
      (if (and (seq scenes) @show-images)
        [:fieldset.thumbnails
         (for [{:keys [db/id image/checksum]} scenes]
           ^{:key checksum}
           [thumbnail
            {:checksum  checksum
             :selected  (= id (:db/id (:canvas/scene canvas)))
             :on-select (fn [] (dispatch :canvas/change-scene id))
             :on-remove (fn []
                          (.delete (.-images store) checksum)
                          (dispatch :map/remove id))}])])
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
                (fn [{:keys [data filename element]}]
                  (let [checks (image/checksum data)
                        record #js {:checksum checks :data data :created-at (.now js/Date)}
                        entity {:image/checksum checks
                                :image/name     filename
                                :image/width    (.-width element)
                                :image/height   (.-height element)}]
                    (-> (.put (.-images store) record)
                        (.then
                         (fn [] (dispatch :scene/create entity)))))))))}]]
     [:section
      [:legend "Options"]
      [:fieldset.setting
       [:label "Theme"]
       (for [theme [:light :dark] :let [checked? (= theme (:canvas/theme canvas))]]
         ^{:key theme}
         [checkbox
          {:checked checked?
           :on-change
           (fn []
             (when (not checked?)
               (dispatch :canvas/toggle-theme)))}
          (capitalize (name theme))])]
      [:fieldset.setting
       [:label "Lighting"]
       (for [option [:bright :dim :dark] :let [checked (= option (:canvas/lighting canvas))]]
         ^{:key option}
         [checkbox
          {:checked checked
           :on-change #(dispatch :canvas/change-lighting option)}
          (capitalize (name option))])]
      [:fieldset.setting
       [:label "Filter"]
       (for [color [:none :dusk :midnight]
             :let  [current  (or (:canvas/color canvas) :none)
                    checked? (= color current)]]
         ^{:key color}
         [checkbox
          {:checked checked?
           :on-change #(dispatch :canvas/change-color color)}
          (capitalize (name color))])]]
     [:section.form-canvas-grid
      [:legend "Grid Configuration"]
      [:fieldset.setting
       [:label "Show Grid"]
       (for [value [false true] :let [checked? (= (:grid/show canvas) value)]]
         ^{:key value}
         [checkbox
          {:checked checked?
           :on-change
           (fn [] (if (not checked?) (dispatch :grid/toggle)))}
          (if value "Yes" "No")])]
      [:fieldset.setting
       [:label "Align to Grid"]
       (for [value [false true] :let [checked? (= (:grid/align canvas) value)]]
         ^{:key value}
         [checkbox
          {:checked checked? :on-change (fn [] (if (not checked?) (dispatch :grid/align)))}
          (if value "Yes" "No")])]
      [:hr]
      [:fieldset.group
       [:input
        {:type "number"
         :placeholder "Grid size"
         :value (or (:grid/size canvas) 0)
         :on-change
         (fn [event]
           (dispatch :grid/change-size (.. event -target -value)))}]
       [checkbox
        {:checked (= (:canvas/mode canvas) :grid)
         :on-change
         (fn [checked]
           (if checked
             (dispatch :canvas/toggle-mode :grid)
             (dispatch :canvas/toggle-mode :select)))}
        [icon {:name "crop" :size 16}]]]
      [:p {:style {:margin-top 4}}
       "Manually enter the grid size or click the button to draw a square
        that represents 5ft. on the map so the application knows how to
        measure distance and how big to make tokens."]]]))

(defmethod form :canvas []
  canvas)
