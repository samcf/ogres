(ns ogre.tools.form.canvas
  (:require [clojure.string :refer [capitalize]]
            [ogre.tools.form.render :refer [form]]
            [ogre.tools.image :as image]
            [ogre.tools.render :refer [button checkbox icon use-image]]
            [ogre.tools.state :refer [use-dispatch use-query]]
            [ogre.tools.storage :refer [use-store]]
            [uix.core.alpha :as uix]))

(defn ^{:private true} thumbnail [{:keys [checksum selected on-select on-remove]}]
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

(defn ^{:private true} preview [{:keys [checksum]}]
  (let [url (use-image checksum)]
    [:div {:style {:background-image (str "url(" url ")")}}]))

(def ^{:private true} query
  [{:root/scenes [:image/checksum]}
   {:root/local
    [{:local/window
      [[:window/label :default ""]
       [:window/draw-mode :default :select]
       {:window/canvas
        [:entity/key
         {:canvas/image [:image/checksum]}
         [:canvas/theme :default :light]
         [:canvas/visibility :default :revealed]
         [:canvas/color :default :none]
         [:grid/size :default 70]]}]}]}])

(defn ^{:private true} canvas []
  (let [dispatch    (use-dispatch)
        result      (use-query query [:db/ident :root])
        store       (use-store)
        show-images (uix/state false)
        file-upload (uix/ref)
        {scenes :root/scenes
         {window :local/window
          {canvas :window/canvas} :local/window} :root/local} result
        checksum (-> canvas :canvas/image :image/checksum)]
    [:<>
     [:section
      [:header "Canvas Options"]
      [:fieldset.form-canvas-profile
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
         :value (:window/label window)
         :on-change
         (fn [event]
           (let [value (.. event -target -value)]
             (dispatch :window/change-label value)))}]]]
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
       (for [value [:light :dark] :let [checked? (= value (:canvas/theme canvas))]]
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
       (for [value [:revealed :dimmed :hidden] :let [checked (= value (:canvas/visibility canvas))]]
         ^{:key value}
         [checkbox
          {:checked checked :on-change #(dispatch :canvas/change-visibility value)}
          (capitalize (name value))])]
      [:fieldset.setting
       [:label "Filter"]
       (for [value [:none :dusk :midnight] :let [checked? (= value (:canvas/color canvas))]]
         ^{:key value}
         [checkbox
          {:checked checked? :on-change #(dispatch :canvas/change-color value)}
          (capitalize (name value))])]]
     [:section.form-canvas-grid
      [:legend "Grid Configuration"]
      [:fieldset.group
       [:input
        {:type "number"
         :placeholder "Grid size"
         :value (or (:grid/size canvas) 0)
         :on-change
         (fn [event]
           (dispatch :canvas/change-grid-size (.. event -target -value)))}]
       [checkbox
        {:checked (= (:window/draw-mode window) :grid)
         :on-change
         (fn [checked]
           (if checked
             (dispatch :window/change-mode :grid)
             (dispatch :window/change-mode :select)))}
        [icon {:name "crop" :size 16}]]]
      [:p {:style {:margin-top 4}}
       "Manually enter the grid size or click the button to draw a square
        that represents 5ft. on the map so the application knows how to
        measure distance and how big to make tokens."]]]))

(defmethod form :canvas []
  canvas)
