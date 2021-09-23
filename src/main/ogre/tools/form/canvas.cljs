(ns ogre.tools.form.canvas
  (:require
   [clojure.string :refer [capitalize]]
   [ogre.tools.form.render :refer [form]]
   [ogre.tools.image :as image]
   [ogre.tools.query :as query]
   [ogre.tools.render :refer [css checkbox use-image]]
   [ogre.tools.render.icon :refer [icon]]
   [ogre.tools.state :refer [state]]
   [ogre.tools.storage :refer [storage]]
   [uix.core.alpha :as uix]))

(defn thumbnail [{:keys [board selected on-select on-remove]}]
  (let [checksum (:image/checksum board)
        url      (use-image checksum)]
    [:div
     {:key checksum
      :class (css {:selected selected})
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

(defmethod form :canvas [props]
  (let [{:keys [data workspace dispatch]} (uix/context state)
        {:keys [store]} (uix/context storage)]
    [:<>
     [:section
      [:header "Canvas Options"]]
     [:section
      [:input
       {:type "text"
        :placeholder "New canvas"
        :maxLength 36
        :spellCheck "false"
        :value (or (:element/name workspace) "")
        :on-change
        (fn [event]
          (let [value (.. event -target -value)]
            (dispatch :element/update [(:db/id workspace)] :element/name value)))}]]
     [:section
      [:div.thumbnails
       (for [board (query/boards data)]
         [thumbnail
          {:key       (:image/checksum board)
           :board     board
           :selected  (= board (:canvas/map workspace))
           :on-select (fn [] (dispatch :canvas/change-map (:db/id board)))
           :on-remove (fn []
                        (.delete (.-images store) (:image/checksum board))
                        (dispatch :map/remove (:db/id board)))}])]
      [:input
       {:type "file"
        :accept "image/*"
        :multiple true
        :on-change
        #(doseq [file (.. % -target -files)]
           (image/load
            file
            (fn [{:keys [data filename img]}]
              (let [checks (image/checksum data)
                    record #js {:checksum checks :data data :created-at (.now js/Date)}
                    entity {:image/checksum checks
                            :image/name     filename
                            :image/width    (.-width img)
                            :image/height   (.-height img)}]
                (-> (.put (.-images store) record)
                    (.then
                     (fn [] (dispatch :map/create workspace entity))))))))}]]
     [:section
      [:legend "Options"]
      [:fieldset.setting
       [:label "Show Grid"]
       (for [display? [true false] :let [checked? (= (:grid/show workspace) display?)]]
         ^{:key display?}
         [checkbox
          {:checked checked?
           :on-change
           (fn [] (if (not checked?) (dispatch :grid/toggle)))}
          (if display? "Yes" "No")])]
      [:fieldset.setting
       [:label "Theme"]
       (for [theme [:light :dark] :let [checked? (= theme (:canvas/theme workspace))]]
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
       (for [option [:bright :dim :dark] :let [checked (= option (:canvas/lighting workspace))]]
         ^{:key option}
         [checkbox
          {:checked checked
           :on-change #(dispatch :canvas/change-lighting option)}
          (capitalize (name option))])]
      [:fieldset.setting
       [:label "Filter"]
       (for [color [:none :dusk :midnight]
             :let  [current  (or (:canvas/color workspace) :none)
                    checked? (= color current)]]
         ^{:key color}
         [checkbox
          {:checked checked?
           :on-change #(dispatch :canvas/change-color color)}
          (capitalize (name color))])]]
     [:section
      [:legend "Grid Configuration"]
      [:fieldset.group
       [:input
        {:type "number"
         :placeholder "Grid size"
         :value (or (:grid/size workspace) 0)
         :on-change
         (fn [event]
           (dispatch :grid/change-size (.. event -target -value)))}]
       [checkbox
        {:checked (= (:canvas/mode workspace) :grid)
         :on-change
         (fn [checked]
           (if checked
             (dispatch :canvas/toggle-mode :grid)
             (dispatch :canvas/toggle-mode :select)))}
        [icon {:name :crop :size 16}]]]
      [:p {:style {:margin-top 4}}
       "Manually enter the grid size or click the button to draw a square
        that represents 5ft. on the map so the application knows how to
        measure distance and how big to make tokens."]]]))
