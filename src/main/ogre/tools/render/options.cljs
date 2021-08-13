(ns ogre.tools.render.options
  (:import goog.crypt.Md5)
  (:require [clojure.string :as string]
            [uix.core.alpha :as uix]
            [datascript.core :as ds]
            [ogre.tools.render :refer [css context handler use-image]]
            [ogre.tools.render.pattern :refer [pattern]]
            [ogre.tools.query :as query]))

(def colors
  ["#182125" "#f2f2f2" "#f44336" "#e91e63"
   "#9c27b0" "#673ab7" "#3f51b5" "#2196f3"
   "#00bcd4" "#009688" "#4caf50" "#cddc39"
   "#ffeb3b" "#ffc107" "#ff9800"])

(defn checksum [data]
  (let [hash (new Md5)]
    (.update hash data)
    (reduce
     (fn [s b]
       (str s (.slice (str "0" (.toString b 16)) -2))) "" (.digest hash))))

(defn radio-button [props child]
  (let [id (str (:name props) ":" (:value props))]
    [:div
     [:input (merge props {:id id :type "radio" :class "ogre-radio-button"})]
     [:label {:for id} child]]))

(defn load-image [file handler]
  (let [reader (new js/FileReader)]
    (.readAsDataURL reader file)
    (.addEventListener
     reader "load"
     (fn [event]
       (let [data  (.. event -target -result)
             image (new js/Image)]
         (.addEventListener
          image "load"
          (fn []
            (this-as img (handler {:data data :filename (.-name file) :img img}))))
         (set! (.-src image) data))))))

(defn thumbnail [{:keys [board selected on-select on-remove]}]
  (let [url (use-image (:image/checksum board))]
    [:div {:key      (:image/checksum board)
           :class    (css {:selected selected})
           :style    {:background-image (str "url(" url ")")}
           :on-click (handler on-select)}
     [:div
      {:on-click (handler on-remove)} "×"]]))

(defn canvas []
  (let [{:keys [data workspace dispatch store]} (uix/context context)]
    [:div.options.options-canvas
     [:section
      [:label {:style {:flex 1 :margin-right "8px"}}
       [:input
        {:type "text"
         :placeholder "Workspace name"
         :autoFocus true
         :value (or (:element/name workspace) "")
         :on-change
         (fn [event]
           (let [value (.. event -target -value)]
             (dispatch :element/update (:db/id workspace) :element/name value)))}]]
      [:button
       {:type "button" :on-click #(dispatch :canvas/toggle-mode :canvas)} "×"]]

     [:section
      (when-let [boards (query/boards data)]
        [:div
         [:div.options-canvas-maps
          (for [board boards]
            [thumbnail
             {:key       (:image/checksum board)
              :board     board
              :selected  (= board (:canvas/map workspace))
              :on-select (fn [] (dispatch :canvas/change-map (:db/id board)))
              :on-remove (fn []
                           (.delete (.-images store) (:image/checksum board))
                           (dispatch :map/remove (:db/id board)))}])]])
      [:input
       {:type "file"
        :accept "image/*"
        :multiple true
        :on-change
        #(doseq [file (.. % -target -files)]
           (load-image
            file
            (fn [{:keys [data filename img]}]
              (let [checks (checksum data)
                    record #js {:checksum checks :data data :created-at (.now js/Date)}
                    entity {:image/checksum checks
                            :image/name     filename
                            :image/width    (.-width img)
                            :image/height   (.-height img)}]
                (-> (.put (.-images store) record)
                    (.then
                     (fn [] (dispatch :map/create workspace entity))))))))}]]

     [:section
      [:fieldset
       [:header "Lighting"]
       [:div.options-canvas-lighting
        (for [option [:bright :dim :dark]
              :let [checked (= option (:canvas/lighting workspace))]]
          [radio-button
           {:key option
            :name "canvas/lighting"
            :value option
            :checked checked
            :on-change #(dispatch :canvas/change-lighting option)}
           (string/capitalize (name option))])]]]]))

(defn token [props]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [db/id element/name] :as token} (:canvas/selected workspace)]
    (when token
      [:div.options.options-token {:key id}
       [:section
        [:input
         {:type "text"
          :style {:flex 1 :margin-right "8px"}
          :value (or name "")
          :placeholder "Label"
          :maxLength 24
          :autoFocus true
          :on-change
          (fn [event]
            (let [value (.. event -target -value)]
              (dispatch :element/update id :element/name value)))}]
        [:button {:type "button" :on-click #(dispatch :element/remove id) :style {:margin-right "8px"}} "♼"]
        [:button {:type "button" :on-click #(dispatch :element/select id)} "×"]]
       [:section
        [:header "Light"]
        [:div.options-token-lights
         (for [[bright dim] [[0 0] [5 5] [10 10] [15 30] [20 20] [30 30] [40 40] [60 60]]
               :let [checked (= [bright dim] (:token/light token))
                     key (str bright ":" dim)]]
           [radio-button
            {:key key
             :name "token/light"
             :value key
             :checked checked
             :on-change #(dispatch :token/change-light id bright dim)}
            (str bright " ft. / " dim " ft.")])]]
       [:section
        [:header "Size"]
        [:div.options-token-sizes
         (for [[name size] [[:tiny 2.5] [:small 5] [:medium 5] [:large 10] [:huge 15] [:gargantuan 20]]
               :let [checked (= name (:name (:token/size token)))]]
           [radio-button
            {:key name
             :name "token/size"
             :value name
             :checked checked
             :on-change #(dispatch :token/change-size id name size)}
            (string/capitalize (clojure.core/name name))])]]

       (let [{:keys [aura/label aura/radius aura/color]} token]
         [:section
          [:header "Aura"]
          [:div
           [:input
            {:type "text"
             :placeholder "Label"
             :value (or label "")
             :on-change #(dispatch :aura/change-label id (.. % -target -value))}]]
          [:div.options-token-auras
           (for [radius [0 10 15 20 30 60] :let [checked (= radius (:aura/radius token))]]
             [radio-button
              {:key radius
               :name "token/aura-radius"
               :checked checked
               :value radius
               :on-change #(dispatch :aura/change-radius id radius)}
              (if (= radius 0) "None" (str radius " ft."))])]])])))

(defn shape [props]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [db/id element/name] :as shape} (:canvas/selected workspace)]
    [:div.options.options-shape {:key id}
     [:section
      [:input
       {:type "text"
        :value (or name "")
        :placeholder "Label"
        :auto-focus true
        :style {:flex 1 :margin-right "8px"}
        :on-change
        (fn [event]
          (let [value (.. event -target -value)]
            (dispatch :element/update id :element/name value)))}]
      [:button {:type "button" :style {:margin-right "8px"} :on-click #(dispatch :element/remove id)} "♼"]
      [:button {:type "button" :on-click #(dispatch :element/select id)} "×"]]
     [:section
      [:header "Color"]
      [:div.options-shape-colors
       (for [color colors]
         [radio-button
          {:key color
           :name "shape/color"
           :value color
           :checked (= color (:shape/color shape))
           :on-change
           (fn []
             (dispatch :element/update id :shape/color color))}
          [:div {:style {:background-color color}}]])]]
     [:section
      [:header "Pattern"]
      [:div.options-shape-patterns
       (for [name [:solid :lines :circles :crosses :caps :waves]]
         [radio-button
          {:key name
           :name "shape/pattern"
           :value name
           :checked (= name (:shape/pattern shape))
           :on-change
           (fn []
             (dispatch :element/update id :shape/pattern name))}
          (let [id (str "template-pattern-" (clojure.core/name name))]
            [:svg {:width "100%" :height "32px"}
             [:defs [pattern {:id id :name name}]]
             [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill (str "url(#" id ")")}]])])]]
     [:section
      [:header "Opacity"]
      [:input
       {:type "range"
        :style {:width "100%"}
        :value (or (:shape/opacity shape) 0)
        :on-change
        (fn [event]
          (let [value (.. event -target -value)]
            (dispatch :element/update id :shape/opacity value)))
        :min 0
        :max 1
        :step 0.25}]]]))

(defn grid [{:keys [workspace]}]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [grid/size]} workspace]
    [:div.options.options-grid
     [:section
      [:input
       {:type "number"
        :value (or size 0)
        :style {:flex 1 :margin-right "8px"}
        :min 0
        :on-change
        (fn [event]
          (let [value (.. event -target -value)]
            (dispatch :grid/change-size value)))}]
      [:button
       {:type "button" :on-click #(dispatch :canvas/toggle-mode :grid)} "×"]]
     [:section
      [:header "Setting the Grid Size"]
      [:div "Draw a square that represents 5 feet. We'll try to guess what
             the real grid size is based on your selection and the size of
             the map. If its not quite right, edit the size above manually."]]]))

(defn options []
  (let [{:keys [workspace]} (uix/context context)
        {:keys [canvas/mode canvas/selected]} workspace]
    (cond
      (= mode :canvas) [canvas]
      (= mode :grid)   [grid]
      (= mode :select)
      (case (:element/type selected)
        :token [token]
        :shape [shape]
        nil)
      :default nil)))
