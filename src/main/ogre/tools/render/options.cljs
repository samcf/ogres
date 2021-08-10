(ns ogre.tools.render.options
  (:import goog.crypt.Md5)
  (:require [clojure.string :as string]
            [uix.core.alpha :as uix]
            [datascript.core :as ds]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [css context handler use-image]]
            [ogre.tools.query :as query]))

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

(defclass styles []
  {:background-color "#F2F2EB"
   :border           "1px solid black"
   :border-radius    "3px"
   :color            "#461B0E"
   :display          "flex"
   :flex-direction   "column"
   :font-size        "14px"
   :line-height      "1.3"
   :margin           "16px"
   :max-height       "100%"
   :max-width        "360px"
   :overflow-y       "auto"
   :padding          "8px"
   :pointer-events   "all"}
  [:section+section
   {:margin-top "8px"}]
  [:.header
   {:display         "flex"
    :justify-content "space-between"}]
  [:header
   {:border-bottom "1px solid hsl(0, 10%, 55%)"
    :font-weight   "bold"
    :margin-bottom "4px"}]
  [:.boards
   {:display               "grid"
    :grid-gap              "2px"
    :grid-template-columns "repeat(3, 1fr)"
    :margin-bottom         "4px"
    :max-height            "180px"
    :overflow-y            "auto"
    :font-size             "12px"}
   [:>div
    {:background-size "cover"
     :cursor          "pointer"
     :display         "flex"
     :border-radius   "4px"
     :box-sizing      "border-box"
     :flex-direction  "column"
     :justify-content "flex-end"
     :position        "relative"
     :height          "88px"}
    [:&.selected>.name :&:hover>.name
     {:background-color "rgba(0, 0, 0, 1)"
      :color            "rgba(255, 255, 255)"}]
    [:.close
     {:border-radius "0 0 0 4px"
      :color         "rgba(255, 255, 255)"
      :position      "absolute"
      :top           0
      :right         0
      :padding       "4px 8px"}
     [:&:hover
      {:background-color "rgba(0, 0, 0, 0.70)"}]]]]
  [:.lighting
   {:display               "grid"
    :grid-template-columns "repeat(3, 1fr)"
    :grid-gap              "4px"}
   [:button
    {:text-transform "capitalize"}]]
  [:.lights
   {:display               "grid"
    :grid-template-columns "repeat(4, 1fr)"
    :grid-gap              "4px"}]
  [:.sizes
   {:display               "grid"
    :grid-template-columns "repeat(3, 1fr)"
    :grid-gap              "4px"}
   [:button
    {:text-transform "capitalize"}]])

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
     [:div.close
      {:on-click (handler on-remove)} "×"]]))

(defn board [{:keys [workspace]}]
  (let [{:keys [data workspace dispatch store]} (uix/context context)]
    [:div {:class (styles)}
     [:section.header
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
      [:button.close
       {:type "button" :on-click #(dispatch :canvas/toggle-canvas-options)} "×"]]

     [:section
      (when-let [boards (query/boards data)]
        [:div
         [:div.boards
          (for [board boards]
            [thumbnail
             {:key       (:image/checksum board)
              :board     board
              :selected  (= board (:canvas/map workspace))
              :on-select (fn []
                           (dispatch :canvas/change-map (:db/id board)))
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
       [:div.lighting
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
    [:div {:class (styles)}
     [:section.header
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
      [:button.remove {:type "button" :on-click #(dispatch :token/remove id) :style {:margin-right "8px"}} "♼"]
      [:button.close {:type "button" :on-click #(dispatch :view/toggle id)} "×"]]
     [:section
      [:header "Light"]
      [:div.lights
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
      [:div.sizes
       (for [[name size] [[:tiny 2.5] [:small 5] [:medium 5] [:large 10] [:huge 15] [:gargantuan 20]]
             :let [checked (= name (:name (:token/size token)))]]
         [radio-button
          {:key name
           :name "token/size"
           :value name
           :checked checked
           :on-change #(dispatch :token/change-size id name size)}
          (string/capitalize (clojure.core/name name))])]]

     (let [{:keys [aura/label aura/radius aura/color]} (:canvas/selected workspace)]
       [:section
        [:header "Aura"]
        [:div
         [:input
          {:type "text"
           :placeholder "Label"
           :value (or label "")
           :on-change #(dispatch :aura/change-label id (.. % -target -value))}]]
        [:div {:style {:display "grid" :grid-template-columns "repeat(5, 1fr)" :grid-gap "4px" :margin-top "4px"}}
         (for [radius [0 10 20 30 60] :let [checked (= radius (:aura/radius token))]]
           [radio-button
            {:key radius
             :name "token/aura-radius"
             :checked checked
             :value radius
             :on-change #(dispatch :aura/change-radius id radius)}
            (if (= radius 0) "None" (str radius " ft."))])]])]))

(defn grid [{:keys [workspace]}]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [grid/size]} workspace]
    [:div {:class (styles)}
     [:section.header
      [:input
       {:type "number"
        :value (or size 0)
        :style {:flex 1 :margin-right "8px"}
        :min 0
        :on-change
        (fn [event]
          (let [value (.. event -target -value)]
            (dispatch :grid/change-size value)))}]
      [:button.close
       {:type "button" :on-click #(dispatch :canvas/toggle-grid-options :select)} "×"]]
     [:section
      [:header "Setting the Grid Size"]
      [:div "Draw a square that represents 5 feet. We'll try to guess what
             the real grid size is based on your selection and the size of
             the map. If its not quite right, edit the size above manually."]]]))

(defn options []
  (let [{:keys [workspace]} (uix/context context)
        {:keys [canvas/mode canvas/selected]} workspace]
    (case [mode (:element/type selected)]
      [:board :canvas]
      [board {:key (:db/id workspace)}]

      [:select :token]
      [token {:key (:db/id selected)}]

      [:grid nil]
      [grid {:key (:db/id workspace)}]

      nil)))
