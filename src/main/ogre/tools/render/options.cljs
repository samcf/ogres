(ns ogre.tools.render.options
  (:import goog.crypt.Md5)
  (:require [uix.core.alpha :as uix]
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

(defclass styles []
  {:background-color "#F2F2EB"
   :border           "1px solid black"
   :border-radius    "3px"
   :color            "#461B0E"
   :display          "flex"
   :flex-direction   "column"
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
  [:.boards
   {:display               "grid"
    :grid-gap              "2px"
    :grid-template-columns "repeat(3, 1fr)"
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
      :color    "rgba(255, 255, 255)"
      :position "absolute"
      :top      0
      :right    0
      :padding  "4px 8px"}
     [:&:hover
      {:background-color "rgba(0, 0, 0, 0.70)"}]]
    [:.name
     {:background-color "rgba(0, 0, 0, 0.20)"
      :border-radius    "0 0 4px 4px"
      :color            "rgba(255, 255, 255, 0.80)"
      :max-height       "44px"
      :overflow-y       "hidden"
      :padding          "0 4px"
      :pointer-events   "none"}]]])

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
     [:div.name (:image/name board)]
     [:div.close
      {:on-click (handler on-remove)} "×"]]))

(defn board [{:keys [workspace]}]
  (let [{:keys [data workspace dispatch store]} (uix/context context)]
    [:div {:class (styles)}
     [:section.header
      [:label "Workspace Options"]
      [:button.close
       {:type "button"
        :on-click
        #(dispatch :workspace/toggle-board-options)} "×"]]
     [:section
      [:label
       [:input
        {:type "text"
         :placeholder "Workspace name"
         :autoFocus true
         :value (or (:element/name workspace) "")
         :on-change
         (fn [event]
           (let [value (.. event -target -value)]
             (dispatch :element/update (:db/id workspace) :element/name value)))}]]]

     (let [boards (query/boards data)]
       [:section
        (when (seq boards)
          [:div
           [:label "Select an existing map"]
           [:div.boards
            (for [board boards]
              [thumbnail
               {:key       (:image/checksum board)
                :board     board
                :selected  (= board (:workspace/map workspace))
                :on-select (fn []
                             (dispatch :workspace/change-map (:db/id board)))
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
                       (fn [] (dispatch :map/create workspace entity))))))))}]])]))

(defn token [{:keys [token]}]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [db/id element/name]} (:workspace/selected workspace)]
    [:div {:class (styles)}
     [:section.header
      [:label "Token Options"]
      [:button.close {:type "button" :on-click #(dispatch :view/toggle id)} "×"]]
     [:section
      [:fieldset
       [:label
        [:input
         {:type "text"
          :value (or name "")
          :placeholder "Label"
          :maxLength 24
          :autoFocus true
          :on-change
          (fn [event]
            (let [value (.. event -target -value)]
              (dispatch :element/update id :element/name value)))}]]]]]))

(defn grid [{:keys [workspace]}]
  (let [{:keys [workspace dispatch]} (uix/context context)
        {:keys [grid/size]} workspace]
    [:div {:class (styles)}
     [:section.header
      [:label "Grid Options"]
      [:button.close
       {:type "button"
        :on-click
        #(dispatch :workspace/toggle-grid-options :select)} "×"]]
     [:section
      [:label
       [:div "Size"]
       [:input
        {:type "number"
         :value (or size 0)
         :min 0
         :on-change
         (fn [event]
           (let [value (.. event -target -value)]
             (dispatch :grid/change-size value)))}]]]]))

(defn options []
  (let [{:keys [workspace]} (uix/context context)
        {:keys [workspace/mode workspace/selected]} workspace]
    (case [mode (:element/type selected)]
      [:select :workspace]
      [board {:key (:db/id workspace)}]

      [:select :token]
      [token {:key (:db/id selected)}]

      [:grid nil]
      [grid {:key (:db/id workspace)}]

      nil)))
