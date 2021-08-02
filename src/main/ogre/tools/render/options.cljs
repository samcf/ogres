(ns ogre.tools.render.options
  (:import goog.crypt.Md5)
  (:require [datascript.core :as ds]
            [rum.core :as rum]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [css context use-image]]
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
             image (new js/Image)
             url   (.createObjectURL js/URL file)]
         (.addEventListener
          image "load"
          (fn []
            (this-as img (handler {:data data :filename (.-name file) :url url :img img}))))
         (set! (.-src image) url))))))

(rum/defc board-thumbnail [props]
  (let [{:keys [board]} props
        url             (use-image board)]
    [:div {:key      (:map/id board)
           :class    (css {:selected (:selected props)})
           :style    {:background-image (str "url(" url ")")}
           :on-click #((:on-select props))}
     [:div.name (:map/name board)]
     [:div.close
      {:on-click #((:on-remove props))} "×"]]))

(defmulti form (fn [{:keys [element]}] (:element/type element)))

(defmethod form :workspace [{:keys [context element]}]
  (let [{:keys [data dispatch store]} context]
    [:<>
     [:section.header
      [:label "Workspace Options"]
      [:button.close {:type "button" :on-click #(dispatch :view/toggle (:db/id element))} "×"]]
     [:section
      [:label
       [:input
        {:type "text"
         :placeholder "Workspace name"
         :autoFocus true
         :value (or (:element/name element) "")
         :on-change
         (fn [event]
           (let [value (.. event -target -value)]
             (dispatch :element/update (:db/id element) :element/name value)))}]]]

     (let [boards (query/boards data)]
       [:section
        (when (seq boards)
          [:div
           [:label "Select an existing map"]
           [:div.boards
            (for [board boards]
              (rum/with-key
                (board-thumbnail
                 {:board board
                  :on-select
                  (fn []
                    (dispatch :workspace/change-map (:db/id board)))
                  :on-remove
                  (fn []
                    (.delete (.-images store) (:map/id board))
                    (dispatch :map/remove (:db/id board)))}) (:map/id board)))]])
        [:input
         {:type "file"
          :accept "image/*"
          :multiple true
          :on-change
          #(doseq [file (.. % -target -files)]
             (load-image
              file
              (fn [{:keys [data filename url img]}]
                (let [checks (checksum data)
                      record #js {:checksum checks :data data :created-at (.now js/Date)}
                      entity {:map/id     checks
                              :map/name   filename
                              :map/url    url
                              :map/width  (.-width img)
                              :map/height (.-height img)}]
                  (-> (.put (.-images store) record)
                      (.then
                       (fn [] (dispatch :map/create element entity))))))))}]])]))

(defmethod form :token [{:keys [context element]}]
  (let [{:keys [data dispatch]} context]
    [:<>
     [:section.header
      [:label "Token Options"]
      [:button.close {:type "button" :on-click #(dispatch :view/toggle (:db/id element))} "×"]]
     [:section
      [:fieldset
       [:label
        [:input {:type "text"
                 :value (or (:element/name element) "")
                 :placeholder "Label"
                 :maxLength 24
                 :autoFocus true
                 :on-change (fn [event]
                              (let [value (.. event -target -value)]
                                (dispatch :element/update (:db/id element) :element/name value)))}]]]]]))

(rum/defc options [{:keys [element]}]
  (rum/with-context [value context]
    [:div {:class (styles)}
     (form {:context value :element element})]))
