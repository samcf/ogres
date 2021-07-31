(ns ogre.tools.render.options
  (:require [datascript.core :as ds]
            [rum.core :as rum]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [context]]
            [ogre.tools.query :as query]))

(defclass styles []
  {:background-color "white"
   :border           "1px solid black"
   :border-radius    "3px"
   :color            "black"
   :display          "flex"
   :flex-direction   "column"
   :margin           "16px"
   :max-width        "320px"
   :padding          "8px"
   :pointer-events   "all"}
  [:.header {:display "flex" :justify-content "space-between"}]
  [:section+section {:margin-top "8px"}])

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

(rum/defc options [props & children]
  (let [{:keys [element]} props]
    (rum/with-context [{:keys [data dispatch store]} context]
      [:div {:class (styles)}
       (case (:element/type element)
         :workspace
         [:<>
          [:section.header
           [:label "Workspace Settings"]
           [:button.close {:type "button" :on-click #(dispatch :view/close (:db/id element))} "×"]]
          [:section
           [:label
            [:input
             {:type "text"
              :placeholder "Workspace name"
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
                [:div
                 (for [board boards :let [{:keys [db/id map/url map/name]} board]]
                   [:div {:key id :style {:background-image (str "url(" url ")")}
                          :on-click #(dispatch :workspace/change-map (:db/id element) id)}
                    [:div name]])]])
             [:input
              {:type "file"
               :accept "image/*"
               :multiple true
               :on-change
               #(doseq [file (.. % -target -files)]
                  (load-image
                   file
                   (fn [{:keys [data filename url img]}]
                     (let [id (ds/squuid) w (.-width img) h (.-height img)]
                       (-> (.-images store) (.put  #js {:id (str id) :data data :created-at (.now js/Date)}))
                       (dispatch
                        :map/create
                        element
                        {:map/id id :map/name filename :map/url url :map/width w :map/height h})))))}]])] nil)])))
