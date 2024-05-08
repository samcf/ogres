(ns ogres.app.component.scenes
  (:require [clojure.string :refer [replace]]
            [ogres.app.component :refer [icon]]
            [ogres.app.hooks :refer [use-dispatch use-query use-shortcut]]
            [uix.core :refer [defui $ use-callback]]))

(def ^:private filename-re #"\d+x\d+|[^\w ]|.[^.]+$")

(defn ^:private render-scene-name [camera]
  (if-let [label (:camera/label camera)]
    label
    (if-let [filename (-> camera :camera/scene :scene/image :image/name)]
      (replace filename filename-re "")
      "Untitled scene")))

(defn ^:private render-remove-prompt [camera]
  (str "Are you sure you want to remove '" (render-scene-name camera) "'?"))

(def ^:private query
  [:user/camera
   {:user/cameras
    [:db/id
     :camera/label
     {:camera/scene
      [{:scene/image [:image/name]}]}]}])

(defui scenes []
  (let [dispatch (use-dispatch)
        {current :user/camera
         cameras :user/cameras} (use-query query)]
    (use-shortcut ["delete" "backspace"]
      (use-callback
       (fn [event]
         (if (= (.-name (.-activeElement js/document)) "scene")
           (let [id (js/Number (.. event -originalEvent -target -value))
                 cm (first (filter (comp #{id} :db/id) cameras))]
             (if (js/confirm (render-remove-prompt cm))
               (dispatch :scenes/remove id))))) [dispatch cameras]))
    ($ :ul.scenes {:role "tablist"}
      (for [{id :db/id :as camera} cameras
            :let [selected (= id (:db/id current))]]
        ($ :li.scenes-scene {:key id :role "tab" :aria-selected selected}
          ($ :label
            ($ :input
              {:type "radio"
               :name "scene"
               :value id
               :checked selected
               :on-change (fn [event] (dispatch :scenes/change (js/Number (.. event -target -value))))})
            ($ :.scenes-label
              (render-scene-name camera))
            ($ :.scenes-remove
              {:on-click
               (fn []
                 (if (js/confirm (render-remove-prompt camera))
                   (dispatch :scenes/remove id)))}
              ($ icon {:name "x" :size 21})))))
      ($ :li.scenes-create {:role "tab"}
        ($ :button
          {:type "button"
           :title "Create a new scene."
           :on-click #(dispatch :scenes/create)}
          ($ icon {:name "plus" :size 19}))))))
