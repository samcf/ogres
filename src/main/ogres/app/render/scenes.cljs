(ns ogres.app.render.scenes
  (:require [clojure.string :refer [replace]]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render :refer [icon]]
            [uix.core :refer [defui $]]))

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
  [{:local/camera [:db/key]}
   {:local/cameras
    [:db/key
     :camera/label
     {:camera/scene
      [{:scene/image [:image/name]}]}]}])

(defui scenes []
  (let [dispatch (use-dispatch)
        result   (use-query query)]
    ($ :nav.scenes
      ($ :ul
        (for [camera (:local/cameras result) :let [key (:db/key camera)]]
          ($ :li.scenes-scene
            {:key key
             :data-selected (= (:db/key (:local/camera result)) key)
             :on-click #(dispatch :scenes/change key)}
            ($ :.scenes-scene-label (render-scene-name camera))
            ($ :button.scenes-scene-remove
              {:type "button" :title "Remove scene"
               :on-click
               (fn [event]
                 (.stopPropagation event)
                 (if (js/confirm (render-remove-prompt camera))
                   (dispatch :scenes/remove key)))}
              ($ icon {:name "x-circle-fill" :size 16}))))
        ($ :li.scenes-create
          ($ :button
            {:type "button"
             :title "Create new scene"
             :on-click #(dispatch :scenes/create)} ($ icon {:name "plus" :size 18})))))))
