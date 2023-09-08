(ns ogres.app.render.scenes
  (:require [clojure.string :refer [blank? trim]]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render :refer [icon]]
            [uix.core :refer [defui $]]))

(defn ^:private close-prompt
  [label]
  (str "Are you sure you want to close "
       (if (blank? label)
         (str "this scene")
         (str "'" label "'"))
       "? This action cannot be undone."))

(def ^:private query
  [{:local/camera [:db/key]}
   {:local/cameras [:db/key :camera/label]}])

(defui scenes []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {current :local/camera
         cameras :local/cameras} result]
    ($ :nav.scenes
      ($ :ul
        (for [{:keys [db/key camera/label]} cameras]
          ($ :li.scenes-scene
            {:key key
             :data-selected (= (:db/key current) key)
             :on-click #(dispatch :scenes/change key)}
            ($ :.scenes-scene-label
              (if (blank? label) "New scene" (trim label)))
            ($ :button.scenes-scene-remove
              {:type "button" :title "Remove scene"
               :on-click
               (fn [event]
                 (.stopPropagation event)
                 (if (js/confirm (close-prompt label))
                   (dispatch :scenes/remove key)))}
              ($ icon {:name "x-circle-fill" :size 16}))))
        ($ :li.scenes-create
          ($ :button
            {:type "button"
             :title "Create new scene"
             :on-click #(dispatch :scenes/create)} ($ icon {:name "plus" :size 18})))))))
