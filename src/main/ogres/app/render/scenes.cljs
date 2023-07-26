(ns ogres.app.render.scenes
  (:require [clojure.string :refer [blank? trim]]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render :refer [css]]
            [uix.core :refer [defui $]]))

(def query
  [{:local/camera [:db/key]}
   {:local/cameras [:db/key :camera/label]}])

(defui scenes []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {current :local/camera
         cameras :local/cameras} result]
    ($ :div.scenes
      (for [{:keys [db/key camera/label]} cameras]
        ($ :div {:key key :class (css {:selected (= key (:db/key current))})}
          ($ :div {:on-click #(dispatch :scenes/change key)}
            (if (blank? label) ($ :em "New scene") (trim label)))
          ($ :button
            {:type "button"
             :on-click #(when (js/confirm (str "Close " (when (not (blank? label)) (str label " ")) "scene?"))
                          (dispatch :scenes/remove key))
             :title "Close scene"}
            "×")))
      ($ :button {:type "button" :on-click #(dispatch :scenes/create) :title "Create new scene"} "+"))))
