(ns ogres.app.render.scenes
  (:require [clojure.string :refer [blank? trim]]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render :refer [css icon]]
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
    ($ :div.scenes
      (for [{:keys [db/key camera/label]} cameras]
        ($ :div {:key key :class (css {:selected (= key (:db/key current))})}
          ($ :div {:on-click #(dispatch :scenes/change key)}
            (if (blank? label) "New scene" (trim label)))
          ($ :button
            {:type "button" :title "Remove scene"
             :on-click
             (fn []
               (if (js/confirm (close-prompt label))
                 (dispatch :scenes/remove key)))}
            ($ icon {:name "x-circle-fill" :size 16}))))
      ($ :button {:type "button" :on-click #(dispatch :scenes/create) :title "Create new scene"} "+"))))
