(ns ogres.app.render.workspaces
  (:require [clojure.string :refer [blank? trim]]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render :refer [css]]
            [uix.core :refer [defui $]]))

(def query
  [{:local/window [:db/key]}
   {:local/windows [:db/key :window/label]}])

(defui workspaces []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {current :local/window
         windows :local/windows} result]
    ($ :div.workspaces
      (for [{:keys [db/key window/label]} windows]
        ($ :div {:key key :class (css {:selected (= key (:db/key current))})}
          ($ :div {:on-click #(dispatch :workspace/change key)}
            (if (blank? label) ($ :em "New Canvas") (trim label)))
          ($ :button {:type "button" :on-click #(dispatch :workspace/remove key) :title "Close canvas"} "×")))
      ($ :button {:type "button" :on-click #(dispatch :workspace/create) :title "Create new canvas"} "+"))))
