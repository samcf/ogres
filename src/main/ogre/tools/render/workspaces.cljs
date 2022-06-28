(ns ogre.tools.render.workspaces
  (:require [clojure.string :refer [blank? trim]]
            [ogre.tools.events :refer [use-dispatch]]
            [ogre.tools.state :refer [use-query]]))

(def query
  [{:local/window [:entity/key]}
   {:local/windows [:entity/key :window/label]}])

(defn workspaces []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {current :local/window
         windows :local/windows} result]
    [:div.workspaces
     (for [{:keys [entity/key window/label]} windows]
       [:div {:key key :css {:selected (= key (:entity/key current))}}
        [:div {:on-click #(dispatch :workspace/change key)}
         (if (blank? label) [:em "New Canvas"] (trim label))]
        [:button {:type "button" :on-click #(dispatch :workspace/remove key) :title "Close canvas"} "×"]])
     [:button {:type "button" :on-click #(dispatch :workspace/create) :title "Create new canvas"} "+"]]))
