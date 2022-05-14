(ns ogre.tools.render.workspaces
  (:require [clojure.string :refer [blank? trim]]
            [ogre.tools.state :refer [use-query]]))

(def query
  [{:local/window [:entity/key]}
   {:local/windows [:entity/key {:window/canvas [:entity/key :element/name]}]}])

(defn workspaces []
  (let [[result dispatch] (use-query query)
        {current :local/window
         windows :local/windows} result]
    [:div.workspaces
     (for [{:keys [entity/key element/name]} windows]
       [:div {:key key :css {:selected (= key (:entity/key current))}}
        [:div {:on-click #(dispatch :workspace/change key)}
         (if (blank? name) [:em "New Canvas"] (trim name))]
        [:button {:type "button" :on-click #(dispatch :workspace/remove key) :title "Close canvas"} "×"]])
     [:button {:type "button" :on-click #(dispatch :workspace/create) :title "Create new canvas"} "+"]]))
