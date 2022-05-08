(ns ogre.tools.render.workspaces
  (:require [clojure.string :refer [blank? trim]]
            [datascript.core :refer [squuid]]
            [ogre.tools.state :refer [use-query]]))

(def query
  {:pull
   [{:root/canvas [:entity/key]}
    {:root/canvases [:entity/key :element/name]}]})

(defn workspaces []
  (let [[data dispatch] (use-query query)
        {current :root/canvas
         workspaces :root/canvases} data]
    [:div.workspaces
     (for [{:keys [entity/key element/name]} workspaces]
       [:div {:key key :css {:selected (= key (:entity/key current))}}
        [:div {:on-click #(dispatch :workspace/change key)}
         (if (blank? name) [:em "New Canvas"] (trim name))]
        [:button {:type "button" :on-click #(dispatch :workspace/remove key (squuid)) :title "Close canvas"} "×"]])
     [:button {:type "button" :on-click #(dispatch :workspace/create (squuid)) :title "Create new canvas"} "+"]]))
