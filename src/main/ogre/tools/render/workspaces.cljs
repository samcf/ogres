(ns ogre.tools.render.workspaces
  (:require [clojure.string :refer [blank? trim]]
            [ogre.tools.state :refer [use-query]]))

(def query
  {:pull
   [:root/canvas
    {:root/canvases
     [:db/id :element/name]}]})

(defn workspaces [props]
  (let [[data dispatch] (use-query query)
        {current :root/canvas
         workspaces :root/canvases} data]
    [:div.workspaces
     (for [{:keys [db/id element/name]} workspaces]
       [:div {:key id :css {:selected (= id (:db/id current))}}
        [:div {:on-click #(dispatch :workspace/change id)}
         (if (blank? name) [:em "New Canvas"] (trim name))]
        [:button {:type "button" :on-click #(dispatch :workspace/remove id) :title "Close canvas"} "×"]])
     [:button {:type "button" :on-click #(dispatch :workspace/create) :title "Create new canvas"} "+"]]))
