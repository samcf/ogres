(ns ogre.tools.render.workspaces
  (:require [uix.core.alpha :as uix]
            [ogre.tools.render :refer [context css]]
            [ogre.tools.query :as query]))

(defn workspaces [props]
  (let [{:keys [data dispatch] :as context} (uix/context context)]
    [:div.workspaces
     (for [workspace (query/workspaces data) :let [id (:db/id workspace)]]
       [:div {:key id :class (css {:selected (= (:workspace context) workspace)})}
        [:label
         [:input
          {:type "radio"
           :name "window"
           :value id
           :checked (= (:workspace context) workspace)
           :on-change #(dispatch :workspace/change id)}]
         (let [name (clojure.string/trim (or (:element/name workspace) ""))]
           (if (empty? name) [:em "Unnamed Workspace"] [:span name]))]
        [:button {:type "button" :on-click #(dispatch :workspace/remove id)} "×"]])
     [:button {:type "button" :on-click #(dispatch :workspace/create)} "+"]]))
