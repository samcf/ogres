(ns ogre.tools.render.workspaces
  (:require [clojure.string :refer [blank? trim]]
            [ogre.tools.query :as query]
            [ogre.tools.render :refer [css]]
            [ogre.tools.state :refer [state]]
            [uix.core.alpha :as uix]))

(defn workspaces [props]
  (let [{:keys [data dispatch] :as context} (uix/context state)]
    [:div.workspaces
     (for [{:keys [db/id element/name] :as ws} (query/workspaces data)
           :let [selected? (= ws (:workspace context))]]
       [:div {:key id :class (css {:selected selected?})}
        [:div {:on-click #(dispatch :workspace/change id)}
         (if (blank? name) [:em "New Canvas"] (trim name))]
        [:button {:type "button" :on-click #(dispatch :workspace/remove id) :title "Close canvas"} "×"]])
     [:button {:type "button" :on-click #(dispatch :workspace/create) :title "Create new canvas"} "+"]]))
