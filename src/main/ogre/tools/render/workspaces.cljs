(ns ogre.tools.render.workspaces
  (:require [clojure.string :refer [blank? trim]]
            [ogre.tools.state :refer [use-query]]))

(def query
  '[:find (pull $ ?id pattern) ?tx
    :keys attrs tx
    :in $ pattern
    :where [?id :element/type :canvas ?tx]])

(def attrs
  [:db/id :element/name :viewer/_workspace])

(defn workspaces [props]
  (let [[workspaces dispatch] (use-query {:query query :pull attrs})]
    [:div.workspaces
     (for [workspace (sort-by :tx workspaces)
           :let [{:keys [db/id element/name viewer/_workspace]} (:attrs workspace)]]
       [:div {:key id :css {:selected _workspace}}
        [:div {:on-click #(dispatch :workspace/change id)}
         (if (blank? name) [:em "New Canvas"] (trim name))]
        [:button {:type "button" :on-click #(dispatch :workspace/remove id) :title "Close canvas"} "×"]])
     [:button {:type "button" :on-click #(dispatch :workspace/create) :title "Create new canvas"} "+"]]))
