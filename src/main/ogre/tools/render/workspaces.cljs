(ns ogre.tools.render.workspaces
  (:require [uix.core.alpha :as uix]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [context css]]
            [ogre.tools.query :as query]))

(defclass styles []
  {:display "flex" :flex-wrap "wrap"}
  [:>div
   {:background-color "transparent"
    :border "1px solid var(--theme-background-d)"
    :border-radius "4px"
    :color "rgba(255, 255, 255, 0.50)"
    :display "flex"
    :margin "0 4px 4px 0"}
   [:&:hover
    {}]
   [:&.active
    {:background-color "var(--theme-background-d)"
     :color "rgba(255, 255, 255, 1)"}]
   [:>label
    {:cursor "pointer"
     :font-size "13px"
     :overflow "hidden"
     :padding "4px 32px 4px 12px"
     :text-overflow "ellipsis"
     :white-space "nowrap"
     :width "140px"}
    [:>input
     {:position "absolute"
      :clip "rect(0, 0, 0, 0)"
      :pointer-events "none"}]]
   [:>button
    {:background-color "transparent"
     :border "none"
     :color "var(--theme-text)"
     :cursor "pointer"
     :font-size "16px"
     :padding "4px 8px"}]]
  [:>button
   {:background "var(--theme-background-d)"
    :border "none"
    :border-radius "4px"
    :color "var(--theme-text)"
    :cursor "pointer"
    :display "block"
    :margin-bottom "4px"
    :width "30px"}
   [:&:hover {:background-color "rgba(255, 255, 255, 0.10)"}]])

(defn workspaces [props]
  (let [{:keys [data dispatch] :as context} (uix/context context)]
    [:div {:class (styles)}
     (for [workspace (query/workspaces data) :let [id (:db/id workspace)]]
       [:div {:key id :class (css {:active (= (:workspace context) workspace)})}
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
