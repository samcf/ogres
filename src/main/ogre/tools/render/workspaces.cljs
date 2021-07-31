(ns ogre.tools.render.workspaces
  (:require [rum.core :as rum]
            [spade.core :refer [defclass]]
            [ogre.tools.render :refer [context css]]
            [ogre.tools.query :as query]))

(defclass styles []
  {:display "flex" :flex-wrap "wrap"}
  [:>div
   {:background-color "transparent"
    :border-radius "2px"
    :border "1px solid var(--color-primary-c)"
    :box-sizing "border-box"
    :display "flex"
    :margin "0 8px 8px 0"}
   [:>label
    {:cursor "pointer"
     :font-size "13px"
     :width "140px"
     :opacity "0.60"
     :overflow "hidden"
     :padding "4px 32px 4px 12px"
     :text-overflow "ellipsis"
     :white-space "nowrap"}
    [:>input
     {:position "absolute"
      :clip "rect(0, 0, 0, 0)"
      :pointer-events "none"}]
    [:&.active {:background-color "var(--theme-background-c)" :opacity 1}]
    [:&:hover {:background-color "var(--theme-background-c)"}]
    [:>em {}]]
   [:>button
    {:background-color "transparent"
     :border "none"
     :color "var(--theme-text)"
     :cursor "pointer"
     :font-size "16px"
     :padding "4px 8px"}]]
  [:>button
   {:background "transparent"
    :border "1px solid var(--color-primary-c)"
    :border-radius "2px"
    :color "var(--theme-text)"
    :cursor "pointer"
    :display "block"
    :margin-bottom "8px"
    :width "34px"}
   [:&:hover {:background-color "var(--color-primary-c)"}]])

(rum/defc workspaces [props & children]
  (rum/with-context [{:keys [data dispatch]} context]
    (let [current (:workspace props)]
      [:div {:class (styles)}
       (for [workspace (query/workspaces data) :let [id (:db/id workspace)]]
         [:div {:key id}
          [:label
           {:className (css {:active (= current workspace)})}
           [:input
            {:type "radio"
             :name "window"
             :value id
             :checked (= current workspace)
             :on-change #(dispatch :workspace/change id)}]
           (let [name (clojure.string/trim (or (:element/name workspace) ""))]
             (if (empty? name) [:em "Unnamed Workspace"] [:span name]))]
          [:button {:type "button" :on-click #(dispatch :workspace/remove id)} "×"]])
       [:button {:type "button" :on-click #(dispatch :workspace/create)} "+"]])))
