(ns ogre.tools.render.command
  (:require [ogre.tools.query :as query]
            [ogre.tools.render :refer [css]]
            [ogre.tools.render.icon :refer [icon]]
            [ogre.tools.state :refer [state]]
            [uix.core.alpha :as uix]))

(def modes
  [[:select :cursor 20 "Select"]
   [:canvas :image 18 "Canvas Options"]
   [:grid :grid 21 "Grid Options"]
   [:ruler :rulers 20 "Ruler Tool"]
   [:circle :circle 20 "Draw Circle"]
   [:rect :square 20 "Draw Rectangle"]
   [:cone :triangle 20 "Draw Cone"]
   [:line :slash-lg 20 "Draw Line"]])

(defn command [props]
  (let [{:keys [data workspace dispatch]} (uix/context state)
        {show :grid/show current :canvas/mode theme :canvas/theme} workspace
        viewer (query/viewer data)]
    [:div.commands
     (for [[mode name size title] modes]
       [:button
        {:key mode :type "button" :class (css {:selected (= current mode)})
         :title title :on-click #(dispatch :canvas/toggle-mode mode)}
        [icon {:name name :width size :height size}]])
     [:button
      {:type "button" :title "Toggle Grid" :class (css {:selected show}) :on-click #(dispatch :grid/toggle)}
      [:div.grid]]
     [:button
      {:type "button" :title "Toggle Theme" :on-click #(dispatch :canvas/toggle-theme)}
      (if (= theme :dark)
        [icon {:name :back :width 20 :height 20}]
        [icon {:name :front :width 20 :height 20}])]
     [:button
      {:type "button" :title "Zoom Out" :on-click #(dispatch :zoom/out)}
      [icon {:name :zoom-out :width 18 :height 18}]]
     [:button
      {:type "button" :title "Zoom In" :on-click #(dispatch :zoom/in)}
      [icon {:name :zoom-in :width 18 :height 18}]]
     [:button
      {:type "button" :title "Toggle Player Window" :class (css {:selected (:share/open? viewer)})
       :on-click #(dispatch :share/initiate)}
      [icon {:name :window :width 18 :height 18}]]
     [:button
      {:type "button" :title "Pause/Play" :class (css {:selected false})
       :disabled (not (:share/open? viewer)) :on-click #(dispatch :share/switch)}
      (if (:share/paused? viewer)
        [icon {:name :play-fill :width 24 :height 24}]
        [icon {:name :pause-fill :width 24 :height 24}])]
     [:a {:href "https://www.github.com/samcf/ogre.tools" :title "Project home" :target "_blank"}
      [:button {:type "button"}
       [icon {:name :github :width 20 :height 20}]]]
     [:button
      {:type "button" :title "Reset Local Storage" :on-click #(dispatch :storage/reset)}
      [icon {:name :recycle :width 20 :height 20}]]]))
