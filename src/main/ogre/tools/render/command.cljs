(ns ogre.tools.render.command
  (:require [ogre.tools.query :as query]
            [ogre.tools.render :refer [css]]
            [ogre.tools.render.icon :refer [icon]]
            [ogre.tools.state :refer [state]]
            [uix.core.alpha :as uix]))

(def modes
  [[:select :cursor 20 "Select"]
   [:canvas :image 18 "Canvas Options"]
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
      {:type "button" :title "Toggle Player Window" :class (css {:selected (:share/open? viewer)})
       :on-click #(dispatch :share/initiate)}
      [icon {:name :window :width 18 :height 18}]]
     [:button
      {:type "button" :title "Pause/Play" :class (css {:selected false})
       :disabled (not (:share/open? viewer)) :on-click #(dispatch :share/switch)}
      (if (:share/paused? viewer)
        [icon {:name :play-fill :width 24 :height 24}]
        [icon {:name :pause-fill :width 24 :height 24}])]]))
