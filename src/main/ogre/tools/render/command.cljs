(ns ogre.tools.render.command
  (:require [ogre.tools.query :as query]
            [ogre.tools.render :refer [css]]
            [ogre.tools.render.icon :refer [icon]]
            [ogre.tools.state :refer [state]]
            [uix.core.alpha :as uix]))

(def shape->icon
  {:circle :circle
   :rect :square
   :cone :triangle
   :line :slash-lg})

(defn command-set [first & rest]
  [:div.command-set
   [:div.command-set-mark]
   [:div.command-set-first first]
   [:div.command-set-rest rest]])

(defn command [props]
  (let [{:keys [dispatch data workspace]} (uix/context state)
        {:keys [grid/show canvas/mode canvas/theme canvas/last-shape]} workspace
        {:keys [share/open? share/paused?]} (query/viewer data)
        mode-attrs
        (fn [given]
          {:type "button"
           :key given
           :class (css {:selected (= given mode)})
           :on-click #(dispatch :canvas/toggle-mode given)})]
    [:div.commands
     [:button (mode-attrs :select) [icon {:name :cursor}]]
     [:button (mode-attrs :canvas) [icon {:name :image}]]
     [:button (mode-attrs :ruler) [icon {:name :rulers}]]
     [command-set
      (let [last (or last-shape :circle)]
        [:button (mode-attrs last) [icon {:name (shape->icon last)}]])
      [:button (mode-attrs :circle) [icon {:name :circle}]]
      [:button (mode-attrs :rect) [icon {:name :square}]]
      [:button (mode-attrs :cone) [icon {:name :triangle}]]
      [:button (mode-attrs :line) [icon {:name :slash-lg}]]]
     [command-set
      [:button
       {:class (css {:active open?}) :on-click #(dispatch :share/initiate)}
       [icon {:name :window}]]
      [:button {:key :switch :disabled (not open?) :on-click #(dispatch :share/switch)}
       (if paused?
         [icon {:name :play-fill}]
         [icon {:name :pause-fill}])]]
     #_[:button (mode-attrs :help) [icon {:name :question-diamond}]]]))
