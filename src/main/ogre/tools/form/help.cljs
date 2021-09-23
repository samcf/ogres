(ns ogre.tools.form.help
  (:require
   [ogre.tools.form.render :refer [form]]
   [ogre.tools.query :as query]
   [ogre.tools.render :refer [button checkbox]]
   [ogre.tools.state :refer [state]]
   [uix.core.alpha :as uix]))

(defmethod form :help [props]
  (let [{:keys [dispatch data]} (uix/context state)
        {:keys [viewer/shortcuts? viewer/tooltips?]} (query/viewer data)]
    [:<>
     [:section
      [:header "About"]
      [:p "ogre.tools is a free and open-source virtual table top for your "
       [:strong "Dungeons & Dragons 5th Edition"]
       " games."]
      [:p "Find a bug or want to explore the project code?"
       [:br]
       [:a {:href "https://github.com/samcf/ogre.tools" :target "_blank" :title "Repository home"}
        "https://github.com/samcf/ogre.tools"]]]
     [:section
      [:legend "Options"]
      [:fieldset.setting
       [:label "Show Shortcuts"]
       (for [display? [true false] :let [checked? (= display? shortcuts?)]]
         ^{:key display?}
         [checkbox
          {:checked checked?
           :on-change
           (fn []
             (if (not checked?)
               (dispatch :interface/toggle-shortcuts display?)))}
          (if display? "Yes" "No")])]
      [:fieldset.setting
       [:label "Show Tooltips"]
       (for [display? [true false] :let [checked? (= display? tooltips?)]]
         ^{:key display?}
         [checkbox
          {:checked checked?
           :on-change
           (fn []
             (if (not checked?)
               (dispatch :interface/toggle-tooltips display?)))}
          (if display? "Yes" "No")])]]
     [:section
      [:legend "Local Data"]
      [:p "This application stores all uploaded images and work on your
           browser. You may restore the application to its factory defaults
           by clicking the button below."]
      [:p [:strong "All uploaded images and work will be permanently deleted."]]
      [button {:on-click #(dispatch :storage/reset) :style {:margin-top 8}}
       "Reset Data"]]]))
