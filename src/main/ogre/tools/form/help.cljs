(ns ogre.tools.form.help
  (:require [ogre.tools.form.render :refer [form]]
            [ogre.tools.render :refer [button checkbox]]
            [ogre.tools.state :refer [use-pull VERSION]]))

(def pattern
  [[:local/shortcuts? :default true]
   [:local/tooltips? :default true]])

(def links
  [["https://reddit.com/r/ogretools/" "Project subreddit"]
   ["https://github.com/samcf/ogre.tools" "Project source code"]
   ["mailto:mail@samcf.me" "Personal email"]])

(defn help []
  (let [[result dispatch] (use-pull pattern [:db/ident :local])
        {shortcuts? :local/shortcuts?
         tooltips?  :local/tooltips?} result]
    [:<>
     [:section
      [:header "Settings"]]
     [:section
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
      [:legend "More Information"]
      [:p "ogre.tools is a free and open-source virtual tabletop that helps you
           run your Dungeons & Dragons 5th Edition games. Please feel free to
           use any of the resources below to ask for help or make a suggestion."]
      [:ul
       (for [[url title] links]
         [:li {:key url} [:a {:href url :target "_blank" :title title} url]])]]
     [:section
      [:legend "Local Data"]
      [:p "This application stores all uploaded images and work on your
           browser. You may restore the application to its factory defaults
           by clicking the button below."]
      [:p "This will also move you to the latest version of this software so
           you may notice some changes."
       [:br]
       "Current version: " [:strong VERSION]]
      [button {:on-click #(dispatch :storage/reset) :style {:margin-top 8}}
       "Restore Factory Defaults"]]]))

(defmethod form :help []
  help)
