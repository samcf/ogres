(ns ogres.app.form.help
  (:require [ogres.app.form.render :refer [form]]))

(def links
  [["https://reddit.com/r/ogretools/" "Project subreddit"]
   ["https://github.com/samcf/ogres.app" "Project source code"]
   ["mailto:mail@samcf.me" "Personal email"]])

(defn ^{:private true} help []
  [:section
   [:header "Support and Contact"]
   [:p "ogres.app is a free and open-source virtual tabletop that helps you
        run your Dungeons & Dragons 5th Edition games. Please feel free to
        use any of the resources below to ask for help or make a suggestion."]
   [:ul
    (for [[url title] links]
      [:li {:key url} [:a {:href url :target "_blank" :title title} url]])]])

(defmethod form :help []
  help)
