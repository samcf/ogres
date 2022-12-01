(ns ogres.app.form.data
  (:require [ogres.app.env :as env]
            [ogres.app.hooks :refer [use-dispatch]]
            [ogres.app.form.render :refer [form]]))

(defn ^{:private true} data-form []
  (let [dispatch (use-dispatch)]
    [:section
     [:header "Local Data"]
     [:p "This application stores all uploaded images and work on your
           browser. You may restore the application to its factory defaults
           by clicking the button below."]
     [:p "This will also move you to the latest version of this software so
           you may notice some changes."
      [:br]
      "Current version: " [:strong env/VERSION]]
     [:button.ogre-button {:on-click #(dispatch :storage/reset) :style {:margin-top 8}}
      "Restore Factory Defaults"]]))

(defmethod form :data []
  data-form)
