(ns ogres.app.form.help
  (:require [ogres.app.const :refer [VERSION]]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [uix.core :refer [defui $]]))

(def ^:private confirm-delete
  "Are you sure you want to delete your local data? This will revert this app to its original state.")

(def ^:private resource-links
  [["https://github.com/samcf/ogres" "GitHub repository page" "Source Code"]
   ["https://github.com/samcf/ogres/discussions" "Ask a Question" "Ask a Question"]
   ["https://github.com/samcf/ogres/wiki" "Project wiki" "Wiki"]])

(def ^:private query
  [[:local/shortcuts? :default true]
   [:local/tooltips? :default true]])

(defui form []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {:local/keys [shortcuts? tooltips?]} result]
    ($ :section.help
      ($ :section
        ($ :header "Interface preferences")
        ($ :fieldset.checkbox
          ($ :input
            {:id        "show-shortcuts"
             :type      "checkbox"
             :checked   shortcuts?
             :on-change (fn [event]
                          (let [checked (.. event -target -checked)]
                            (dispatch :local/toggle-shortcuts checked)))})
          ($ :label {:for "show-shortcuts"} "Show shortcuts"))
        ($ :fieldset.checkbox
          ($ :input
            {:id        "show-tooltips"
             :type      "checkbox"
             :checked   tooltips?
             :on-change (fn [event]
                          (let [checked (.. event -target -checked)]
                            (dispatch :local/toggle-tooltips checked)))})
          ($ :label {:for "show-tooltips"} "Show tooltips")))
      ($ :section
        ($ :header "Data")
        ($ :p "All application data is saved locally on your browser, including
           any images you upload for maps or tokens. Any changes you make
           are automatically saved. As long as you use the same browser and
           don't clear your browser's cache, all your changes will stick
           around even if you close the browser or restart your computer.")
        ($ :br)
        ($ :button.button.button-danger
          {:on-click
           (fn []
             (if-let [_ (js/confirm confirm-delete)]
               (dispatch :storage/reset)))
           :style {:margin-bottom 8}}
          "Delete Local Data")
        ($ :p "Click this button to delete all application data, restoring this
           application to its original empty state. All your work and images
           will be removed.")
        ($ :br)
        ($ :p "Current version: " ($ :code VERSION)))
      ($ :section
        ($ :header "Resources")
        ($ :ul
          (for [[url title label] resource-links]
            ($ :li {:key url} ($ :a {:href url :title title :target "_blank"} label))))))))
