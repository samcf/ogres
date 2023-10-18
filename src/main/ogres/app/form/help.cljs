(ns ogres.app.form.help
  (:require [ogres.app.const :refer [VERSION]]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.shortcut :refer [shortcuts]]
            [uix.core :refer [defui $]]))

(def ^:private confirm-delete
  "Are you sure you want to reset your local data? This will revert this app to its original state.")

(def ^:private resource-links
  [["https://ogres.app" "Application home" "Home"]
   ["https://github.com/samcf/ogres" "Project repository" "Code"]
   ["https://github.com/samcf/ogres/wiki" "Project wiki" "Wiki"]
   ["https://github.com/samcf/ogres/discussions" "Project discussion" "Support"]])

(def ^:private query
  [[:local/tooltips? :default true]])

(defui form []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {:local/keys [tooltips?]} result]
    ($ :section.help
      ($ :section
        ($ :header "Interface preferences")
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
        ($ :header "Version" " [ " VERSION " ]")
        ($ :p "To upgrade to the latest version of this app, click the button
               below to delete your local data and reload the page."
          ($ :span " ")
          ($ :strong "This will remove all uploaded images and destroy all created scenes."))
        ($ :br)
        ($ :button.button.button-danger
          {:on-click
           (fn []
             (if-let [_ (js/confirm confirm-delete)]
               (dispatch :storage/reset)))}
          "Reset local data"))
      ($ :section
        ($ :header "Keyboard Shortcuts")
        ($ :table.shortcuts
          (for [shortcut shortcuts]
            ($ :tr {:key (:name shortcut)}
              ($ :td
                ($ :div.shortcut (map (fn [s] ($ :div (str s))) (interpose \+ (:keys shortcut)))))
              ($ :td (str (:desc shortcut)))))))
      ($ :section
        ($ :header "Resources")
        ($ :ul {:style {:color "var(--color-danger-500)"}}
          (for [[url title] resource-links]
            ($ :li {:key url} ($ :a {:href url :title title :target "_blank"} url))))))))
