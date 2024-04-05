(ns ogres.app.form.help
  (:require [ogres.app.const :refer [VERSION]]
            [ogres.app.hooks :refer [use-dispatch]]
            [ogres.app.provider.release :as release]
            [uix.core :refer [defui $ use-context use-ref use-state]]))

(def ^:private confirm-upgrade
  "Upgrading will delete all your local data and restore this application to its original state.")

(def ^:private confirm-delete
  "Delete all your local data and restore this application to its original state?")

(def ^:private confirm-backup
  "Backup your local data and images?")

(def ^:private confirm-restore
  "Delete all your local data and restore this application using the provided backup?")

(defui form []
  (let [[file-name set-file-name] (use-state nil)
        releases  (use-context release/context)
        dispatch  (use-dispatch)
        file-input (use-ref)]
    ($ :.form-help
      ($ :header ($ :h2 "Data"))
      ($ :fieldset.fieldset
        ($ :legend "Version" " [ " VERSION " ]")
        ($ :div.form-notice
          (if-let [latest (last releases)]
            (if (not= VERSION latest)
              ($ :<>
                ($ :p ($ :strong "There are updates available!"))
                ($ :p "Upgrading to the latest version will "
                  ($ :strong "delete all your local data") ". "
                  "Only upgrade if you are ready to start over from scratch.")
                ($ :br)
                ($ :button.button.button-primary
                  {:on-click
                   (fn []
                     (if-let [_ (js/confirm confirm-upgrade)]
                       (dispatch :storage/reset)))} "Upgrade to latest version [ " latest " ]"))
              ($ :<>
                ($ :p ($ :strong "You're on the latest version."))
                ($ :p "Pressing this button will delete all your local data and
                       restore the application to its original state.")
                ($ :br)
                ($ :button.button.button-neutral
                  {:on-click
                   (fn []
                     (if-let [_ (js/confirm confirm-delete)]
                       (dispatch :storage/reset)))} "Delete local data"))))))
      ($ :fieldset.fieldset
        ($ :legend "Backup and Restore")
        ($ :div.form-notice
          ($ :<>
            ($ :p {:style {:margin-bottom 4}}
              "Create a backup file that contains all your data and images. You
               can then use this file to restore your work on another computer
               or browser.")
            ($ :button.button.button-neutral
              {:on-click
               (fn []
                 (if-let [_ (js/confirm confirm-backup)]
                   (dispatch :storage/backup)))} "Create Backup")
            ($ :br)
            ($ :p {:style {:margin-bottom 4}}
              "Select a backup file to restore your data and images. Note that "
              ($ :strong "restoring from a file will delete all your current data") ".")
            ($ :form
              {:class "form-restore"
               :on-submit
               (fn [event]
                 (.preventDefault event)
                 (let [files (.. file-input -current -files)
                       file (first files)]
                   (if-not (nil? file)
                     (if-let [_ (js/confirm confirm-restore)]
                       (dispatch :storage/restore file)))))}
              ($ :div
                {:style
                 {:display "flex"
                  :flex-flow "row rap"
                  :gap "4px"}}
                ($ :label
                  {:for "restore-upload"
                   :class "button button-neutral"
                   :style {:box-sizing "border-box"}}
                  (or file-name "Choose file"))
                ($ :input
                  {:type "file"
                   :name "restore-upload"
                   :id "restore-upload"
                   :style {:display "none"}
                   :accept ".json"
                   :ref file-input
                   :on-change
                   (fn [event]
                     (let [files (.. event -target -files)
                           file (first files)]
                       (set-file-name (.-name file))))})
                ($ :button.button.button-primary
                  {:type "submit" :disabled (if (nil? file-name) true)}
                  "Restore")))))))))
