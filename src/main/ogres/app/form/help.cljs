(ns ogres.app.form.help
  (:require [ogres.app.const :refer [VERSION]]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.shortcut :refer [shortcuts]]
            [ogres.app.provider.release :as release]
            [uix.core :refer [defui $ use-context]]))

(def ^:private confirm-upgrade
  "Upgrading will delete all your local data and restore this application to its original state.")

(def ^:private confirm-delete
  "Delete all your local data and restore this application to its original state?")

(defui form []
  (let [releases (use-context release/context)
        dispatch (use-dispatch)
        result   (use-query [[:local/type :default :conn]] [:db/ident :local])]
    ($ :.form-help
      (if (= (:local/type result) :host)
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
                         (dispatch :storage/reset)))} "Delete local data")))))))
      ($ :div
        ($ :h2 "Keyboard Shortcuts")
        ($ :table.shortcuts
          ($ :tbody
            (for [shortcut shortcuts]
              ($ :tr {:key (:name shortcut)}
                ($ :td
                  ($ :.shortcut (map (fn [s] ($ :code {:key (str s)} (str s))) (interpose \+ (:keys shortcut)))))
                ($ :td (str (:desc shortcut)))))))))))
