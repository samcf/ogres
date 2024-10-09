(ns ogres.app.component.error
  (:require [ogres.app.component :refer [icon]]
            [uix.core :refer [$]]))

(defn error-page
  "Returns a 2-arity function suitable for use as a React error boundary via
   `(uix/create-error-boundary ...)`."
  [[state _] {:keys [children]}]
  (if (some? state)
    ($ :.layout-error
      ($ :.layout-error-content
        ($ :div {:style {:margin-top 4 :color "hsl(6, 73%, 60%)"}}
          ($ icon {:name "exclamation-triangle-fill"}))
        ($ :div
          ($ :div {:style {:font-size 20 :line-height 1}}
            "The application crashed unexpectedly.")
          ($ :div {:style {:margin-top 16 :margin-bottom 8 :max-width 512}}
            "Try refreshing the page; if that doesn't work you may have to
                 delete your local data. This will reset the application to
                 its original state, removing your data and images.")
          ($ :button.button.button-danger
            {:on-click
             (fn []
               (if (js/confirm
                    (str "Are you sure you want to delete all your work and "
                         "start over? This may only be necessary if reloading "
                         "the app in your browser doesn't resolve this error "
                         "page."))
                 (let [delete (.deleteDatabase js/indexedDB "ogres.app")
                       reload (fn [] (.. js/window -location reload))]
                   (.addEventListener delete "blocked" reload)
                   (.addEventListener delete "success" reload))))}
            "Delete your local data")
          ($ :nav {:style {:margin-top 32 :color "var(--color-danger-500)"}}
            ($ :ul {:style {:display "flex" :gap 16}}
              ($ :li ($ :a {:href "/"} "Home"))
              ($ :li ($ :a {:href "https://github.com/samcf/ogres/wiki"} "Wiki"))
              ($ :li ($ :a {:href "https://github.com/samcf/ogres/discussions"} "Support")))))))
    children))
