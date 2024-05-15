(ns ogres.app.component.error
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.provider.storage :refer [initialize]]
            [uix.core :refer [$]]))

(defn error-page
  "Returns a 2-arity function suitable for use as a React error boundary via
   `(uix/create-error-boundary ...)`."
  [[state _] {:keys [children]}]
  (let [store (initialize)]
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
               #(.then
                 (.delete store)
                 (.reload (.-location js/window)))} "Delete your local data")
            ($ :nav {:style {:margin-top 32 :color "var(--color-danger-500)"}}
              ($ :ul {:style {:display "flex" :gap 16}}
                ($ :li ($ :a {:href "/"} "Home"))
                ($ :li ($ :a {:href "https://github.com/samcf/ogres/wiki"} "Wiki"))
                ($ :li ($ :a {:href "https://github.com/samcf/ogres/discussions"} "Support")))))))
      children)))
