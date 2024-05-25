(ns ogres.app.core
  (:require [ogres.app.component         :refer [stylesheet]]
            [ogres.app.component.error   :refer [error-page]]
            [ogres.app.component.layout  :refer [layout]]
            [ogres.app.dom               :refer [user-type]]
            [ogres.app.provider.cursor   :as cursor]
            [ogres.app.provider.dispatch :as dispatch]
            [ogres.app.provider.events   :as events]
            [ogres.app.provider.image    :as image]
            [ogres.app.provider.portal   :as portal]
            [ogres.app.provider.release  :as release]
            [ogres.app.provider.shortcut :as shortcut]
            [ogres.app.provider.state    :as state]
            [ogres.app.provider.storage  :as storage]
            [ogres.app.provider.window   :as window]
            [ogres.app.session           :as session]
            [uix.core :as uix :refer [$ defui]]
            [uix.dom  :as dom]))

(def ^:private error-boundary
  (uix/create-error-boundary
   {:derive-error-state identity}
   error-page))

(defui ^:private app []
  ($ uix/strict-mode
    ($ stylesheet {:name "ogres.app.css"})
    ($ events/provider
      ($ state/provider {:type (user-type)}
        ($ dispatch/provider
          ($ storage/provider
            ($ release/provider
              ($ image/provider
                ($ portal/provider
                  ($ :<>
                    ($ storage/handlers)
                    ($ window/provider)
                    ($ shortcut/handlers)
                    ($ session/handlers)
                    ($ cursor/handlers)
                    ($ error-boundary
                      ($ layout))))))))))))

(defn ^:export main []
  (let [elem (.querySelector js/document "#root")
        root (dom/create-root elem)]
    (dom/render-root ($ app) root)))
