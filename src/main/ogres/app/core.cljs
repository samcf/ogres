(ns ogres.app.core
  (:require [ogres.app.layout            :refer [layout]]
            [ogres.app.provider.cursor   :as cursor]
            [ogres.app.provider.dispatch :as dispatch]
            [ogres.app.provider.events   :as events]
            [ogres.app.provider.image    :as image]
            [ogres.app.provider.portal   :as portal]
            [ogres.app.provider.release  :as release]
            [ogres.app.provider.state    :as state]
            [ogres.app.provider.storage  :as storage]
            [ogres.app.provider.window   :as window]
            [ogres.app.render            :refer [error-boundary stylesheet]]
            [ogres.app.session           :as session]
            [ogres.app.shortcut          :as shortcut]
            [uix.core                    :refer [defui $ strict-mode]]
            [uix.dom                     :refer [create-root render-root]]))

(defui ^:private app []
  ($ strict-mode
    ($ stylesheet {:name "fonts.css"})
    ($ stylesheet {:name "reset.css"})
    ($ stylesheet {:name "ogres.app.css"})
    ($ events/provider
      ($ state/provider
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
        root (create-root elem)]
    (render-root ($ app) root)))
