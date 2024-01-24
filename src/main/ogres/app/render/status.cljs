(ns ogres.app.render.status
  (:require [ogres.app.render :refer [icon]]
            [ogres.app.hooks :refer [use-query use-dispatch]]
            [uix.core :refer [defui $]]))

(def ^:private status-query
  [{:root/local   [[:session/state :default :initial]]
    :root/session [:session/conns :session/room]}])

(def ^:private status-icon
  ($ icon {:name "globe-americas" :size 16}))

(defui button []
  (let [dispatch (use-dispatch)
        result   (use-query status-query [:db/ident :root])
        {{state :session/state} :root/local
         {code  :session/room
          conns :session/conns} :root/session} result]
    ($ :.join
      (case state
        :initial      ($ :button.button {:on-click #(dispatch :session/request)} status-icon "Start online game")
        :connecting   ($ :button.button {:disabled true} status-icon "Connecting...")
        :connected    ($ :button.button {:disabled true} status-icon "Connected / " code " / [ " (inc (count conns)) " ]")
        :disconnected ($ :button.button {:disabled true} status-icon "Disconnected")
        ($ :button.button {:disabled true} status-icon "Status not known")))))
