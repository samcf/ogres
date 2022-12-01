(ns ogres.app.form.session
  (:require [clojure.string :refer [capitalize]]
            [ogres.app.env :as env]
            [ogres.app.form.render :refer [form]]
            [ogres.app.hooks :refer [use-dispatch use-query]]))

(def ^{:private true} query
  [{:root/local
    [:entity/key
     :local/type
     :local/color
     [:session/state :default :initial]]}
   {:root/session
    [:session/room
     {:session/conns [:entity/key :local/color :local/type]}
     {:session/host [:entity/key]}]}])

(defn ^{:private true} session-url [room-key]
  (str (.. js/window -location -origin) "?r=" env/VERSION "&join=" room-key))

(defn ^{:private true} session-form []
  (let [dispatch (use-dispatch)
        result   (use-query query [:db/ident :root])
        {{state :session/state
          type  :local/type} :root/local
         {room  :session/room
          conns :session/conns} :root/session} result]
    [:div
     (if (= type :host)
       [:section
        [:header "Invite Friends"]
        [:fieldset.session-controls
         [:button.ogre-button
          {:type "button" :on-click #(dispatch :session/request) :style {:flex 2}
           :disabled (#{:connecting :connected} state)}
          "Start New Session"]
         [:button.ogre-button
          {:type "button" :on-click #(dispatch :session/close) :style {:flex 1}
           :disabled (#{:initial :disconnected} state)}
          "Disconnect"]]
        (if (= state :connected)
          [:fieldset.session-share
           [:input {:type "text" :value (session-url room) :readOnly true :on-click (fn [event] (.select (.-target event)))}]
           [:label "Invite players to join this game by sharing this link
                    with them. When you disconnect or close the browser tab,
                    the entire session will be closed."]])]
       [:section
        [:header "Session"]
        [:div.session-status
         {:css (str "session-status--" (name state))} (capitalize (name state))]])
     (if (= state :connected)
       [:section
        [:header "Players"]
        [:fieldset.session-players
         (let [{:keys [local/color]} (:root/local result)]
           [:div.session-player
            [:div.session-player-color {:style {:background-color color}}]
            [:div.session-player-label "You"]])
         (if (seq conns)
           (for [{:local/keys [color type] :as conn} conns]
             [:div.session-player {:key (:entity/key conn)}
              [:div.session-player-color {:style {:background-color color}}]
              [:div.session-player-label
               (case type
                 :conn "Player"
                 :host "Host"
                 nil)]])
           [:span "No one else is here, yet."])]])]))

(defmethod form :session []
  session-form)
