(ns ogre.tools.form.session
  (:require [clojure.string :refer [capitalize]]
            [ogre.tools.form.render :refer [form]]
            [ogre.tools.state :refer [use-query]]))

(def ^{:private true} query
  [{:root/local
    [:entity/key
     :local/type
     [:session/state :default :initial]]}
   {:root/session
    [:session/room
     {:session/conns [:entity/key]}
     {:session/host [:entity/key]}]}])

(defn ^{:private true} session-url [room-key]
  (str (.. js/window -location -origin) "?join=" room-key))

(defn ^{:private true} session-form []
  (let [[result dispatch] (use-query query [:db/ident :root])
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
         (if (seq conns)
           (for [{:keys [entity/key]} conns]
             [:div.session-player {:key key} key])
           [:em "No one is here, yet."])]])]))

(defmethod form :session []
  session-form)
