(ns ogre.tools.form.session
  (:require [ogre.tools.form.render :refer [form]]
            [ogre.tools.state :refer [use-query]]
            [uix.core.alpha :as uix]))

(def query
  [{:root/local
    [:entity/key
     [:session/state :default :initial]]}
   {:root/session
    [:session/room
     {:session/conns [:entity/key]}
     {:session/host [:entity/key]}]}])

(defn session-url [room-key]
  (str (.. js/window -location -origin) "?join=" room-key))

(defmethod form :session []
  (fn [_]
    (let [[result dispatch] (use-query query [:db/ident :root])
          {{state :session/state} :root/local
           {room  :session/room
            conns :session/conns} :root/session} result]

      [:<>
       [:section [:header "Invite Friends"]]
       [:section {:style {:display "flex" :justify-content "space-between" :gap 4}}
        [:button.ogre-button
         {:type "button" :on-click #(dispatch :session/request) :style {:flex 2}
          :disabled (#{:connecting :connected} state)}
         "Start New Session"]
        [:button.ogre-button
         {:type "button" :on-click #(dispatch :session/close) :style {:flex 1}
          :disabled (#{:initial :disconnected} state)}
         "Disconnect"]]
       (if (= state :connected)
         (let [url (session-url room)]
           [:section
            [:fieldset.session-share
             [:input {:type "text" :value url :readOnly true :on-click (fn [event] (.select (.-target event)))}]
             [:label "Invite players to join this game by sharing this link
                      with them. When you disconnect or close the browser tab,
                      the entire session will be closed."]]]))
       (if (= state :connected)
         [:section.session-players
          [:header "Players"]
          (if (seq conns)
            (for [{:keys [entity/key]} conns]
              [:div.session-player {:key key} key])
            [:em "No one is here, yet."])])])))
