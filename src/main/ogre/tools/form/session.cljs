(ns ogre.tools.form.session
  (:require [ogre.tools.form.render :refer [form]]
            [ogre.tools.state :refer [use-query]]
            [ogre.tools.render :refer [icon]]))

(def query
  [{:root/local [:entity/key]}
   {:root/session
    [:session/room
     {:session/conns [:entity/key]}
     {:session/host [:entity/key]}
     [:session/state :default :disconnected]]}])

(defn session-status [state]
  (case state
    nil           "Not Started"
    :disconnected "Disconnected"
    :connecting   "Connecting"
    :connected    "Connected"))

(defn session-url [session]
  (str (.. js/window -location -origin) "?join=" (:session/room session)))

(defmethod form :session []
  (fn [props]
    (let [[result dispatch] (use-query query [:db/ident :root])
          {local :root/local
           session :root/session
           {host  :session/host
            room  :session/room
            conns :session/conns
            state :session/state} :root/session} result]
      [:<>
       [:section [:header "Invite Friends"]]
       [:section
        [:span "Status: " (session-status state)]
        [:button {:type "button" :on-click #(dispatch :session/request)} "Invite Friends"]
        (let [url (session-url session)]
          [:fieldset {:style {:display "flex" :align-items "center" :justify-content "space-between" :gap "4px"}}
           [:input {:type "text" :readOnly true :value url}]
           [:button {:type "button" :data-tooltip "foo" :on-click (fn [] (.. js/window -navigator -clipboard (writeText url)))}
            [icon {:name "clipboard" :size 20}]]])]
       [:section [:header "Players"]]
       [:section
        (for [{:keys [entity/key]} conns]
          [:div {:key key} key])]])))
