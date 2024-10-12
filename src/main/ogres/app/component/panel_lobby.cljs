(ns ogres.app.component.panel-lobby
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.const :refer [VERSION]]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.provider.release :as release]
            [uix.core :refer [defui $ use-context]]))

(def ^:private query-footer
  [{:root/user
    [[:user/type :default :conn]
     [:session/status :default :initial]]}
   {:root/session [:session/room]}])

(def ^:private query-form
  [{:root/user
    [:db/id
     :user/uuid
     :user/type
     :user/color
     [:session/status :default :initial]
     [:user/share-cursor :default true]]}
   {:root/session
    [:session/room
     {:session/conns [:db/id :user/uuid :user/color :user/type]}
     {:session/host [:db/id :user/uuid :user/color]}
     [:session/share-cursors :default true]]}])

(defn ^:private session-url [room-key]
  (let [params (js/URLSearchParams. #js {"r" VERSION "join" room-key})
        origin (.. js/window -location -origin)
        path   (.. js/window -location -pathname)]
    (str origin path "?" (.toString params))))

(defui form []
  (let [releases (use-context release/context)
        dispatch (use-dispatch)
        result   (use-query query-form [:db/ident :root])
        {{code    :session/room
          host    :session/host
          conns   :session/conns
          cursors :session/share-cursors} :root/session
         {share :user/share-cursor
          state :session/status
          type  :user/type
          id    :db/id} :root/user} result]
    (if (#{:connecting :connected :disconnected} state)
      ($ :.form-session.session
        ($ :header ($ :h2 "Lobby"))
        (if (and (= type :host) (some? code) (seq releases) (not= VERSION (last releases)))
          ($ :div
            ($ :.form-notice {:style {:margin-bottom 16}}
              ($ :p ($ :strong "Warning: ")
                "You're not using the latest version of this application.
                 Either upgrade to the latest version or make sure that
                 players connect using the fully qualified URL below."))
            ($ :fieldset.fieldset
              ($ :legend "Fully qualified URL")
              ($ :input.text.text-ghost.session-url {:type "text" :value (session-url code) :readOnly true}))))
        (if code
          ($ :fieldset.fieldset
            ($ :legend "Room Code")
            ($ :.session-room
              ($ :input.text-ghost.session-code
                {:type "text" :value code :readOnly true})
              (let [url (.. js/window -location -origin)]
                ($ :.form-notice
                  "Players can join your room by going to "
                  ($ :a {:href url :target "_blank"} url)
                  " and entering this code.")))))
        ($ :fieldset.fieldset
          ($ :legend "Options")
          ($ :fieldset.session-options
            ($ :.input-group
              ($ :label.checkbox
                ($ :input
                  {:type "checkbox"
                   :checked cursors
                   :aria-disabled (not= type :host)
                   :on-change
                   (fn [event]
                     (if (= type :host)
                       (let [checked (.. event -target -checked)]
                         (dispatch :session/toggle-share-cursors checked))))})
                ($ icon {:name "check" :size 20})
                "Share cursors")
              ($ :label.checkbox
                ($ :input
                  {:type "checkbox"
                   :checked share
                   :on-change
                   (fn [event]
                     (let [checked (.. event -target -checked)]
                       (dispatch :session/toggle-share-my-cursor checked)))})
                ($ icon {:name "check" :size 20})
                "Share my cursor"))))
        ($ :fieldset.fieldset
          ($ :legend "Host")
          ($ :.session-players
            (if host
              ($ :.session-player
                ($ :.session-player-color {:data-color (:user/color host)})
                ($ :.session-player-label "Host"
                  (if (= (:db/id host) id)
                    ($ :span " ( You )"))))
              ($ :.prompt "Not connected."))))
        ($ :fieldset.fieldset
          ($ :legend (str "Players"))
          ($ :.session-players
            (if (seq conns)
              (let [xf (filter (comp #{:conn} :user/type))]
                (for [conn (->> conns (sequence xf) (sort-by :db/id))]
                  ($ :.session-player {:key (:db/id conn)}
                    ($ :.session-player-color {:data-color (:user/color conn)})
                    ($ :.session-player-label "Friend"
                      (if (= (:db/id conn) id)
                        ($ :span " ( You )"))))))
              ($ :.prompt "No one else is here.")))))
      ($ :<>
        ($ :header ($ :h2 "Lobby"))
        ($ :.prompt
          "Invite your friends to this virtual tabletop by clicking
           the 'Start online game' button and sharing the room code
           or URL with them.")))))

(defui footer []
  (let [dispatch (use-dispatch)
        result   (use-query query-footer [:db/ident :root])
        {{status :session/status type :user/type} :root/user
         {room-key :session/room} :root/session} result]
    ($ :<>
      ($ :button.button.button-primary
        {:type "button"
         :on-click #(dispatch :session/request)
         :disabled (or (= status :connecting) (= status :connected) (not= type :host))}
        ($ icon {:name "globe-americas" :size 16})
        (case [type status]
          [:host :initial]      "Start online game"
          [:host :connected]    "Connected"
          [:host :disconnected] "Restart"
          [:host :connecting]   "Connecting"
          [:conn :connected]    "Connected"))
      ($ :button.button.button-neutral
        {:type "button"
         :title "Share room link"
         :disabled (not= status :connected)
         :on-click #(.. js/window -navigator -clipboard (writeText (session-url room-key)))}
        ($ icon {:name "share-fill" :size 14}) "Share")
      ($ :button.button.button-danger
        {:type "button"
         :title "Disconnect"
         :disabled (or (not= status :connected) (not= type :host))
         :on-click #(dispatch :session/close)}
        ($ icon {:name "wifi-off" :size 16})
        "Quit"))))
