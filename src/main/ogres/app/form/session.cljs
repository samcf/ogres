(ns ogres.app.form.session
  (:require [ogres.app.const :refer [VERSION]]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render :refer [icon]]
            [ogres.app.util :refer [comp-fn]]
            [uix.core :refer [defui $]]))

(def ^:private query-footer
  [{:root/local
    [[:local/type :default :conn]
     [:session/state :default :initial]]}
   {:root/session [:session/room]}])

(def ^:private query-form
  [{:root/local
    [:db/key
     :local/type
     :local/color
     [:session/state :default :initial]
     [:local/share-cursor :default true]]}
   {:root/session
    [:session/room
     {:session/conns [:db/key :local/color :local/type]}
     {:session/host [:db/key :local/color]}
     [:session/share-cursors :default true]]}])

(defn ^:private session-url [room-key]
  (str (.. js/window -location -origin) "?r=" VERSION "&join=" room-key))

(defui form []
  (let [dispatch (use-dispatch)
        result   (use-query query-form [:db/ident :root])
        {{code    :session/room
          host    :session/host
          conns   :session/conns
          cursors :session/share-cursors} :root/session
         {share :local/share-cursor
          state :session/state
          type  :local/type
          key   :db/key} :root/local
         local :root/local} result]
    (if (#{:connecting :connected :disconnected} state)
      ($ :section.session
        (if code
          ($ :section
            ($ :header "Room Code")
            ($ :code.session-code code)))
        ($ :section
          ($ :header "Options")
          ($ :fieldset.checkbox
            ($ :input
              {:id "share-cursors"
               :type "checkbox"
               :checked cursors
               :disabled (not= type :host)
               :on-change
               (fn [event]
                 (let [checked (.. event -target -checked)]
                   (dispatch :session/toggle-share-cursors checked)))})
            ($ :label {:for "share-cursors"} "Share cursors"))
          ($ :fieldset.checkbox
            ($ :input
              {:id "share-my-cursor"
               :type "checkbox"
               :checked share
               :disabled false
               :on-change
               (fn [event]
                 (let [checked (.. event -target -checked)]
                   (dispatch :session/toggle-share-my-cursor checked)))})
            ($ :label {:for "share-my-cursor"} "Share my cursor")))
        ($ :section
          ($ :header "Host")
          ($ :div.session-players
            (if host
              ($ :div.session-player
                ($ :div.session-player-color {:style {:background-color (:local/color host)}})
                ($ :div.session-player-label "Host"
                  (if (= (:db/key host) key)
                    ($ :span " (You)"))))
              ($ :div.prompt "Not connected."))))
        ($ :section
          ($ :header (str "Players" " [" (count conns) "]"))
          ($ :div.session-players
            (if (seq conns)
              (let [xf (filter (comp-fn = :local/type :conn))]
                (for [conn (->> (conj conns local) (sequence xf) (sort-by :db/key))]
                  ($ :div.session-player {:key (:db/key conn)}
                    ($ :div.session-player-color {:style {:background-color (:local/color conn)}})
                    ($ :div.session-player-label "Friend"
                      (if (= (:db/key conn) key)
                        ($ :span " (You)"))))))
              ($ :div.prompt "No one else is here.")))))
      ($ :section.session
        ($ :div.prompt
          "Invite your friends to this virtual tabletop"
          ($ :br) "by clicking the 'Create Room' button"
          ($ :br) "and sharing the room code or URL with them.")))))

(defui footer []
  (let [dispatch (use-dispatch)
        result   (use-query query-footer [:db/ident :root])
        {{state :session/state type :local/type} :root/local
         {room-key :session/room} :root/session} result]
    ($ :<>
      ($ :button.button.button-primary
        {:type "button"
         :title "Create room"
         :on-click #(dispatch :session/request)
         :disabled (or (= state :connecting) (= state :connected) (not= type :host))}
        ($ icon {:name "globe-americas" :size 16})
        (case [type state]
          [:host :initial]      "Create Room"
          [:host :connected]    "Connected"
          [:host :disconnected] "Recreate Room"
          [:host :connecting]   "Connecting"
          [:conn :initial]      "No Such Room"
          [:conn :connected]    "Connected"
          [:conn :disconnected] "Reconnecting"
          [:conn :connecting]   "Reconnecting"))
      ($ :button.button.button-neutral
        {:type "button"
         :title "Copy the room URL"
         :disabled (not= state :connected)
         :on-click #(.. js/window -navigator -clipboard (writeText (session-url room-key)))}
        ($ icon {:name "clipboard" :size 18}) "Copy")
      ($ :button.button.button-danger
        {:type "button"
         :title "Disconnect"
         :disabled (or (not= state :connected) (not= type :host))
         :on-click #(dispatch :session/close)}
        ($ icon {:name "wifi-off" :size 16})
        "Quit"))))
