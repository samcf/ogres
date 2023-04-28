(ns ogres.app.form.session
  (:require [ogres.app.env :as env]
            [ogres.app.form.render :as render]
            [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render :refer [icon]]))

(def ^:private query-header
  [{:root/local
    [[:local/type :default :conn]
     [:session/state :default :initial]]}
   {:root/session
    [{:session/conns [:db/key]}]}])

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
     [:session/share-cursor :default true]]}
   {:root/session
    [:session/room
     {:session/conns [:db/key :local/color :local/type]}
     {:session/host [:db/key :local/color]}
     [:session/share-cursors :default true]]}])

(defn- session-url [room-key]
  (str (.. js/window -location -origin) "?r=" env/VERSION "&join=" room-key))

(defn- header []
  (let [result (use-query query-header [:db/ident :root])
        {{type  :local/type
          state :session/state} :root/local
         {conns :session/conns} :root/session} result]
    (case [type state]
      ([:host :connecting] [:conn :connecting])
      [:.session-status {:css {state true}} "Status: Connecting"]
      ([:conn :connected] [:host :connected])
      [:.session-status {:css {state true}} "Status: Connected [" (inc (count conns)) "]"]
      [:host :disconnected]
      [:.session-status {:css {state true}} "Status: Disconnected"]
      [:conn :disconnected]
      [:.session-status {:css {state true}} "Status: Reconnecting..."]
      nil)))

(defn- form []
  (let [dispatch (use-dispatch)
        result   (use-query query-form [:db/ident :root])
        {{host          :session/host
          conns         :session/conns
          share-cursors :session/share-cursors} :root/session
         {share-cursor :session/share-cursor
          state        :session/state
          type         :local/type
          key          :db/key} :root/local
         local :root/local} result]
    (if (or (= state :connected) (= state :disconnected) (= state :connecting))
      [:section.session
       [:section
        [:header "Options"]
        [:fieldset.checkbox
         [:input
          {:id "share-cursors"
           :type "checkbox"
           :checked share-cursors
           :disabled (not= type :host)
           :on-change
           (fn [event]
             (let [checked (.. event -target -checked)]
               (dispatch :session/toggle-share-cursors checked)))}]
         [:label {:for "share-cursors"} "Share cursors"]]
        [:fieldset.checkbox
         [:input
          {:id "share-my-cursor"
           :type "checkbox"
           :checked share-cursor
           :disabled false
           :on-change
           (fn [event]
             (let [checked (.. event -target -checked)]
               (dispatch :session/toggle-share-my-cursor checked)))}]
         [:label {:for "share-my-cursor"} "Share my cursor"]]]
       [:section
        [:header "Host"]
        [:div.session-players
         (if host
           [:div.session-player
            [:div.session-player-color {:style {:background-color (:local/color host)}}]
            [:div.session-player-label "Host"
             (if (= (:db/key host) key)
               [:span " (You)"])]]
           [:div.prompt "Not connected."])]]
       [:section
        [:header (str "Players" " [" (count conns) "]")]
        [:div.session-players
         (if (seq conns)
           (let [xf (filter (fn [entity] (= (:local/type entity) :conn)))]
             (for [conn (->> (conj conns local) (sequence xf) (sort-by :db/key))]
               [:div.session-player {:key (:db/key conn)}
                [:div.session-player-color {:style {:background-color (:local/color conn)}}]
                [:div.session-player-label "Friend"
                 (if (= (:db/key conn) key)
                   [:span " (You)"])]]))
           [:div.prompt "No one else is here."])]]]
      [:section.session
       [:div.prompt
        "Invite your friends to this virtual tabletop"
        [:br] "by clicking the 'Create Room' button above"
        [:br] "and sharing the URL with them."]])))

(defn- footer []
  (let [dispatch (use-dispatch)
        result   (use-query query-footer [:db/ident :root])
        {{state :session/state type :local/type} :root/local
         {room-key :session/room} :root/session} result]
    [:<>
     [:button.button.button-primary
      {:type "button"
       :title "Create room"
       :on-click #(dispatch :session/request)
       :disabled (or (= state :connecting) (= state :connected) (not= type :host))}
      [icon {:name "globe-americas" :size 16}]
      (case [type state]
        [:host :initial]      "Create Room"
        [:host :connected]    "Connected"
        [:host :disconnected] "Recreate Room"
        [:host :connecting]   "Connecting"
        [:conn :initial]      "No Such Room"
        [:conn :connected]    "Connected"
        [:conn :disconnected] "Reconnecting"
        [:conn :connecting]   "Reconnecting")]
     [:button.button.button-neutral
      {:type "button"
       :title "Copy the room URL"
       :disabled (not= state :connected)
       :on-click #(.. js/window -navigator -clipboard (writeText (session-url room-key)))}
      [icon {:name "clipboard" :size 18}] "Copy"]
     [:button.button.button-danger
      {:type "button"
       :title "Disconnect"
       :disabled (or (not= state :connected) (not= type :host))
       :on-click #(dispatch :session/close)}
      [icon {:name "wifi-off" :size 16}]
      "Quit"]]))

(defmethod render/header :session [] header)
(defmethod render/form :session [] form)
(defmethod render/footer :session [] footer)
