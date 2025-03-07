(ns ogres.app.component.panel-lobby
  (:require [ogres.app.component :as component :refer [icon]]
            [ogres.app.const :refer [VERSION]]
            [ogres.app.hooks :as hooks]
            [ogres.app.provider.release :as release]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query-footer
  [{:root/user
    [[:user/type :default :conn]
     [:session/status :default :initial]]}
   {:root/session [:session/room]}])

(defn ^:private session-url [room-key]
  (let [params (js/URLSearchParams. #js {"r" VERSION "join" room-key})
        origin (.. js/window -location -origin)
        path   (.. js/window -location -pathname)]
    (str origin path "?" (.toString params))))

(defui ^:private player-tile [props]
  (let [{:keys [user editable]} props]
    ($ :.player-tile
      {:data-color (:user/color user)
       :data-editable editable}
      ($ :input {:type "hidden" :name "id" :value (:user/uuid user)})
      ($ :.player-tile-color)
      ($ :button.player-tile-image
        {:disabled (not editable)}
        ($ :.player-tile-image-frame)
        (if (not (nil? (:user/image user)))
          ($ :.player-tile-image-content)
          ($ :.player-tile-image-default
            ($ component/icon {:name "dnd" :size 46})))
        ($ :.player-tile-image-edit "Change portrait"))
      ($ :.player-tile-content
        ($ :input.player-tile-input
          {:type "text"
           :name "label"
           :disabled (not editable)
           :default-value (:user/label user)
           :placeholder (if editable "Name" "")
           :auto-focus (= (:user/label user) "")
           :auto-complete "off"})
        ($ :input.player-tile-input
          {:type "text"
           :name "description"
           :disabled (not editable)
           :default-value (:user/description user)
           :placeholder (if editable "Description" "")
           :auto-complete "off"})))))

(def ^:private tokens-query
  [{:root/user [{:user/image [:image/hash]}]}
   {:root/token-images
    [:image/hash
     :image/scope
     {:image/thumbnail
      [:image/hash]}]}])

(defui ^:private tokens [{:keys [on-change]}]
  (let [[page set-page] (uix/use-state 1)
        result (hooks/use-query tokens-query [:db/ident :root])
        select (:image/hash (:user/image (:root/user result)))
        public (into [] (filter (comp #{:public} :image/scope)) (:root/token-images result))
        limit  10
        pages  (js/Math.ceil (/ (count public) limit))
        start  (max (* (min (dec page) pages) limit) 0)
        stop   (min (+ start limit) (count public))
        images (subvec public start stop)]
    (if (> (count public) 0)
      ($ :.session-tokens {:data-paginated (> pages 1)}
        ($ :.session-tokens-gallery
          (for [idx (range limit)]
            (if-let [image (get images idx)]
              (let [{hash :image/hash {display :image/hash} :image/thumbnail} image]
                ($ component/image {:key display :hash display}
                  (fn [url]
                    ($ :button.session-tokens-image
                      {:style {:background-image (str "url(" url ")")}
                       :on-click (fn [] (on-change hash))
                       :data-selected (= select hash)}))))
              ($ :button.session-tokens-placeholder
                {:key idx :disabled true}))))
        (if (> pages 1)
          ($ :.session-tokens-pagination
            ($ component/pagination
              {:name "tokens-user-image"
               :pages pages
               :value page
               :on-change
               (fn [page]
                 (set-page page))})))))))

(def ^:private character-form-query
  [:db/id
   :user/uuid
   :user/color
   [:user/label :default ""]
   [:user/description :default ""]
   {:user/image [{:image/thumbnail [:image/hash]}]}])

(defui ^:private character-form []
  (let [dispatch (hooks/use-dispatch)
        user (hooks/use-query character-form-query)]
    ($ :form
      {:on-submit
       (fn [event]
         (.preventDefault event)
         (let [input (.. event -target -elements)
               label (.. input -label -value)
               descr (.. input -description -value)]
           (dispatch :user/change-details label descr)))
       :on-blur
       (fn [event]
         (let [name  (.. event -target -name)
               value (.. event -target -value)]
           (cond (= name "label")       (dispatch :user/change-label value)
                 (= name "description") (dispatch :user/change-description value))))}
      ($ :fieldset.fieldset
        ($ :legend "Your character")
        ($ player-tile {:user user :editable true})))))

(def ^:private form-query
  [{:root/user
    [:db/id
     :user/uuid
     :user/type
     [:session/status :default :initial]
     [:user/share-cursor :default true]]}
   {:root/session
    [:session/room
     {:session/host [:db/id :user/uuid :user/color]}
     [:session/share-cursors :default true]
     {:session/conns
      [:db/id
       :user/uuid
       :user/color
       [:user/type :default :conn]
       [:user/label :default ""]
       [:user/description :default ""]
       {:user/image
        [:image/hash
         {:image/thumbnail
          [:image/hash]}]}]}]}])

(defui form []
  (let [releases (uix/use-context release/context)
        dispatch (hooks/use-dispatch)
        result   (hooks/use-query form-query [:db/ident :root])
        {{:session/keys [room share-cursors]} :root/session
         {:user/keys [uuid type share-cursor]} :root/user
         {status :session/status} :root/user} result]
    (if (#{:connecting :connected :disconnected} status)
      ($ :.form-session.session
        ($ :header ($ :h2 "Lobby"))
        (if (and (= type :host) (some? room) (seq releases) (not= VERSION (last releases)))
          ($ :div
            ($ :.form-notice {:style {:margin-bottom 16}}
              ($ :p ($ :strong "Warning: ")
                "You're not using the latest version of this application.
                 Either upgrade to the latest version or make sure that
                 players connect using the fully qualified URL below."))
            ($ :fieldset.fieldset
              ($ :legend "Fully qualified URL")
              ($ :input.text.text-ghost.session-url {:type "text" :value (session-url room) :readOnly true}))))
        (if room
          ($ :fieldset.fieldset
            ($ :legend "Room Code")
            ($ :.session-room
              ($ :input.text-ghost.session-code
                {:type "text" :value room :readOnly true})
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
                   :checked share-cursors
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
                   :checked share-cursor
                   :on-change
                   (fn [event]
                     (let [checked (.. event -target -checked)]
                       (dispatch :session/toggle-share-my-cursor checked)))})
                ($ icon {:name "check" :size 20})
                "Share my cursor"))))
        (if (= type :conn)
          ($ character-form {}))
        ($ :section.session-players
          (let [xf (comp (filter (comp (complement #{uuid}) :user/uuid))
                         (filter (comp #{:conn} :user/type)))]
            (for [user (into [] xf (:session/conns (:root/session result)))]
              ($ player-tile
                {:key (:user/uuid user)
                 :user user
                 :editable (= type :host)})))))
      ($ :<>
        ($ :header ($ :h2 "Lobby"))
        ($ :.prompt
          "Invite your friends to this virtual tabletop by clicking
           the 'Start online game' button and sharing the room code
           or URL with them.")))))

(defui footer []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query-footer [:db/ident :root])
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
