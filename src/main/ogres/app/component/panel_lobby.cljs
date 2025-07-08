(ns ogres.app.component.panel-lobby
  (:require [ogres.app.component :as component :refer [icon]]
            [ogres.app.component.player-tile :refer [player-tile]]
            [ogres.app.const :refer [VERSION]]
            [ogres.app.hooks :as hooks]
            [ogres.app.provider.release :as release]
            [uix.core :as uix :refer [defui $]]))

(def ^:private query-tokens
  [{:root/user
    [[:user/host :default true]]}
   {:root/token-images
    [:image/hash
     :image/name
     :image/public
     {:image/thumbnail
      [:image/hash]}]}])

(def ^:private query-user
  [:user/uuid
   [:user/host :default true]
   [:user/color :default "red"]
   [:user/label :default ""]
   [:user/description :default ""]
   [:user/share-cursor :default true]
   [:session/status :default :initial]
   {:user/image
    [:image/hash
     {:image/thumbnail
      [:image/hash]}]}])

(def ^:private query-panel
  [{:root/user query-user}
   {:root/session
    [:session/room
     {:session/host [:user/uuid :user/color]}
     [:session/share-cursors :default true]
     {:session/conns query-user}]}])

(def ^:private query-actions
  [{:root/user
    [[:user/host :default true]
     [:session/status :default :initial]]}
   {:root/session [:session/room]}])

(defn ^:private players-xf [uuid]
  (comp (filter (comp (complement #{uuid}) :user/uuid))
        (filter (comp not :user/host))))

(defn ^:private tokens-xf [host]
  (if host
    (map identity)
    (filter :image/public)))

(defn ^:private session-url [room-key]
  (let [params (js/URLSearchParams. #js {"r" VERSION "join" room-key})
        origin (.. js/window -location -origin)
        path   (.. js/window -location -pathname)]
    (str origin path "?" (.toString params))))

(defui ^:private upload-button []
  (let [upload (hooks/use-image-uploader {:type :token})
        input  (uix/use-ref nil)]
    ($ :button.button
      {:on-click (fn [] (.click (deref input)))}
      ($ :input
        {:type "file"
         :hidden true
         :accept "image/*"
         :multiple true
         :ref input
         :on-change
         (fn [event]
           (upload (.. event -target -files))
           (set! (.. event -target -value) ""))})
      ($ icon {:name "camera-fill" :size 16})
      "Upload image")))

(defui ^:private tokens
  [{:keys [selected on-change]}]
  (let [[page set-page] (uix/use-state 1)
        {{host :user/host} :root/user
         tokens :root/token-images}
        (hooks/use-query query-tokens [:db/ident :root])
        limit  10
        tokens (into [] (tokens-xf host) tokens)
        pages  (js/Math.ceil (/ (count tokens) limit))
        start  (max (* (min (dec page) pages) limit) 0)
        stop   (min (+ start limit) (count tokens))
        images (subvec tokens start stop)]
    ($ :.player-tokens {:data-paginated (> pages 1)}
      ($ :.player-tokens-gallery
        (for [idx (range limit)]
          (if-let [image (get images idx)]
            (let [{hash :image/hash name :image/name {display :image/hash} :image/thumbnail} image]
              ($ component/image {:key display :hash display}
                (fn [url]
                  ($ :label.player-tokens-item.player-tokens-image
                    {:style {:background-image (str "url(" url ")")} :aria-label name}
                    ($ :input
                      {:type "radio"
                       :name "player-token"
                       :value hash
                       :checked (= selected hash)
                       :on-change
                       (fn [event]
                         (on-change (.. event -target -value)))})))))
            ($ :.player-tokens-item.player-tokens-placeholder
              {:key idx}))))
      ($ :.player-tokens-actions
        ($ upload-button {})
        ($ component/pagination
          {:name "tokens-user-image"
           :pages (max pages 1)
           :value page
           :on-change
           (fn [page]
             (set-page page))})))))

(defui ^:private player-form
  [{:keys [user]}]
  (let [selected (:image/hash (:user/image user))
        [display-tokens? set-display-tokens] (uix/use-state (nil? selected))
        dispatch (hooks/use-dispatch)]
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
      ($ player-tile
        {:user user
         :editable true
         :auto-focus true
         :on-click-portrait (fn [] (set-display-tokens not))})
      (if display-tokens?
        ($ :.session-player-form-tokens
          ($ tokens
            {:selected selected
             :on-change
             (fn [hash]
               (dispatch :user/change-image hash))}))))))

(defui ^:private players-form
  [{:keys [users editable]}]
  (let [[display-tokens set-display-tokens] (uix/use-state nil)
        dispatch (hooks/use-dispatch)]
    ($ :form.session-players-form
      {:on-submit (fn [event] (.preventDefault event))
       :on-blur
       (fn [event]
         (let [input (.-target event)
               value (.-value input)
               group (.closest input "fieldset")
               ident (.querySelector group "[name='id']")
               uuid  (uuid (.-value ident))]
           (case (.-name input)
             "label"       (dispatch :user/change-label       uuid value)
             "description" (dispatch :user/change-description uuid value)
             nil)))}
      (for [{:keys [user/uuid] :as user} users]
        ($ :fieldset.session-player-form {:key uuid}
          ($ player-tile
            {:user user
             :editable editable
             :on-click-portrait
             (fn []
               (set-display-tokens
                (fn [current]
                  (if (= current uuid) nil uuid))))})
          (if (= display-tokens uuid)
            ($ :.session-player-form-tokens
              ($ tokens
                {:selected (:image/hash (:user/image user))
                 :on-change
                 (fn [hash]
                   (dispatch :user/change-image uuid hash))}))))))))

(defui ^:memo panel []
  (let [releases (uix/use-context release/context)
        dispatch (hooks/use-dispatch)
        result   (hooks/use-query query-panel [:db/ident :root])
        user     (:root/user result)
        {{room-code :session/room} :root/session
         {host :user/host
          user-status :session/status} :root/user} result]
    (if (or (= user-status :connecting) (= user-status :connected) (= user-status :disconnected))
      ($ :.form-session.session
        ($ :header ($ :h2 "Lobby"))
        (if (and host (some? room-code) (seq releases) (not= VERSION (last releases)))
          ($ :div
            ($ :.form-notice {:style {:margin-bottom 16}}
              ($ :p ($ :strong "Warning: ")
                "You're not using the latest version of this application.
                 Either upgrade to the latest version or make sure that
                 players connect using the fully qualified URL below."))
            ($ :fieldset.fieldset
              ($ :legend "Fully qualified URL")
              ($ :input.text.text-ghost.session-url
                {:type "text"
                 :value (session-url room-code)
                 :readOnly true}))))
        (if (some? room-code)
          ($ :fieldset.fieldset
            ($ :legend "Room Code")
            ($ :.session-room
              ($ :input.text-ghost.session-code
                {:type "text" :value room-code :readOnly true})
              (let [url (.. js/window -location -origin)]
                ($ :.form-notice
                  "Players can join your room by going to "
                  ($ :a {:href url :target "_blank"} url)
                  " and entering this code.")))))
        (if host
          ($ :fieldset.fieldset
            ($ :legend "Options")
            ($ :fieldset.session-options
              ($ :.input-group
                ($ :label.checkbox
                  ($ :input
                    {:type "checkbox"
                     :checked (:session/share-cursors (:root/session result))
                     :aria-disabled (not host)
                     :on-change
                     (fn [event]
                       (if host
                         (let [checked (.. event -target -checked)]
                           (dispatch :session/toggle-share-cursors checked))))})
                  ($ icon {:name "check" :size 20})
                  "Share cursors")
                ($ :label.checkbox
                  ($ :input
                    {:type "checkbox"
                     :checked (:user/share-cursor user)
                     :on-change
                     (fn [event]
                       (let [checked (.. event -target -checked)]
                         (dispatch :session/toggle-share-my-cursor checked)))})
                  ($ icon {:name "check" :size 20})
                  "Share my cursor")))))
        (if (not host)
          ($ :fieldset.fieldset
            ($ :legend "Your character")
            ($ player-form {:user user})))
        (let [conns (sequence (players-xf (:user/uuid user)) (:session/conns (:root/session result)))]
          (if (seq conns)
            ($ :fieldset.fieldset-flat
              ($ :legend "Players")
              ($ :section.session-players
                ($ players-form
                  {:users (sequence (players-xf (:user/uuid user)) conns)
                   :editable host}))))))
      ($ :<>
        ($ :header ($ :h2 "Lobby"))
        ($ :.prompt
          "Invite your friends to this virtual tabletop by clicking
           the 'Start online game' button and sharing the room code
           or URL with them.")))))

(defui ^:memo actions []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query-actions [:db/ident :root])
        {{status :session/status host :user/host} :root/user
         {room-key :session/room} :root/session} result]
    ($ :<>
      ($ :button.button.button-primary
        {:type "button"
         :on-click #(dispatch :session/request)
         :disabled (or (= status :connecting) (= status :connected) (not host))}
        ($ icon {:name "globe-americas" :size 16})
        (case [host status]
          [true :initial]      "Start online game"
          [true :connected]    "Connected"
          [true :disconnected] "Restart"
          [true :connecting]   "Connecting"
          [false :connected]   "Connected"))
      ($ :button.button.button-neutral
        {:type "button"
         :title "Share room link"
         :disabled (not= status :connected)
         :on-click #(.. js/window -navigator -clipboard (writeText (session-url room-key)))}
        ($ icon {:name "share-fill" :size 14}) "Share")
      ($ :button.button.button-danger
        {:type "button"
         :title "Disconnect"
         :disabled (or (not host) (not= status :connected))
         :on-click #(dispatch :session/close)}
        ($ icon {:name "wifi-off" :size 16})
        "Quit"))))
