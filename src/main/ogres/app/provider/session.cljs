(ns ogres.app.provider.session
  (:require [cognitect.transit :as transit]
            [datascript.core :as ds]
            [goog.functions :refer [throttle]]
            [ogres.app.const :refer [SOCKET-URL]]
            [ogres.app.hooks :as hooks]
            [ogres.app.provider.idb :as idb]
            [ogres.app.provider.state :as state]
            [ogres.app.serialize :refer [reader writer]]
            [ogres.app.vec :as vec]
            [uix.core :as uix :refer [defui]]
            ["@msgpack/msgpack" :as MessagePack]))

(def ^:private interval-heartbeat 20000)
(def ^:private interval-reconnect 5000)

(def ^:private color-options
  ["blue" "yellow" "green" "purple" "orange"])

(defn ^:private next-color [data colors]
  (let [session (ds/entity data [:db/ident :session])
        conns   (:session/conns session)
        taken   (into #{} (map :user/color) conns)]
    (or (first (filter (complement taken) colors))
        (first (shuffle colors)))))

(defn ^:private initialize-player-state [data uuid]
  (let [{{host :user/uuid} :session/host}
        (ds/pull data [{:session/host [:user/uuid]}] [:db/ident :session])]
    (ds/db-with
     data
     [[:db/retract [:user/uuid host] :db/ident]
      [:db/add [:user/uuid uuid] :db/ident :user]
      [:db/add [:db/ident :root] :root/user [:user/uuid uuid]]
      [:db/add [:db/ident :session] :session/conns [:user/uuid host]]])))

(defmulti ^:private on-receive-text
  (fn [{:keys [type]}] type))

(defmethod on-receive-text :default
  [_ _ _ _ _ _])

;; Handles messages that are sent by the server, some of which are responses to
;; creating or connecting to a session or to notify connections within a session
;; of important events. The embedded :data map will always contain a member
;; called :name to distinguish different kinds of events.
(defmethod on-receive-text :event
  [{:keys [data] :as message} conn publish images on-send on-send-binary]
  (case (:name data)
    :session/created
    (ds/transact!
     conn
     [[:db/add [:db/ident :user] :user/uuid (:uuid data)]
      [:db/add [:db/ident :user] :session/status :connected]
      [:db/add [:db/ident :user] :session/last-room (:room data)]
      [:db/add [:db/ident :session] :session/room (:room data)]])

    :session/joined
    (ds/transact!
     conn
     [[:db/add [:db/ident :user] :user/uuid (:uuid data)]
      [:db/add [:db/ident :user] :session/status :connected]])

    :session/join
    (let [user (ds/entity @conn [:db/ident :user])]
      (if (= (:user/type user) :host)
        (let [tx-data
              [{:user/uuid (:uuid data)
                :user/type :conn
                :user/ready true
                :user/color (next-color @conn color-options)
                :user/cameras -1
                :session/status :connected
                :user/camera
                (let [{{point :camera/point
                        scale :camera/scale
                        {scene :db/id} :camera/scene}
                       :user/camera} user]
                  {:db/id -1
                   :camera/scene scene
                   :camera/point (or point vec/zero)
                   :camera/scale (or scale 1)})}
               [:db/add [:db/ident :session] :session/host [:db/ident :user]]
               [:db/add [:db/ident :session] :session/conns [:user/uuid (:uuid data)]]]
              report (ds/transact! conn tx-data)
              datoms (ds/datoms (:db-after report) :eavt)]
          (on-send {:type :datoms :data (into [] datoms) :dst (:uuid data)})
          (on-send {:type :tx :data (:tx-data report)}))))

    :session/leave
    (ds/transact! conn [[:db/retractEntity [:user/uuid (:uuid data)]]])

    :image/request
    (-> (images (:hash data))
        (.then (fn [data] (.-data data)))
        (.then (fn [data] (.arrayBuffer data)))
        (.then
         (fn [data]
           (on-send-binary
            #js {"data" (js/Uint8Array. data)
                 "dst"  (str (:src message))}))))

    :image/change-thumbnail-request
    (let [{{hash :hash bounds :bounds} :data} message]
      (publish :image/change-thumbnail hash bounds))

    :cursor/moved
    (let [{src :src {[x y] :coord} :data} message]
      (publish :cursor/moved src x y))))

;; Handles messages that include a complete copy of the host's initial state as
;; a set of DataScript datoms. This message is generally received right after
;; connecting to the session, but it may also be received periodically in an
;; attempt to correct any divergences in state.
(defmethod on-receive-text :datoms
  [{:keys [data dst]} conn _ _ _ _]
  (let [user-data (ds/db conn)
        user-vers (:root/release (ds/entity user-data [:db/ident :root]))
        host-data (ds/init-db data state/schema)
        host-vers (:root/release (ds/entity host-data [:db/ident :root]))]
    (if (not= user-vers host-vers)
      (let [params (js/URLSearchParams. (.. js/window -location -search))
            origin (.. js/window -location -origin)
            path   (.. js/window -location -pathname)]
        (.set params "r" host-vers)
        (.replace
         (.-location js/window)
         (str origin path "?" (.toString params))))
      (ds/reset-conn! conn (initialize-player-state host-data dst)))))

;; Handles messages that include a DataScript transaction from another
;; connection within the session. These messages can be received at any time
;; and from any connection besides its own.
(defmethod on-receive-text :tx
  [{:keys [data]} conn _ _ _ _]
  (ds/transact! conn data))

(defn ^:private on-receive-binary
  [message conn publish]
  (let [data (js/Blob. #js [(.-data message)] #js {"type" "image/jpeg"})
        user (ds/entity (ds/db conn) [:db/ident :user])]
    (case (:user/type user)
      :host (publish :image/create-token data)
      :conn (publish :image/cache data))))

(defui listeners []
  (let [[socket set-socket] (uix/use-state nil)
        dispatch (hooks/use-dispatch)
        publish  (hooks/use-publish)
        images   (idb/use-reader "images")
        conn     (uix/use-context state/context)
        on-send-text
        (uix/use-callback
         (fn [message]
           (if (and (some? socket) (= (.-readyState socket) 1))
             (let [user (ds/entity @conn [:db/ident :user])
                   data (merge {:time (js/Date.now) :src (:user/uuid user)} message)]
               (.send socket (transit/write writer data))))) [socket conn])
        on-send-binary
        (uix/use-callback
         (fn [message]
           (let [user (ds/entity @conn [:db/ident :user])
                 data (js/Object.assign
                       #js {}
                       #js {"time" (js/Date.now) "src" (str (:user/uuid user))}
                       message)]
             (.send socket (MessagePack/encode data)))) [socket conn])
        on-status-change
        (uix/use-callback
         (fn [event]
           (let [state (.. event -target -readyState)]
             (dispatch :session/change-status state))) [dispatch])]

    ;; Listen to changes to the state of the WebSocket connection.
    (hooks/use-event-listener socket "open"  on-status-change)
    (hooks/use-event-listener socket "close" on-status-change)
    (hooks/use-event-listener socket "error" on-status-change)

    ;; Periodically attempt to re-establish closed connections.
    (hooks/use-interval
     (uix/use-callback
      (fn []
        (let [user (ds/entity @conn [:db/ident :user])]
          (if (and (= (:user/type user) :conn)
                   (= (:session/status user) :disconnected))
            (dispatch :session/join)))) [conn dispatch]) interval-reconnect)

    ;; Periodically send heartbeat messages to keep the session connections
    ;; alive. This heartbeat will be sent to the server and then distributed
    ;; to the other connections in the room.
    (hooks/use-interval
     (uix/use-callback
      (fn []
        (let [user (ds/entity @conn [:db/ident :user])]
          (if (and (= (:user/type user) :host)
                   (= (:session/status user) :connected))
            (dispatch :session/heartbeat)))) [conn dispatch]), interval-heartbeat)

    ;; Subscribe to requests to create a new session, creating a WebSocket
    ;; connection object.
    (hooks/use-subscribe :session/request
      (uix/use-callback
       (fn []
         (let [host (ds/entity @conn [:db/ident :user])
               conn (js/WebSocket.
                     (if (:session/last-room host)
                       (str SOCKET-URL "?host=" (:session/last-room host))
                       SOCKET-URL))]
           (set-socket conn))) [conn]))

    ;; Subscribe to requests to join the session, creating a WebSocket
    ;; connection object.
    (hooks/use-subscribe :session/join
      (uix/use-callback
       (fn []
         (let [search (.. js/window -location -search)
               params (js/URLSearchParams. search)
               room   (.get params "join")]
           (if (some? room)
             (let [conn (js/WebSocket. (str SOCKET-URL "?join=" room))]
               (set-socket conn))))) []))

    ;; Subscribe to regular heartbeat events, rebroadcasting it to the other
    ;; connections in the server.
    (hooks/use-subscribe :session/heartbeat
      (uix/use-callback
       (fn []
         (on-send-text {:type :heartbeat})) [on-send-text]))

    ;; Subscribe to an intentional action to close the session, calling
    ;; `close` on the WebSocket object.
    (hooks/use-subscribe :session/close
      (uix/use-callback
       (fn []
         (when (some? socket)
           (.close socket)
           (set-socket nil))) [socket]))

    (hooks/use-subscribe :image/create
      (uix/use-callback
       (fn [blob]
         (let [{host :session/host} (ds/entity (ds/db conn) [:db/ident :session])]
           (.then (.arrayBuffer blob)
                  (fn [data]
                    (on-send-binary
                     #js {"data" (js/Uint8Array. data)
                          "dst"  (str (:user/uuid host))}))))) [conn on-send-binary]))

    ;; Subscribe to requests for image data from other non-host connections
    ;; and reply with the appropriate image data in the form of a data URL.
    (hooks/use-subscribe :image/request
      (uix/use-callback
       (fn [hash]
         (let [session (ds/entity @conn [:db/ident :session])]
           (if-let [host (-> session :session/host :user/uuid)]
             (let [data {:name :image/request :hash hash}]
               (on-send-text {:type :event :dst host :data data}))))) [conn on-send-text]))

    (hooks/use-subscribe :image/change-thumbnail-request
      (uix/use-callback
       (fn [hash bounds]
         (let [session (ds/entity @conn [:db/ident :session])]
           (if-let [host (-> session :session/host :user/uuid)]
             (on-send-text
              {:type :event
               :dst host
               :data
               {:name :image/change-thumbnail-request
                :hash hash
                :bounds bounds}})))) [conn on-send-text]))

    ;; Subscribe to DataScript transactions and broadcast the transaction data
    ;; to the other connections in the session.
    (hooks/use-subscribe :tx/commit
      (uix/use-callback
       (fn [{tx-data :tx-data}]
         (on-send-text {:type :tx :data tx-data})) [on-send-text]))

    ;; Subscribe to changes to the user's cursor position on the scene and
    ;; broadcast these changes to the other connections in the session.
    (hooks/use-subscribe :cursor/move
      (uix/use-memo
       #(throttle
         (fn [x y]
           (let [data {:name :cursor/moved :coord [x y]}]
             (on-send-text {:type :event :data data}))) 66) [on-send-text]))

    ;; Listen to the "message" event on the WebSocket object and forward the
    ;; event details to the appropriate handler.
    (hooks/use-event-listener socket "message"
      (uix/use-callback
       (fn [event]
         (if (string? (.-data event))
           (-> (transit/read reader (.-data event))
               (on-receive-text conn publish images on-send-text on-send-binary))
           (-> (.arrayBuffer (.-data event))
               (.then (fn [data] (MessagePack/decode data)))
               (.then (fn [data] (on-receive-binary data conn publish))))))
       [conn publish images on-send-text on-send-binary]))

    ;; Establish a WebSocket connection to the room identified by the "join"
    ;; query parameter in the URL.
    (uix/use-effect
     (fn []
       (let [user (ds/entity @conn [:db/ident :user])
             type (:user/type user)
             status (:session/status user)]
         (if (and (= type :conn) (or (nil? status) (= status :initial)))
           (dispatch :session/join)))) [conn dispatch])))
