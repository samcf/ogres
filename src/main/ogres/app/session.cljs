(ns ogres.app.session
  (:require [clojure.core.async :refer [chan sliding-buffer]]
            [cognitect.transit :as transit]
            [datascript.core :as ds]
            [datascript.transit :as dst]
            [ogres.app.const :refer [SOCKET-URL]]
            [ogres.app.hooks :refer [use-event-listener use-subscribe use-dispatch use-publish use-interval use-store]]
            [ogres.app.provider.state :as provider.state]
            [uix.core :refer [defui use-context use-state use-callback use-effect]]))

(def ^:private reader (transit/reader :json {:handlers dst/read-handlers}))
(def ^:private writer (transit/writer :json {:handlers dst/write-handlers}))
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

(defmulti ^:private handle-message
  (fn [_ {:keys [type]} _] type))

(defmethod handle-message :default
  [_ _ _])

;; Handles messages that are sent by the server, some of which are responses to
;; creating or connecting to a session or to notify connections within a session
;; of important events. The embedded :data map will always contain a member
;; called :name to distinguish different kinds of events.
(defmethod handle-message :event
  [{:keys [conn store publish]} {:keys [data] :as message} on-send]
  (case (:name data)
    :session/created
    (ds/transact!
     conn
     [[:db/add [:db/ident :user] :user/uuid (:uuid data)]
      [:db/add [:db/ident :user] :session/state :connected]
      [:db/add [:db/ident :user] :session/last-room (:room data)]
      [:db/add [:db/ident :session] :session/room (:room data)]])

    :session/joined
    (ds/transact!
     conn
     [[:db/add [:db/ident :user] :user/uuid (:uuid data)]
      [:db/add [:db/ident :user] :session/state :connected]])

    :session/join
    (let [user (ds/entity @conn [:db/ident :user])]
      (if (= (:user/type user) :host)
        (let [tx-data
              [{:user/uuid     (:uuid data)
                :user/type     :conn
                :user/status   :ready
                :user/color    (next-color @conn color-options)
                :session/state :connected
                :user/cameras  -1
                :user/camera
                (let [{{point :camera/point
                        scale :camera/scale
                        {scene :db/id} :camera/scene}
                       :user/camera} user]
                  {:db/id -1
                   :camera/scene scene
                   :camera/point (or point [0 0])
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
    (-> (.get (.table store "images") (:hash data))
        (.then (fn [record] (.-data record)))
        (.then (fn [data] (.arrayBuffer data)))
        (.then (fn [data]
                 (on-send
                  {:type :image
                   :dst  (:src message)
                   :data (js/Uint8Array. data)}))))

    :cursor/moved
    (let [{src :src {[x y] :coord} :data} message]
      (publish {:topic :cursor/moved :args [src x y]}))))

;; Handles messages that include a complete copy of the host's initial state as
;; a set of DataScript datoms. This message is generally received right after
;; connecting to the session, but it may also be received periodically in an
;; attempt to correct any divergences in state.
(defmethod handle-message :datoms
  [{:keys [conn]} body]
  (let [user-data (ds/db conn)
        user-vers (:root/release (ds/entity user-data [:db/ident :root]))
        host-data (ds/init-db (:data body) provider.state/schema)
        host-vers (:root/release (ds/entity host-data [:db/ident :root]))]
    (if (not= user-vers host-vers)
      (let [params (js/URLSearchParams. (.. js/window -location -search))
            origin (.. js/window -location -origin)
            path   (.. js/window -location -pathname)]
        (.set params "r" host-vers)
        (.replace
         (.-location js/window)
         (str origin path "?" (.toString params))))
      (ds/reset-conn! conn (initialize-player-state host-data (:dst body))))))

;; Handles messages that include a DataScript transaction from another
;; connection within the session. These messages can be received at any time
;; and from any connection besides its own.
(defmethod handle-message :tx
  [{:keys [conn]} message]
  (ds/transact! conn (:data message)))

;; Handles messages that include image data as data URLs. These messages are
;; generally received from the host after sending a request for it via its
;; hash.
(defmethod handle-message :image
  [{:keys [conn publish]} message]
  (let [data (js/Blob. #js [(:data message)] #js {"type" "image/jpeg"})
        user (ds/entity (ds/db conn) [:db/ident :user])
        call (fn [hash] (publish {:topic :image/create-token :args [hash data]}))]
    (case (:user/type user)
      :host (publish {:topic :image/cache :args [data call]})
      :conn (publish {:topic :image/cache :args [data]}))))

(defui handlers []
  (let [[socket set-socket] (use-state nil)
        [cursor] (use-state (chan (sliding-buffer 1)))
        dispatch (use-dispatch)
        publish  (use-publish)
        store    (use-store)
        conn     (use-context provider.state/context)
        on-send  (use-callback
                  (fn [message]
                    (if (some? socket)
                      (if (= (.-readyState socket) 1)
                        (let [user (ds/entity @conn [:db/ident :user])
                              defaults {:time (js/Date.now) :src (:user/uuid user)}]
                          (->> (merge defaults message)
                               (transit/write writer)
                               (.send socket)))))) ^:lint/disable [socket])]

    ;; Establish a WebSocket connection to the room identified by the "join"
    ;; query parameter in the URL.
    (use-effect
     (fn []
       (let [user (ds/entity @conn [:db/ident :user])
             state (:session/state user)
             type (:user/type user)]
         (if (and (= type :conn) (or (nil? state) (= state :initial)))
           (dispatch :session/join)))) [conn dispatch])

    ;; Periodically attempt to re-establish closed connections.
    (use-interval
     (use-callback
      (fn []
        (let [user (ds/entity @conn [:db/ident :user])]
          (if (and (= (:user/type user) :conn)
                   (= (:session/state user) :disconnected))
            (dispatch :session/join)))) [conn dispatch]) interval-reconnect)

    ;; Periodically send heartbeat messages to keep the session connections
    ;; alive. This heartbeat will be sent to the server and then distributed
    ;; to the other connections in the room.
    (use-interval
     (use-callback
      (fn []
        (let [user (ds/entity @conn [:db/ident :user])]
          (if (and (= (:user/type user) :host)
                   (= (:session/state user) :connected))
            (dispatch :session/heartbeat)))) [conn dispatch]), interval-heartbeat)

    ;; Subscribe to requests to create a new session, creating a WebSocket
    ;; connection object.
    (use-subscribe :session/request
      (use-callback
       (fn []
         (let [host (ds/entity @conn [:db/ident :user])
               conn (js/WebSocket.
                     (if (:session/last-room host)
                       (str SOCKET-URL "?host=" (:session/last-room host))
                       SOCKET-URL))]
           (set-socket conn))) [conn]))

    ;; Subscribe to requests to join the session, creating a WebSocket
    ;; connection object.
    (use-subscribe :session/join
      (use-callback
       (fn []
         (let [search (.. js/window -location -search)
               params (js/URLSearchParams. search)
               room   (.get params "join")]
           (if (some? room)
             (let [conn (js/WebSocket. (str SOCKET-URL "?join=" room))]
               (set-socket conn))))) []))

    ;; Subscribe to regular heartbeat events, rebroadcasting it to the other
    ;; connections in the server.
    (use-subscribe :session/heartbeat
      (use-callback
       (fn []
         (on-send {:type :heartbeat})) [on-send]))

    ;; Subscribe to an intentional action to close the session, calling
    ;; `close` on the WebSocket object.
    (use-subscribe :session/close
      (use-callback
       (fn []
         (when (some? socket)
           (.close socket)
           (set-socket nil))) [socket]))

    (use-subscribe :image/create
      (use-callback
       (fn [{[blob] :args}]
         (let [{host :session/host} (ds/entity (ds/db conn) [:db/ident :session])]
           (.then
            (.arrayBuffer blob)
            (fn [data]
              (on-send
               {:type :image
                :dst  (:user/uuid host)
                :data (js/Uint8Array. data)}))))) [conn on-send]))

    ;; Subscribe to requests for image data from other non-host connections
    ;; and reply with the appropriate image data in the form of a data URL.
    (use-subscribe :image/request
      (use-callback
       (fn [{[hash] :args}]
         (let [session (ds/entity @conn [:db/ident :session])]
           (if-let [host (-> session :session/host :user/uuid)]
             (let [data {:name :image/request :hash hash}]
               (on-send {:type :event :dst host :data data}))))) [conn on-send]))

    ;; Subscribe to DataScript transactions and broadcast the transaction data
    ;; to the other connections in the session.
    (use-subscribe :tx/commit
      (use-callback
       (fn [{[{tx-data :tx-data}] :args}]
         (on-send {:type :tx :data tx-data})) [on-send]))

    ;; Subscribe to changes to the user's cursor position on the scene and
    ;; broadcast these changes to the other connections in the session.
    (use-subscribe :cursor/move
      {:chan cursor :rate-limit 60}
      (use-callback
       (fn [{[x y] :args}]
         (let [data {:name :cursor/moved :coord [x y]}]
           (on-send {:type :event :data data}))) [on-send]))

    ;; Listen to the "close" event on the WebSocket object and dispatch the
    ;; appropriate event to communicate this change to state.
    (use-event-listener socket "close"
      (use-callback
       (fn []
         (dispatch :session/disconnected)) [dispatch]))

    ;; Listen to the "error" event on the WebSocket object.
    (use-event-listener socket "error"
      (use-callback
       (fn []
         (let [user (ds/entity (ds/db conn) [:db/ident :user])]
           (if (= (:user/type user) :conn)
             (dispatch :user/change-status :disconnected)))) [conn dispatch]))

    ;; Listen to the "message" event on the WebSocket object and forward the
    ;; event details to the appropriate handler.
    (use-event-listener socket "message"
      (use-callback
       (fn [event]
         (let [context {:conn conn :publish publish :dispatch dispatch :store store}
               data (transit/read reader (.-data event))]
           (handle-message context data on-send))) [conn publish dispatch store on-send]))))
