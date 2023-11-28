(ns ogres.app.session
  (:require [clojure.set :refer [difference]]
            [cognitect.transit :as transit]
            [clojure.core.async :refer [chan sliding-buffer]]
            [datascript.core :as ds]
            [datascript.transit :as dst]
            [goog.object :as go]
            [ogres.app.const :refer [SOCKET-URL]]
            [ogres.app.hooks :refer [use-event-listener use-subscribe use-dispatch use-publish use-interval use-store]]
            [ogres.app.provider.image :refer [create-checksum create-image-element]]
            [ogres.app.provider.state :as provider.state]
            [uix.core :refer [defui use-context use-state use-callback use-effect]]))

(def ^:private reader (transit/reader :json {:handlers dst/read-handlers}))
(def ^:private writer (transit/writer :json {:handlers dst/write-handlers}))
(def ^:private interval-heartbeat 20000)
(def ^:private interval-reconnect 5000)

(def ^:private session-color-options
  #{"#f44336" "#2196f3" "#8bc34a" "#673ab7" "#ff9800" "#009688" "#3f51b5" "#9c27b0" "#ff5722"})

(defn ^:private next-color [data colors]
  (let [unused (->> (ds/entity data [:db/ident :session])
                    (:session/conns)
                    (into #{} (map :local/color))
                    (difference colors))]
    (if (seq unused)
      (first (shuffle unused))
      (first (shuffle colors)))))

(def ^:private merge-query
  [{:root/session
    [{:session/host
      [:db/id
       {:local/camera
        [:camera/scene]}]}]}])

(defn ^:private merge-initial-state
  "Returns a new DataScript database with the current local state `prev` mixed
   into the incoming initial state `next`."
  [next prev]
  (let [prev-local  (ds/entity prev [:db/ident :local])
        next-local  (ds/entity next [:local/uuid (:local/uuid prev-local)])
        prev-camera (:local/camera prev-local)

        {{{host :db/id} :session/host} :root/session}
        (ds/pull next merge-query [:db/ident :root])

        tx-data
        (into [[:db/retract host :db/ident]
               {:db/ident :session}

               ;; Selectively merge parts of the previous local entity into the
               ;; next local entity.
               {:db/id       (:db/id next-local)
                :db/ident    :local
                :bounds/self (or (:bounds/self prev-local) [0 0 0 0])}

               ;; Replace host as the local user, swap places in session
               ;; connections.
               [:db/retract [:db/ident :session] :session/conns (:db/id next-local)]
               {:db/ident     :root
                :root/local   (:db/id next-local)
                :root/session {:db/ident :session :session/conns host}}]

              ;; Maintain local camera state when reconnecting and starting
              ;; with a camera that references the same scene.
              (if (= (:db/id (:camera/scene (:local/camera prev-local)))
                     (:db/id (:camera/scene (:local/camera next-local))))
                [{:db/id (:db/id (:local/camera next-local))
                  :camera/point (or (:camera/point prev-camera) [0 0])
                  :camera/scale (or (:camera/scale prev-camera) 1)}]
                []))]
    (ds/db-with next tx-data)))

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
     [[:db/add [:db/ident :local] :local/uuid (:uuid data)]
      [:db/add [:db/ident :local] :session/state :connected]
      [:db/add [:db/ident :local] :session/last-room (:room data)]
      [:db/add [:db/ident :session] :session/room (:room data)]])

    :session/joined
    (ds/transact!
     conn
     [[:db/add [:db/ident :local] :local/uuid (:uuid data)]
      [:db/add [:db/ident :local] :session/state :connected]])

    :session/join
    (let [local   (ds/entity @conn [:db/ident :local])
          tx-data
          [[:db/add -1 :local/uuid (:uuid data)]
           [:db/add -1 :local/type :conn]
           [:db/add -1 :panel/expanded #{:session}]
           [:db/add [:db/ident :session] :session/conns -1]]

          tx-data-addtl
          (if (= (:local/type local) :host)
            [[:db/add -1 :local/uuid (:uuid data)]
             [:db/add -1 :local/type :conn]
             [:db/add -1 :local/status :ready]
             [:db/add -1 :local/color (next-color @conn session-color-options)]
             [:db/add -1 :session/state :connected]
             [:db/add -1 :local/camera -2]
             [:db/add -1 :local/cameras -2]
             [:db/add -2 :camera/scene (-> local :local/camera :camera/scene :db/id)]
             [:db/add -2 :camera/point (or (-> local :local/camera :camera/point) [0 0])]
             [:db/add -2 :camera/scale (or (-> local :local/camera :camera/scale) 1)]]
            [])
          report (ds/transact! conn (into tx-data tx-data-addtl))]
      (if (= (:local/type local) :host)
        (let [datoms (ds/datoms (:db-after report) :eavt)]
          (on-send {:type :datoms :data (into [] datoms) :dst (:uuid data)})
          (on-send {:type :tx :data tx-data-addtl}))))

    :session/leave
    (ds/transact! conn [[:db/retractEntity [:local/uuid (:uuid data)]]])

    :image/request
    (-> (.get (.table store "images") (:checksum data))
        (.then (fn [record] (go/get record "data-url")))
        (.then (fn [data-url]
                 (on-send {:type :image :dst (:src message) :data data-url}))))

    :cursor/moved
    (let [{src :src {[x y] :coord} :data} message]
      (publish {:topic :cursor/moved :args [src x y]}))))

;; Handles messages that include a complete copy of the host's initial state as
;; a set of DataScript datoms. This message is generally received right after
;; connecting to the session, but it may also be received periodically in an
;; attempt to correct any divergences in state.
(defmethod handle-message :datoms
  [{:keys [conn]} body]
  (-> (:data body)
      (ds/init-db provider.state/schema)
      (merge-initial-state (ds/db conn))
      (as-> db (ds/reset-conn! conn db))) [])

;; Handles messages that include a DataScript transaction from another
;; connection within the session. These messages can be received at any time
;; and from any connection besides its own.
(defmethod handle-message :tx
  [{:keys [conn]} message]
  (ds/transact! conn (:data message)))

;; Handles messages that include image data as data URLs. These messages are
;; generally received from the host after sending a request for it via its
;; checksum.
(defmethod handle-message :image
  [{:keys [conn dispatch publish store]} message]
  (let [data-url (:data message)
        checksum (create-checksum data-url)
        record   #js {:checksum checksum :data-url data-url :created-at (js/Date.now)}
        entity   (ds/entity (ds/db conn) [:db/ident :local])]
    (if (= (:local/type entity) :host)
      ;; The host has received an image from another connection. This is
      ;; inferred as another connection in the session uploading a token
      ;; image. Put the image into local storage, update state, and publish
      ;; the relevant event to trigger an update to the image cache.
      (-> (.put (.table store "images") record)
          (.then #(create-image-element data-url))
          (.then (fn [image]
                   (let [data {:checksum checksum :width (.-width image) :height (.-height image)}]
                     (dispatch :tokens/create data :public)
                     (publish {:topic :image/cache :args [checksum data-url]})))))
      ;; This connection has received image data from the host - put it in
      ;; local storage and publish the relevant event to trigger an update
      ;; to the image cache.
      (-> (.put (.table store "images") record)
          (.then #(publish {:topic :image/cache :args [checksum data-url]}))))))

(defui handlers []
  (let [[socket set-socket] (use-state nil)
        [cursor] (use-state (chan (sliding-buffer 1)))
        dispatch (use-dispatch)
        publish  (use-publish)
        store    (use-store)
        conn     (use-context provider.state/context)
        on-send  (use-callback
                  (fn [message]
                    (if (not (nil? socket))
                      (if (= (.-readyState socket) 1)
                        (let [local    (ds/entity @conn [:db/ident :local])
                              defaults {:time (js/Date.now) :src (:local/uuid local)}]
                          (->> (merge defaults message)
                               (transit/write writer)
                               (.send socket)))))) ^:lint/disable [socket])]

    ;; Establish a WebSocket connection to the room identified by the "join"
    ;; query parameter in the URL.
    (use-effect
     (fn []
       (let [local (ds/entity @conn [:db/ident :local])
             state (:session/state local)
             type  (:local/type local)]
         (if (and (= type :conn) (or (nil? state) (= state :initial)))
           (dispatch :session/join)))) [conn dispatch])

    ;; Periodically attempt to re-establish closed connections.
    (use-interval
     (use-callback
      (fn []
        (let [local (ds/entity @conn [:db/ident :local])]
          (if (and (= (:local/type local) :conn)
                   (= (:session/state local) :disconnected))
            (dispatch :session/join)))) [conn dispatch]) interval-reconnect)

    ;; Periodically send heartbeat messages to keep the session connections
    ;; alive. This heartbeat will be sent to the server and then distributed
    ;; to the other connections in the room.
    (use-interval
     (use-callback
      (fn []
        (let [local (ds/entity @conn [:db/ident :local])]
          (if (and (= (:local/type local) :host)
                   (= (:session/state local) :connected))
            (dispatch :session/heartbeat)))) [conn dispatch]), interval-heartbeat)

    ;; Subscribe to requests to create a new session, creating a WebSocket
    ;; connection object.
    (use-subscribe :session/request
      (use-callback
       (fn []
         (let [host (ds/entity @conn [:db/ident :local])
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
           (if (not (nil? room))
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
         (when (not (nil? socket))
           (.close socket)
           (set-socket nil))) [socket]))

    ;; Subscribe to image uploads, either handling them normally if the user is
    ;; the host or sending the image data to the host if the user is a client.
    ;; Sending image data to the host is interpretted as a request to share an
    ;; image from their local computer to the rest of the connections in the
    ;; session as a token.
    (use-subscribe :image/create
      (use-callback
       (fn [{[type data] :args}]
         (let [{{kind :local/type} :root/local
                {host :session/host} :root/session}
               (ds/entity (ds/db conn) [:db/ident :root])]
           (case [kind type]
             [:host :token] (dispatch :tokens/create data :private)
             [:host :scene] (dispatch :scene-images/create data)
             ([:conn :token] [:conn :scene])
             (on-send {:type :image :dst (:local/uuid host) :data (:data-url data)})))) [conn dispatch on-send]))

    ;; Subscribe to requests for image data from other non-host connections
    ;; and reply with the appropriate image data in the form of a data URL.
    (use-subscribe :image/request
      (use-callback
       (fn [{[checksum] :args}]
         (let [session (ds/entity @conn [:db/ident :session])]
           (if-let [host (-> session :session/host :local/uuid)]
             (let [data {:name :image/request :checksum checksum}]
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
      {:chan cursor :rate-limit 80}
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
         (let [local (ds/entity (ds/db conn) [:db/ident :local])]
           (if (= (:local/type local) :conn)
             (dispatch :local/change-status :disconnected)))) [conn dispatch]))

    ;; Listen to the "message" event on the WebSocket object and forward the
    ;; event details to the appropriate handler.
    (use-event-listener socket "message"
      (use-callback
       (fn [event]
         (let [context {:conn conn :publish publish :dispatch dispatch :store store}
               data (transit/read reader (.-data event))]
           (handle-message context data on-send))) [conn publish dispatch store on-send]))))
