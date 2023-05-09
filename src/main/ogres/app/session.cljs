(ns ogres.app.session
  (:require [clojure.set :refer [difference]]
            [cognitect.transit :as transit]
            [clojure.core.async :refer [chan sliding-buffer]]
            [datascript.core :as ds]
            [datascript.transit :as dst]
            [ogres.app.env :as env]
            [ogres.app.hooks :refer [use-listen use-subscribe use-dispatch use-publish use-interval use-store]]
            [ogres.app.provider.image :refer [create-checksum create-image-element]]
            [ogres.app.provider.state :as provider.state]
            [uix.core :refer [defui use-context use-state use-callback use-effect]]))

(def reader (transit/reader :json {:handlers dst/read-handlers}))
(def writer (transit/writer :json {:handlers dst/write-handlers}))
(def ^{:private true} interval-heartbeat 20000)
(def ^{:private true} interval-reconnect 5000)

(def ^{:private true} session-color-options
  #{"#f44336" "#2196f3" "#8bc34a" "#673ab7" "#ff9800" "#009688" "#3f51b5" "#9c27b0" "#ff5722"})

(defn ^{:private true} next-color [data colors]
  (let [unused (->> (ds/entity data [:db/ident :session])
                    (:session/conns)
                    (into #{} (map :local/color))
                    (difference colors))]
    (if (seq unused)
      (first (shuffle unused))
      (first (shuffle colors)))))

(def merge-query
  [{:root/session
    [{:session/host
      [:db/key
       {:local/window
        [{:window/canvas
          [:db/key]}]}]}]}])

(defn merge-initial-state
  "Returns a new DataScript database with the current local state `prev` mixed
   into the incoming initial state `next`."
  [next prev]
  (let [prev-local  (ds/entity prev [:db/ident :local])
        next-local  (ds/entity next [:db/key (:db/key prev-local)])
        prev-window (:local/window prev-local)

        {{{host :db/key} :session/host} :root/session}
        (ds/pull next merge-query [:db/ident :root])

        tx-data
        (into [[:db/add -1 :db/ident :session]
               [:db/retract [:db/key host] :db/ident]

               ;; Selectively merge parts of the previous local entity into the
               ;; next local entity.
               [:db/add -2 :db/key (:db/key prev-local)]
               [:db/add -2 :db/ident :local]
               [:db/add -2 :bounds/self (or (:bounds/self prev-local) [0 0 0 0])]

               ;; Replace host as the local user, swap places in session
               ;; connections.
               [:db/add [:db/ident :session] :session/conns [:db/key host]]
               [:db/add [:db/ident :root] :root/local -2]
               [:db/retract [:db/ident :session] :session/conns -2]]

              ;; Maintain local window state when reconnecting and starting
              ;; with a window that references the same canvas.
              (if (= (:db/key (:window/canvas (:local/window prev-local)))
                     (:db/key (:window/canvas (:local/window next-local))))
                [[:db/add -3 :db/key (:db/key (:local/window next-local))]
                 [:db/add -3 :window/vec (or (:window/vec prev-window) [0 0])]
                 [:db/add -3 :window/scale (or (:window/scale prev-window) 1)]] []))]
    (ds/db-with next tx-data)))

(defmulti handle-message (fn [_ {:keys [type]} _] type))

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
     [[:db/add [:db/ident :local] :db/key (:uuid data)]
      [:db/add [:db/ident :local] :session/state :connected]
      [:db/add [:db/ident :local] :session/last-room (:room data)]
      [:db/add [:db/ident :session] :session/room (:room data)]])

    :session/joined
    (ds/transact!
     conn
     [[:db/add [:db/ident :local] :db/key (:uuid data)]
      [:db/add [:db/ident :local] :session/state :connected]])

    :session/join
    (let [local   (ds/entity @conn [:db/ident :local])
          tx-data
          [[:db/add -1 :db/key (:uuid data)]
           [:db/add -1 :local/type :conn]
           [:db/add -1 :panel/expanded #{:session}]
           [:db/add [:db/ident :session] :session/conns -1]]

          tx-data-addtl
          (if (= (:local/type local) :host)
            [[:db/add -1 :db/key (:uuid data)]
             [:db/add -1 :local/type :conn]
             [:db/add -1 :local/loaded? true]
             [:db/add -1 :local/color (next-color @conn session-color-options)]
             [:db/add -1 :session/state :connected]
             [:db/add -1 :local/window -2]
             [:db/add -1 :local/windows -2]
             [:db/add -2 :db/key (ds/squuid)]
             [:db/add -2 :window/canvas -3]
             [:db/add -3 :db/key (-> local :local/window :window/canvas :db/key)]]
            [])
          report (ds/transact! conn (into tx-data tx-data-addtl))]
      (if (= (:local/type local) :host)
        (let [datoms (ds/datoms (:db-after report) :eavt)]
          (on-send {:type :datoms :data (into [] datoms) :dst (:uuid data)})
          (on-send {:type :tx :data tx-data-addtl}))))

    :session/leave
    (ds/transact! conn [[:db/retractEntity [:db/key (:uuid data)]]])

    :image/request
    (-> (.get (.table store "images") (:checksum data))
        (.then (fn [r] (.-data r)))
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
        record   #js {:checksum checksum :data data-url :created-at (js/Date.now)}
        entity   (ds/entity (ds/db conn) [:db/ident :local])]
    (if (= (:local/type entity) :host)
      ;; The host has received an image from another connection. This is
      ;; inferred as another connection in the session uploading a token
      ;; image. Put the image into local storage, update state, and publish
      ;; the relevant event to trigger an update to the image cache.
      (-> (.put (.table store "images") record)
          (.then #(create-image-element data-url))
          (.then (fn [image]
                   (let [w (.-width image) h (.-height image)]
                     (dispatch :stamp/create checksum w h :public)
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
                              defaults {:time (js/Date.now) :src (:db/key local)}]
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
    (use-subscribe
     (use-callback
      (fn []
        (let [host (ds/entity @conn [:db/ident :local])
              conn (js/WebSocket.
                    (if (:session/last-room host)
                      (str env/SOCKET-URL "?host=" (:session/last-room host))
                      env/SOCKET-URL))]
          (set-socket conn))) [conn]) :session/request)

    ;; Subscribe to requests to join the session, creating a WebSocket
    ;; connection object.
    (use-subscribe
     (use-callback
      (fn []
        (let [search (.. js/window -location -search)
              params (js/URLSearchParams. search)
              room   (.get params "join")]
          (if (not (nil? room))
            (let [conn (js/WebSocket. (str env/SOCKET-URL "?join=" room))]
              (set-socket conn))))) []) :session/join)

    ;; Subscribe to regular heartbeat events, rebroadcasting it to the other
    ;; connections in the server.
    (use-subscribe
     (use-callback
      (fn []
        (on-send {:type :heartbeat})) [on-send]) :session/heartbeat)

    ;; Subscribe to an intentional action to close the session, calling
    ;; `close` on the WebSocket object.
    (use-subscribe
     (use-callback
      (fn []
        (when (not (nil? socket))
          (.close socket)
          (set-socket nil))) [socket]) :session/close)

    ;; Subscribe to image uploads, either handling them normally if the user is
    ;; the host or sending the image data to the host if the user is a client.
    ;; Sending image data to the host is interpretted as a request to share an
    ;; image from their local computer to the rest of the connections in the
    ;; session as a token.
    (use-subscribe
     (use-callback
      (fn [{[type {:keys [data-url checksum width height]}] :args}]
        (let [{{kind :local/type} :root/local
               {host :session/host} :root/session}
              (ds/entity (ds/db conn) [:db/ident :root])]
          (case [kind type]
            [:host :token] (dispatch :stamp/create checksum width height :private)
            [:host :scene] (dispatch :scene/create checksum width height :private)
            ([:conn :token] [:conn :scene])
            (on-send {:type :image :dst (:db/key host) :data data-url})))) [conn dispatch on-send]) :image/create)

    ;; Subscribe to requests for image data from other non-host connections
    ;; and reply with the appropriate image data in the form of a data URL.
    (use-subscribe
     (use-callback
      (fn [{[checksum] :args}]
        (let [session (ds/entity @conn [:db/ident :session])]
          (if-let [host (-> session :session/host :db/key)]
            (let [data {:name :image/request :checksum checksum}]
              (on-send {:type :event :dst host :data data}))))) [conn on-send]) :image/request)

    ;; Subscribe to DataScript transactions and broadcast the transaction data
    ;; to the other connections in the session.
    (use-subscribe
     (use-callback
      (fn [{[{tx-data :tx-data}] :args}]
        (on-send {:type :tx :data tx-data})) [on-send]) :tx/commit)

    ;; Subscribe to changes to the user's cursor position on the canvas and
    ;; broadcast these changes to the other connections in the session.
    (use-subscribe
     (use-callback
      (fn [{[x y] :args}]
        (let [data {:name :cursor/moved :coord [x y]}]
          (on-send {:type :event :data data}))) [on-send]) :cursor/move
     {:chan cursor :rate-limit 80})

    ;; Listen to the "close" event on the WebSocket object and dispatch the
    ;; appropriate event to communicate this change to state.
    (use-listen
     (use-callback
      (fn []
        (dispatch :session/disconnected)) [dispatch]) socket "close")

    ;; Listen to the "message" event on the WebSocket object and forward the
    ;; event details to the appropriate handler.
    (let [context {:conn conn :publish publish :dispatch dispatch :store store}]
      (use-listen
       (use-callback
        (fn [event]
          (let [data (transit/read reader (.-data event))]
            (handle-message context data on-send))) [context on-send]) socket "message"))))
