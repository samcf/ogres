(ns ogre.tools.session
  (:require [datascript.core :as ds]
            [datascript.transit :as dst]
            [ogre.tools.render :refer [listen!]]
            [ogre.tools.state :refer [state schema]]
            [uix.core.alpha :as uix]
            [cognitect.transit :as transit]))

(def reader (transit/reader :json {:handlers dst/read-handlers}))
(def writer (transit/writer :json {:handlers dst/write-handlers}))

(defn handle-open [data]
  (js/console.log "opened" data))

(defn handle-close [data]
  (js/console.log "closed" data))

(def merge-query
  [{:root/session
    [{:session/host
      [:entity/key
       {:local/window
        [{:window/canvas
          [:entity/key]}]}]}]}])

(defn merge-initial-state
  "Returns a new DataScript database with the current local state `prev` mixed
   into the incoming initial state `next`."
  [next prev]
  (let [{local :entity/key {window :entity/key} :local/window}
        (ds/entity prev [:db/ident :local])

        {{{host :entity/key
           {{canvas :entity/key} :window/canvas}
           :local/window}
          :session/host}
         :root/session}
        (ds/pull next merge-query [:db/ident :root])

        tx-data
        [[:db/retract [:entity/key host] :db/ident]
         [:db/add [:db/ident :root] :root/local -1]
         [:db/add -1 :db/ident :local]
         [:db/add -1 :entity/key local]
         [:db/add -1 :local/loaded? true]
         [:db/add -1 :local/type :conn]
         [:db/add -1 :local/window -2]
         [:db/add -2 :entity/key window]
         [:db/add -2 :window/canvas [:entity/key canvas]]]]
    (ds/db-with next tx-data)))

(defmulti handle-message (fn [_ {:keys [type]}] type))

(defmethod handle-message :default
  [_ _])

;; Handles messages that are sent by the server, some of which are responses to
;; creating or connecting to a session or to notify connections within a session
;; of important events. The embedded :data map will always contain a member
;; called :name to distinguish different kinds of events.
(defmethod handle-message :event
  [{:keys [conn]} {:keys [data]}]
  (case (:name data)
    :session/created
    (do (ds/transact!
         conn
         [[:db/add [:db/ident :local] :entity/key (:uuid data)]
          [:db/add [:db/ident :session] :session/state :connected]
          [:db/add [:db/ident :session] :session/room (:room data)]])
        [])

    :session/joined
    (do (ds/transact! conn [[:db/add [:db/ident :local] :entity/key (:uuid data)]])
        [])

    :session/join
    (let [tx-data [[:db/add -1 :entity/key (:uuid data)] [:db/add [:db/ident :session] :session/conns -1]]
          report  (ds/transact! conn tx-data)
          datoms  (ds/datoms (:db-after report) :eavt)]
      [{:type :datoms :time "" :src "" :dst (:uuid data) :data (into [] datoms)}])

    :session/leave
    (do (ds/transact! conn [[:db/retractEntity [:entity/key (:uuid data)]]])
        [])
 
    nil))

;; Handles messages that include a complete copy of the host's initial state as
;; a set of DataScript datoms. This message is generally received right after
;; connecting to the session, but it may also be received periodically in an
;; attempt to correct any divergences in state.
(defmethod handle-message :datoms
  [{:keys [conn]} body]
  (-> (:data body)
      (ds/init-db schema)
      (merge-initial-state (ds/db conn))
      (as-> db (ds/reset-conn! conn db))) [])

;; Handles messages that include a DataScript transaction from another
;; connection within the session. These messages can be received at any time
;; and from any connection besides its own.
(defmethod handle-message :tx
  [{:keys [conn]} message]
  (try (ds/transact! conn (:data message))
       (catch js/Error error
         (println error))))

;; Handles messages that include image data as data URLs. These messages are
;; generally received from the host after sending a request for it via its
;; checksum.
(defmethod handle-message :image
  [_ _])

(defn handlers []
  (let [[conn dispatch] (uix/context state)
        socket (uix/state nil)]

    (uix/effect!
     (fn []
       (let [search (.. js/window -location -search)
             params (js/URLSearchParams. search)
             room   (.get params "join")]
         (if (not (nil? room))
           (let [ws (js/WebSocket. (str "ws://localhost:5000/ws?key=" room))]
             (reset! socket ws))))) [])

    (uix/effect!
     (fn []
       (ds/listen!
        conn :session
        (fn [{[event _ tx-data] :tx-meta}]
          ;; Initiate a WebSocket connection.
          (if (= event :session/request)
            (let [ws (js/WebSocket. "ws://localhost:5000/ws")]
              (reset! socket ws)))

          ;; Broadcast all transactions to the WebSocket session.
          (if (and (not (nil? @socket)) (seq tx-data))
            (let [marshalled (transit/write writer {:type :tx :data tx-data})]
              (.send @socket marshalled)))))
       (fn [] (ds/unlisten! conn :session))) [@socket])

    (let [socket  @socket
          context {:conn conn :dispatch dispatch}]
      (doseq [event ["open" "close" "message" "error"]]
        (listen!
         (fn [body]
           (case event
             "open"    (handle-open body)
             "close"   (handle-close body)
             "message" (let [parsed   (transit/read reader (.-data body))
                             messages (handle-message context parsed)]
                         (doseq [message messages]
                           (let [serialized (transit/write writer message)]
                             (.send socket serialized))))
             "error"   (js/console.log "error" body)))
         socket event [socket]))) nil))
