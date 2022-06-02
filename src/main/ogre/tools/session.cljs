(ns ogre.tools.session
  (:require [datascript.core :as ds]
            [datascript.transit :as dst]
            [ogre.tools.env :as env]
            [ogre.tools.render :refer [listen!]]
            [ogre.tools.state :refer [state schema]]
            [ogre.tools.storage :refer [storage]]
            [uix.core.alpha :as uix]
            [cognitect.transit :as transit]))

(def reader (transit/reader :json {:handlers dst/read-handlers}))
(def writer (transit/writer :json {:handlers dst/write-handlers}))

(defn handle-open [_ _]
  (comment "Not implemented"))

(defn handle-close [{:keys [dispatch]} _]
  (dispatch :session/disconnected))

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
        [[:db/add -1 :db/ident :session]
         [:db/add -2 :entity/key local]
         [:db/add -3 :entity/key window]

         ;; Replace :db/ident :local
         [:db/retract [:entity/key host] :db/ident]
         [:db/add -2 :db/ident :local]

         ;; Replace host as the local user, swap places in session
         ;; connections.
         [:db/add [:db/ident :session] :session/conns [:entity/key host]]
         [:db/add [:db/ident :root] :root/local -2]
         [:db/retract [:db/ident :session] :session/conns -2]

         ;; Initialize the user entity.
         [:db/add -2 :local/loaded? true]
         [:db/add -2 :local/type :conn]
         [:db/add -2 :local/window -3]
         [:db/add -2 :local/windows -3]
         [:db/add -2 :session/state :connected]

         ;; Move local window to the currently viewing canvas.
         [:db/add -3 :window/canvas [:entity/key canvas]]]]
    (ds/db-with next tx-data)))

(defmulti handle-message (fn [_ {:keys [type]} _] type))

(defmethod handle-message :default
  [_ _ _])

;; Handles messages that are sent by the server, some of which are responses to
;; creating or connecting to a session or to notify connections within a session
;; of important events. The embedded :data map will always contain a member
;; called :name to distinguish different kinds of events.
(defmethod handle-message :event
  [{:keys [conn store]} {:keys [data] :as message} on-send]
  (case (:name data)
    :session/created
    (ds/transact!
     conn
     [[:db/add [:db/ident :local] :entity/key (:uuid data)]
      [:db/add [:db/ident :local] :session/state :connected]
      [:db/add [:db/ident :session] :session/room (:room data)]])

    :session/joined
    (ds/transact!
     conn
     [[:db/add [:db/ident :local] :entity/key (:uuid data)]
      [:db/add [:db/ident :local] :session/state :connected]])

    :session/join
    (let [tx-data [[:db/add -1 :entity/key (:uuid data)] [:db/add [:db/ident :session] :session/conns -1]]
          report  (ds/transact! conn tx-data)
          datoms  (ds/datoms (:db-after report) :eavt)]
      (on-send {:type :datoms :dst (:uuid data) :data (into [] datoms)}))

    :session/leave
    (ds/transact! conn [[:db/retractEntity [:entity/key (:uuid data)]]])

    :image/request
    (-> (.get (.table store "images") (:checksum data))
        (.then (fn [r] (.-data r)))
        (.then (fn [data-url]
                 (on-send {:type :image :dst (:src message) :data data-url}))))))

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
  (ds/transact! conn (:data message)))

;; Handles messages that include image data as data URLs. These messages are
;; generally received from the host after sending a request for it via its
;; checksum.
(defmethod handle-message :image
  [{:keys [dispatch]} message]
  (let [data-url (:data message)]
    (dispatch :image/cache data-url)))

(defn handlers []
  (let [{:keys [store]} (uix/context storage)
        [conn dispatch] (uix/context state)
        socket          (uix/state nil)
        on-send         (uix/callback
                         (fn [message]
                           (if-let [socket @socket]
                             (if (= (.-readyState socket) 1)
                               (let [{key :entity/key} (ds/entity @conn [:db/ident :local])
                                     defaults          {:time (js/Date.now) :src key}]
                                 (->> (merge defaults message)
                                      (transit/write writer)
                                      (.send socket)))))) [])]

    (uix/effect!
     (fn []
       (let [search (.. js/window -location -search)
             params (js/URLSearchParams. search)
             room   (.get params "join")]
         (if (not (nil? room))
           (let [ws (js/WebSocket. (str env/SOCKET-URL "?key=" room))]
             (reset! socket ws))))) [])

    (uix/effect!
     (fn []
       (ds/listen!
        conn :session
        (fn [{db-after :db-after [event args tx-data] :tx-meta}]
          (cond (= event :session/request)
                (let [ws (js/WebSocket. env/SOCKET-URL)]
                  (reset! socket ws))

                (= event :session/close)
                (if-let [ws @socket]
                  (.close ws)
                  (reset! socket nil))

                (= event :image/request)
                (let [session (ds/entity db-after [:db/ident :session])]
                  (if-let [host (-> session :session/host :entity/key)]
                    (on-send {:type :event :dst host :data {:name :image/request :checksum (first args)}})))

                (seq tx-data)
                (on-send {:type :tx :data tx-data}))))
       (fn [] (ds/unlisten! conn :session))) [])

    (doseq [type ["open" "close" "message" "error"]]
      (listen!
       (let [context {:conn conn :dispatch dispatch :store store}]
         (fn [event]
           (case type
             "open"    (handle-open context event)
             "close"   (handle-close context event)
             "error"   (js/console.log "error" event)
             "message" (let [data    (transit/read reader (.-data event))]
                         (handle-message context data on-send)))))
        @socket type [@socket])) nil))
