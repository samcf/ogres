(ns ogre.tools.session
  (:require [clojure.set :refer [difference]]
            [cognitect.transit :as transit]
            [datascript.core :as ds]
            [datascript.transit :as dst]
            [ogre.tools.env :as env]
            [ogre.tools.hooks :refer [subscribe-many! use-dispatch]]
            [ogre.tools.provider.state :as provider.state]
            [ogre.tools.render :refer [listen! use-interval]]
            [ogre.tools.storage :refer [use-store]]
            [uix.core.alpha :as uix]))

(def reader (transit/reader :json {:handlers dst/read-handlers}))
(def writer (transit/writer :json {:handlers dst/write-handlers}))

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
  (let [prev-local  (ds/entity prev [:db/ident :local])
        next-local  (ds/entity next [:entity/key (:entity/key prev-local)])
        prev-window (:local/window prev-local)

        {{{host :entity/key} :session/host} :root/session}
        (ds/pull next merge-query [:db/ident :root])

        tx-data
        (into [[:db/add -1 :db/ident :session]
               [:db/retract [:entity/key host] :db/ident]

               ;; Selectively merge parts of the previous local entity into the
               ;; next local entity.
               [:db/add -2 :entity/key (:entity/key prev-local)]
               [:db/add -2 :db/ident :local]
               [:db/add -2 :bounds/self (or (:bounds/self prev-local) [0 0 0 0])]
               [:db/add -2 :panel/current (or (:panel/current prev-local) :session)]
               [:db/add -2 :panel/collapsed? (or (:panel/collapsed? prev-local) false)]

               ;; Replace host as the local user, swap places in session
               ;; connections.
               [:db/add [:db/ident :session] :session/conns [:entity/key host]]
               [:db/add [:db/ident :root] :root/local -2]
               [:db/retract [:db/ident :session] :session/conns -2]]

              ;; Maintain local window state when reconnecting and starting
              ;; with a window that references the same canvas.
              (if (= (:entity/key (:window/canvas (:local/window prev-local)))
                     (:entity/key (:window/canvas (:local/window next-local))))
                [[:db/add -3 :entity/key (:entity/key (:local/window next-local))]
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
  [{:keys [conn store]} {:keys [data] :as message} on-send]
  (case (:name data)
    :session/created
    (ds/transact!
     conn
     [[:db/add [:db/ident :local] :entity/key (:uuid data)]
      [:db/add [:db/ident :local] :session/state :connected]
      [:db/add [:db/ident :local] :session/last-room (:room data)]
      [:db/add [:db/ident :session] :session/room (:room data)]])

    :session/joined
    (ds/transact!
     conn
     [[:db/add [:db/ident :local] :entity/key (:uuid data)]
      [:db/add [:db/ident :local] :session/state :connected]])

    :session/join
    (let [local   (ds/entity @conn [:db/ident :local])
          tx-data
          [[:db/add -1 :entity/key (:uuid data)]
           [:db/add -1 :local/type :conn]
           [:db/add [:db/ident :session] :session/conns -1]]

          tx-data-addtl
          (if (= (:local/type local) :host)
            [[:db/add -1 :entity/key (:uuid data)]
             [:db/add -1 :local/type :conn]
             [:db/add -1 :local/loaded? true]
             [:db/add -1 :local/color (next-color @conn session-color-options)]
             [:db/add -1 :session/state :connected]
             [:db/add -1 :local/window -2]
             [:db/add -1 :local/windows -2]
             [:db/add -2 :entity/key (ds/squuid)]
             [:db/add -2 :window/canvas -3]
             [:db/add -3 :entity/key (-> local :local/window :window/canvas :entity/key)]]
            [])
          report (ds/transact! conn (into tx-data tx-data-addtl))]
      (if (= (:local/type local) :host)
        (let [datoms (ds/datoms (:db-after report) :eavt)]
          (on-send {:type :datoms :data (into [] datoms) :dst (:uuid data)})
          (on-send {:type :tx :data tx-data-addtl}))))

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
  [{:keys [dispatch]} message]
  (let [data-url (:data message)]
    (dispatch :image/cache data-url)))

(defn handlers []
  (let [dispatch (use-dispatch)
        store    (use-store)
        conn     (uix/context provider.state/context)
        socket   (uix/state nil)
        on-send  (uix/callback
                  (fn [message]
                    (if-let [socket @socket]
                      (if (= (.-readyState socket) 1)
                        (let [{key :entity/key} (ds/entity @conn [:db/ident :local])
                              defaults          {:time (js/Date.now) :src key}]
                          (->> (merge defaults message)
                               (transit/write writer)
                               (.send socket)))))) [])]

    (subscribe-many!
     ;; Open a WebSocket connection and attempt to create a new multiplayer
     ;; lobby with either a new random key or with the key most previously
     ;; used.
     :session/request
     (fn []
       (let [host (ds/entity @conn [:db/ident :local])
             conn (js/WebSocket.
                   (if (:session/last-room host)
                     (str env/SOCKET-URL "?host=" (:session/last-room host))
                     env/SOCKET-URL))]
         (reset! socket conn)))

     ;; Open a WebSocket connection and attempt to join an existing multiplayer
     ;; room with the key given by the query parameter "join".     
     :session/join
     (fn []
       (let [search (.. js/window -location -search)
             params (js/URLSearchParams. search)
             room   (.get params "join")]
         (if (not (nil? room))
           (let [ws (js/WebSocket. (str env/SOCKET-URL "?join=" room))]
             (reset! socket ws)))))

     ;; Closes the WebSocket connection.
     :session/close
     (fn []
       (when-let [ws @socket]
         (.close ws)
         (reset! socket nil)))

     ;; Creates a request for image data from the host of the connected
     ;; session. 
     :image/request
     (fn [{[checksum] :args}]
       (let [session (ds/entity @conn [:db/ident :session])]
         (if-let [host (-> session :session/host :entity/key)]
           (let [data {:name :image/request :checksum checksum}]
             (on-send {:type :event :dst host :data data})))))

     ;; Send all DataScript transactions to all other connections in the
     ;; multiplayer session.
     :tx/commit
     (fn [{[{tx-data :tx-data}] :args}]
       (on-send {:type :tx :data tx-data})))

    ;; Establish a WebSocket connection to the room identified by the "join"
    ;; query parameter in the URL.
    (uix/effect!
     (fn []
       (let [local (ds/entity @conn [:db/ident :local])]
         (if (= (:local/type local) :conn)
           (dispatch :session/join)))) [])

    ;; Periodically attempt to re-establish closed connections.
    (use-interval
     (fn []
       (let [local (ds/entity @conn [:db/ident :local])]
         (if (and (= (:local/type local) :conn)
                  (= (:session/state local) :disconnected))
           (dispatch :session/join)))) 5000)

    (doseq [type ["open" "close" "message" "error"]]
      (listen!
       (let [context {:conn conn :dispatch dispatch :store store}]
         (fn [event]
           (case type
             "open"    (handle-open context event)
             "close"   (handle-close context event)
             "error"   (js/console.log "error" event)
             "message" (let [data (transit/read reader (.-data event))]
                         (handle-message context data on-send)))))
       @socket type [@socket])) nil))
