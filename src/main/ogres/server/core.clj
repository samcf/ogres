(ns ogres.server.core
  (:gen-class)
  (:refer-clojure :exclude [send])
  (:import [clojure.lang IPersistentMap]
           [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.nio ByteBuffer]
           [java.util UUID]
           [org.msgpack.core MessagePack])
  (:require [clojure.string :refer [upper-case]]
            [cognitect.transit :as transit]
            [datascript.core :as ds]
            [datascript.transit :refer [read-handlers write-handlers]]
            [io.pedestal.connector :as conn]
            [io.pedestal.http.jetty :as jetty]
            [io.pedestal.websocket :as ws]))

(def sessions (atom {}))

(def conns-sessions-xf
  (comp (map val) (map :session) (filter identity)))

(defn room-create-key []
  (let [keys (:rooms (deref sessions))]
    (loop []
      (let [code (->> (datascript.core/squuid) (str) (take-last 4) (apply str) (upper-case))]
        (if (contains? keys code) (recur) code)))))

(defn room-create
  [sessions room conn session]
  (-> sessions
      (update-in [:conns conn] assoc :session session :room room)
      (update-in [:rooms room] assoc :conns #{conn} :host conn)))

(defn room-join
  [sessions room conn session]
  (-> sessions
      (update-in [:conns conn] assoc :session session :room room)
      (update-in [:rooms room :conns] conj conn)))

(defn room-leave
  [sessions conn]
  (let [room (get-in sessions [:conns conn :room])]
    (if-let [host (get-in sessions [:rooms room :host])]
      (cond-> sessions
        true             (update :conns dissoc conn)
        (= conn host)    (update :rooms dissoc room)
        (not= conn host) (update-in [:rooms room :conns] disj conn))
      (update sessions :conns dissoc conn))))

(defn uuid->room
  [sessions uuid]
  (let [room (get-in sessions [:conns uuid :room])]
    (get-in sessions [:rooms room])))

(defn marshall
  "Serializes the given value as JSON compressed EDN."
  [value]
  (let [stream (ByteArrayOutputStream.)
        writer (transit/writer stream :json {:handlers write-handlers})]
    (transit/write writer value)
    (.toString stream)))

(defn send [session message]
  (when (.isOpen session)
    (condp instance? message
      IPersistentMap (.sendText   (.getAsyncRemote session) (marshall message))
      String         (.sendText   (.getAsyncRemote session) message)
      ByteBuffer     (.sendBinary (.getAsyncRemote session) message))))

(defn send-many [sessions message]
  (condp instance? message
    IPersistentMap
    (let [serialized (marshall message)]
      (doseq [session sessions]
        (.sendText (.getAsyncRemote session) serialized)))
    String
    (doseq [session sessions]
      (.sendText (.getAsyncRemote session) message))
    ByteBuffer
    (doseq [session sessions]
      (.sendBinary (.getAsyncRemote session) message))))

(defn handle-root [_request]
  {:status 405})

(defn handle-ws [{{host :host join :join} :params}]
  (let [data (deref sessions)]
    (cond (and host join)
          {:status 400}
          (and host (get-in data [:rooms host]))
          {:status 403}
          (and join (nil? (get-in data [:rooms (upper-case join)])))
          {:status 404})))

(defn handle-ws-open [session _]
  (.setMaxTextMessageBufferSize session 1e7)
  (.setMaxBinaryMessageBufferSize session 1e7)
  (let [params (.getRequestParameterMap session)
        host (some-> params (.get "host") (.get 0))
        join (some-> params (.get "join") (.get 0) (upper-case))
        uuid (random-uuid)]
    (cond (some? host)
          (do (swap! sessions room-create host uuid session)
              (send session {:type :event :src uuid :dst uuid :data {:name :session/created :room host :uuid uuid}}))
          (some? join)
          (let [sessions (swap! sessions room-join join uuid session)]
            (send session {:type :event :src uuid :dst uuid :data {:name :session/joined :room join :uuid uuid}})
            (let [conns (disj (get-in sessions [:rooms join :conns]) uuid)]
              (send-many
               (sequence conns-sessions-xf (select-keys (:conns sessions) conns))
               {:type :event :src uuid :data {:name :session/join :room join :uuid uuid}})))
          :else
          (let [room (room-create-key)]
            (swap! sessions room-create room uuid session)
            (send session {:type :event :src uuid :dst uuid :data {:name :session/created :room room :uuid uuid}})))
    uuid))

(defn handle-ws-close [uuid _ _]
  (let [data (deref sessions)
        room (get-in data [:conns uuid :room])
        self (get-in data [:conns uuid :session])
        host (get-in data [:rooms room :host])
        rest (->> (disj (get-in data [:rooms room :conns]) uuid)
                  (select-keys (:conns data))
                  (into [] conns-sessions-xf))]

    ;; The connection has been closed; close the associated session.
    (.close self)

    (if (= uuid host)
      ;; The host has left, destroying the session entirely. Find and close
      ;; all remaining connections.
      (doseq [session rest]
        (.close session))

      ;; Notify all other connections in the same session that a connection
      ;; has been closed.
      (send-many rest {:type :event :data {:name :session/leave :uuid uuid}}))

    ;; Update the sessions to remove the closing connection, potentially
    ;; also removing the room and closing all related connections within.
    (swap! sessions room-leave uuid)))

(defn handle-ws-error [_ _ error]
  (prn error))

(defn handle-ws-text [uuid message]
  (let [sessions (deref sessions)]
    (if-let [room (uuid->room sessions uuid)]
      (let [stream (ByteArrayInputStream. (.getBytes message))
            reader (transit/reader stream :json {:handlers read-handlers})
            decode (transit/read reader)
            recips (if (uuid? (:dst decode)) #{(:dst decode)} (disj (:conns room) uuid))
            xf     (comp (map (:conns sessions)) (map :session) (filter identity))]
        (send-many (sequence xf recips) message))
      (when-let [session [:conns uuid :session]]
        (.close session)))))

(defn handle-ws-binary [uuid message]
  (let [data (deref sessions)]
    (if (uuid->room data uuid)
      (let [unpacker (MessagePack/newDefaultUnpacker message)
            max-keys (.unpackMapHeader unpacker)]
        (loop [idx 0]
          (if (< idx max-keys)
            (if (= (.unpackString unpacker) "dst")
              (let [dest (UUID/fromString (.unpackString unpacker))]
                (when-let [session (get-in data [:conns dest :session])]
                  (send session message)))
              (do (.skipValue unpacker)
                  (recur (inc idx))))))
        (.close unpacker)))))

(def upgrade-ws
  (ws/websocket-upgrade
   {:on-open   handle-ws-open
    :on-close  handle-ws-close
    :on-error  handle-ws-error
    :on-text   handle-ws-text
    :on-binary handle-ws-binary}))

(defn create-connector
  ([] (create-connector {}))
  ([{:keys [port] :or {port 5000}}]
   (-> (conn/default-connector-map port)
       (conn/with-default-interceptors)
       (conn/with-routes
         #{["/"   :get [handle-root]]
           ["/ws" :get [handle-ws upgrade-ws]]})
       (jetty/create-connector nil))))

(defn ^:export run-development [opts]
  (conn/start! (create-connector opts)))

(defn -main [port]
  (conn/start! (create-connector {:port (Integer/parseInt port)})))
