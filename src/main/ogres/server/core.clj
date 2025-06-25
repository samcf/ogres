(ns ogres.server.core
  (:gen-class)
  (:refer-clojure :exclude [send])
  (:import [clojure.lang IPersistentMap]
           [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.nio ByteBuffer]
           [org.msgpack.core MessagePack])
  (:require [clojure.string :refer [upper-case]]
            [cognitect.transit :as transit]
            [datascript.core]
            [datascript.transit :refer [read-handlers write-handlers]]
            [io.pedestal.connector :as conn]
            [io.pedestal.http.jetty :as jetty]
            [io.pedestal.log :as log]
            [io.pedestal.websocket :as ws]))

(def state! (atom {}))
(def opts-reader {:handlers read-handlers})
(def opts-writer {:handlers write-handlers})

(defn room-create-key []
  (let [keys (:rooms (deref state!))]
    (loop []
      (let [code (->> (datascript.core/squuid) (str) (take-last 4) (apply str) (upper-case))]
        (if (contains? keys code) (recur) code)))))

(defn room-create [data room uuid session]
  (-> data
      (update-in [:conns uuid] assoc :session session :room room)
      (update-in [:rooms room] assoc :conns #{uuid} :host uuid)))

(defn room-join [data room uuid session]
  (-> data
      (update-in [:conns uuid] assoc :session session :room room)
      (update-in [:rooms room :conns] conj uuid)))

(defn room-leave [data uuid]
  (let [room (get-in data [:conns uuid :room])]
    (if-let [host (get-in data [:rooms room :host])]
      (cond-> data
        true             (update :conns dissoc uuid)
        (= uuid host)    (update :rooms dissoc room)
        (not= uuid host) (update-in [:rooms room :conns] disj uuid))
      (update data :conns dissoc uuid))))

(defn uuid->room [data uuid]
  (let [room (get-in data [:conns uuid :room])]
    (get-in data [:rooms room])))

(defn uuid->conns [data uuid]
  (let [room  (get-in data [:conns uuid :room])
        uuids (get-in data [:rooms room :conns])]
    (into [] (comp (map (:conns data)) (map :session))
          (disj uuids uuid))))

(defn encode [value]
  (let [stream (ByteArrayOutputStream.)
        writer (transit/writer stream :json opts-writer)]
    (transit/write writer value)
    (.toString stream)))

(defn send [session message]
  (when (.isOpen session)
    (condp instance? message
      String
      (.sendText (.getAsyncRemote session) message)
      ByteBuffer
      (.sendBinary (.getAsyncRemote session) message)
      IPersistentMap
      (.sendText (.getAsyncRemote session) (encode message)))))

(defn send-many [sessions message]
  (condp instance? message
    String
    (doseq [session sessions :when (.isOpen session)]
      (.sendText (.getAsyncRemote session) message))
    ByteBuffer
    (doseq [session sessions :when (.isOpen session)]
      (.sendBinary (.getAsyncRemote session) message))
    IPersistentMap
    (let [serialized (encode message)]
      (doseq [session sessions :when (.isOpen session)]
        (.sendText (.getAsyncRemote session) serialized)))))

(defn handle-root [_]
  {:status 405})

(defn handle-ws [{{host :host join :join} :params}]
  (let [data (deref state!)]
    (cond (and host join)
          {:status 400}
          (and host (get-in data [:rooms host]))
          {:status 403}
          (and join (nil? (get-in data [:rooms (upper-case join)])))
          {:status 404})))

(defn handle-ws-open [session _]
  (.setMaxTextMessageBufferSize   session 1e7)
  (.setMaxBinaryMessageBufferSize session 1e7)
  (let [params (.getRequestParameterMap session)
        host (some-> params (.get "host") (.get 0))
        join (some-> params (.get "join") (.get 0) (upper-case))
        uuid (.getId session)]
    (cond (some? host)
          (do (swap! state! room-create host uuid session)
              (send session {:type :event :src uuid :dst uuid :data {:name :session/created :room host :uuid uuid}}))
          (some? join)
          (let [data (swap! state! room-join join uuid session)]
            (send session {:type :event :src uuid :dst uuid :data {:name :session/joined :room join :uuid uuid}})
            (send-many (uuid->conns data uuid) {:type :event :src uuid :data {:name :session/join :room join :uuid uuid}}))
          :else
          (let [room (room-create-key)]
            (swap! state! room-create room uuid session)
            (send session {:type :event :src uuid :dst uuid :data {:name :session/created :room room :uuid uuid}})))
    session))

(defn handle-ws-close [session _ _]
  (let [data (deref state!)
        uuid (.getId session)
        room (get-in data [:conns uuid :room])
        room (get-in data [:rooms room])
        conns (uuid->conns data uuid)]

    ;; The connection has been closed; close the associated session.
    (when (.isOpen session)
      (.close session))

    (if (= (:host room) uuid)
      ;; The host has left, destroying the session entirely. Find and close
      ;; all remaining connections.
      (doseq [session conns :when (.isOpen session)]
        (.close session))

      ;; Notify all other connections in the same session that a connection
      ;; has been closed.
      (send-many conns {:type :event :data {:name :session/leave :uuid uuid}}))

    ;; Update the sessions to remove the closing connection, potentially
    ;; also removing the room and closing all related connections within.
    (swap! state! room-leave uuid)))

(defn handle-ws-error [_ _ error]
  (log/error :message (.getMessage error)))

(defn handle-ws-text [session message]
  (let [data (deref state!)
        uuid (.getId session)]
    (if (uuid->room data uuid)
      (let [stream (ByteArrayInputStream. (.getBytes message))
            reader (transit/reader stream :json opts-reader)
            decode (transit/read reader)]
        (if-let [uuid (:dst decode)]
          (send      (get-in data [:conns uuid :session]) message)
          (send-many (uuid->conns data uuid) message))))))

(defn handle-ws-binary [session message]
  (let [data (deref state!)
        uuid (.getId session)]
    (if (uuid->room data uuid)
      (let [unpacker (MessagePack/newDefaultUnpacker message)
            max-keys (.unpackMapHeader unpacker)]
        (loop [idx 0]
          (if (< idx max-keys)
            (if (= (.unpackString unpacker) "dst")
              (let [dest (.unpackString unpacker)]
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

(defn -main [port]
  (conn/start! (create-connector {:port (Integer/parseInt port)})))
