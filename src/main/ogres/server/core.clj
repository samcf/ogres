(ns ogres.server.core
  (:gen-class)
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.nio ByteBuffer]
           [java.util UUID]
           [org.msgpack.core MessagePack])
  (:require [clojure.core.async :as async]
            [clojure.string :refer [upper-case]]
            [cognitect.transit :as transit]
            [datascript.core :as ds]
            [datascript.transit :refer [read-handlers write-handlers]]
            [io.pedestal.connector :as conn]
            [io.pedestal.http.jetty :as jetty]
            [io.pedestal.websocket :as ws]))

(def sessions (atom {}))

(def conns-chans-xf
  (comp (map val) (map :chan) (filter identity)))

(defn start-ws-conn [session chan]
  (async/go-loop []
    (if-let [message (and (.isOpen session) (async/<! chan))]
      (do (condp instance? message
            String     (.sendText   (.getAsyncRemote session) message)
            ByteBuffer (.sendBinary (.getAsyncRemote session) message))
          (recur))
      (.close session))))

(defn room-create-key []
  (let [keys (:rooms @sessions)]
    (loop []
      (let [code (->> (datascript.core/squuid) (str) (take-last 4) (apply str) (upper-case))]
        (if (contains? keys code) (recur) code)))))

(defn room-create
  [sessions room conn chan]
  (-> sessions
      (update-in [:conns conn] assoc :chan chan :room room)
      (update-in [:rooms room] assoc :conns #{conn} :host conn)))

(defn room-join
  [sessions room conn chan]
  (-> sessions
      (update-in [:conns conn] assoc :chan chan :room room)
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

(defn handle-root [_request]
  {:status 405})

(defn handle-ws [request]
  (let [{:keys [params]} request
        sessions (deref sessions)]
    (cond (and (:host params) (:join params))
          {:status 400}
          (and (:host params) (get-in sessions [:rooms (:host params)]))
          {:status 403}
          (and (:join params) (nil? (get-in sessions [:rooms (upper-case (:join params))])))
          {:status 404})))

(defn handle-ws-open [session _]
  (.setMaxTextMessageBufferSize   session 1e7)
  (.setMaxBinaryMessageBufferSize session 1e7)
  (let [params (.getRequestParameterMap session)
        host (some-> params (.get "host") (.get 0))
        join (some-> params (.get "join") (.get 0) (upper-case))
        uuid (random-uuid)
        chan (async/chan)]
    (start-ws-conn session chan)
    (cond (some? host)
          (do (swap! sessions room-create host uuid chan)
              (async/>!! chan (marshall {:type :event :src uuid :dst uuid :data {:name :session/created :room host :uuid uuid}})))
          (some? join)
          (let [sessions (swap! sessions room-join join uuid chan)]
            (async/>!! chan (marshall {:type :event :src uuid :dst uuid :data {:name :session/joined :room join :uuid uuid}}))
            (let [conns (disj (get-in sessions [:rooms join :conns]) uuid)
                  chans (into [] conns-chans-xf (select-keys (:conns sessions) conns))]
              (doseq [chan chans]
                (async/>!! chan (marshall {:type :event :src uuid :data {:name :session/join :room join :uuid uuid}})))))
          :else
          (let [room (room-create-key)]
            (swap! sessions room-create room uuid chan)
            (async/>!! chan (marshall {:type :event :src uuid :dst uuid :data {:name :session/created :room room :uuid uuid}}))))
    uuid))

(defn handle-ws-close [uuid _ _]
  (let [sess @sessions
        room (get-in sess [:conns uuid :room])
        self (get-in sess [:conns uuid :chan])
        host (get-in sess [:rooms room :host])
        rest (->> (disj (get-in sess [:rooms room :conns]) uuid)
                  (select-keys (:conns sess))
                  (into [] conns-chans-xf))]

    ;; The connection has been closed; close the associated channel.
    (async/close! self)

    (if (= uuid host)
      ;; The host has left, destroying the session entirely. Find and close
      ;; all remaining connections.
      (doseq [chan rest]
        (async/close! chan))

      ;; Notify all other connections in the same session that a connection
      ;; has been closed.
      (doseq [chan rest]
        (->> {:type :event :data {:name :session/leave :uuid uuid}}
             (marshall)
             (async/>!! chan))))

    ;; Update the sessions to remove the closing connection, potentially
    ;; also removing the room and closing all related connections within.
    (swap! sessions room-leave uuid)))

(defn handle-ws-error [_ _ error]
  (prn error))

(defn handle-ws-text [uuid message]
  (let [sessions (deref sessions)
        room     (uuid->room sessions uuid)]
    (if room
      (let [stream (ByteArrayInputStream. (.getBytes message))
            reader (transit/reader stream :json {:handlers read-handlers})
            decode (transit/read reader)
            recips (if (uuid? (:dst decode)) #{(:dst decode)} (disj (:conns room) uuid))
            xf     (comp (map (:conns sessions)) (map :chan) (filter identity))]
        (doseq [chan (sequence xf recips)]
          (async/>!! chan message)))
      (async/close! (get-in sessions [:conns uuid :chan])))))

(defn handle-ws-binary [uuid message]
  (let [data (deref sessions)]
    (if (uuid->room data uuid)
      (let [unpacker (MessagePack/newDefaultUnpacker message)
            max-keys (.unpackMapHeader unpacker)]
        (loop [idx 0]
          (if (< idx max-keys)
            (if (= (.unpackString unpacker) "dst")
              (let [dest (UUID/fromString (.unpackString unpacker))
                    chan (get-in data [:conns dest :chan])]
                (when (some? chan)
                  (.close unpacker)
                  (async/>!! chan (.asReadOnlyBuffer message))))
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

(defn run-development [opts]
  (conn/start! (create-connector opts)))

(defn -main [port]
  (conn/start! (create-connector {:port (Integer/parseInt port)})))
