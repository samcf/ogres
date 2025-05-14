(ns ogres.server.core
  (:gen-class)
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.nio ByteBuffer]
           [java.time Instant]
           [java.util UUID]
           [org.msgpack.core MessagePack])
  (:require [clojure.string :refer [upper-case]]
            [clojure.core.async :as async :refer [go <! >! >!! close! timeout]]
            [cognitect.transit :as transit]
            [datascript.core :as ds]
            [datascript.transit :refer [read-handlers write-handlers]]
            [io.pedestal.http :as server]
            [io.pedestal.http.jetty.websockets :as ws]))

(def conns-chans-xf
  (comp (map val) (map :chan) (filter identity)))

(def sessions (atom {}))

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

(defn on-connect
  [uuid]
  (fn [session chan]

    ;; Increase the max message size to allow for large messages such as
    ;; image data.
    (.setMaxBinaryMessageSize (.getPolicy session) 1e7)
    (.setMaxTextMessageSize   (.getPolicy session) 1e7)

    (let [params (.. session (getUpgradeRequest) (getParameterMap))
          host   (some-> params (.get "host") (.get 0))
          join   (some-> params (.get "join") (.get 0) (upper-case))
          time   (.getEpochSecond (Instant/now))
          meta   {:type :event :time time :src uuid}]
      (cond (string? host)
            (let [_ (swap! sessions room-create host uuid chan)]
              (go (<! (timeout 32))
                  (->> (assoc meta :data {:name :session/created :room host :uuid uuid} :dst uuid)
                       (marshall)
                       (>! chan))))

            (string? join)
            (let [sessions (swap! sessions room-join join uuid chan)]
              (go (<! (timeout 32))
                  (->> (assoc meta :data {:name :session/joined :room join :uuid uuid} :dst uuid)
                       (marshall)
                       (>! chan))
                  (let [conns (disj (get-in sessions [:rooms join :conns]) uuid)
                        chans (into [] conns-chans-xf (select-keys (:conns sessions) conns))]
                    (doseq [chan chans]
                      (->> (assoc meta :data {:name :session/join :room join :uuid uuid})
                           (marshall)
                           (>! chan))))))

            :else
            (let [room (room-create-key)
                  _    (swap! sessions room-create room uuid chan)]
              (go (<! (timeout 32))
                  (->> (assoc meta :data {:name :session/created :room room :uuid uuid} :dst uuid)
                       (marshall)
                       (>! chan))))))))

(defn on-text [uuid]
  (fn [data]
    (let [sessions (deref sessions)
          room     (uuid->room sessions uuid)]
      (if (some? room)
        (let [stream (ByteArrayInputStream. (.getBytes data))
              reader (transit/reader stream :json {:handlers read-handlers})
              decode (transit/read reader)
              recips (if (uuid? (:dst decode)) #{(:dst decode)} (disj (:conns room) uuid))
              xf     (comp (map (:conns sessions)) (map :chan) (filter identity))]
          (doseq [chan (sequence xf recips)]
            (>!! chan data)))
        (close! (get-in sessions [:conns uuid :chan]))))))

(defn on-binary [uuid]
  (fn [data offset len]
    (let [sessions (deref sessions)]
      (if (uuid->room sessions uuid)
        (let [unpacker (MessagePack/newDefaultUnpacker data offset len)
              max-keys (.unpackMapHeader unpacker)]
          (loop [idx 0]
            (if (< idx max-keys)
              (if (= (.unpackString unpacker) "dst")
                (let [dest (UUID/fromString (.unpackString unpacker))
                      chan (get-in sessions [:conns dest :chan])]
                  (when (some? chan)
                    (.close unpacker)
                    (>!! chan (ByteBuffer/wrap data offset len))))
                (do (.skipValue unpacker)
                    (recur (inc idx))))))
          (.close unpacker))))))

(defn on-close
  [uuid]
  (fn [_ _]
    (let [sess @sessions
          room (get-in sess [:conns uuid :room])
          self (get-in sess [:conns uuid :chan])
          host (get-in sess [:rooms room :host])
          rest (->> (disj (get-in sess [:rooms room :conns]) uuid)
                    (select-keys (:conns sess))
                    (into [] conns-chans-xf))]

      ;; The connection has been closed; close the associated channel.
      (close! self)

      (if (= uuid host)
        ;; The host has left, destroying the session entirely. Find and close
        ;; all remaining connections.
        (doseq [chan rest]
          (close! chan))

        ;; Notify all other connections in the same session that a connection
        ;; has been closed.
        (doseq [chan rest]
          (->> {:type :event :time "" :data {:name :session/leave :uuid uuid}}
               (marshall)
               (>!! chan))))

      ;; Update the sessions to remove the closing connection, potentially
      ;; also removing the room and closing all related connections within.
      (swap! sessions room-leave uuid))))

(defn on-error
  [uuid]
  (fn [ex]
    (println ex)
    ((on-close uuid) nil nil)))

(defn actions [uuid]
  {:on-connect (ws/start-ws-connection (on-connect uuid))
   :on-text    (on-text uuid)
   :on-binary  (on-binary uuid)
   :on-error   (on-error uuid)
   :on-close   (on-close uuid)})

(defn create-listener [req res handlers-fn]
  (let [params (.. req getParameterMap)
        host   (some-> params (.get "host") (.get 0))
        join   (some-> params (.get "join") (.get 0) (upper-case))]
    (cond (and host join)
          (.sendForbidden res "Query parameters cannot contain host and join parameters.")

          (and host (get-in @sessions [:rooms host]))
          (.sendForbidden res "Cannot create room with that key; it already exists.")

          (and join (nil? (get-in @sessions [:rooms join])))
          (.sendForbidden res "Cannot join room with that key; it does not exist.")

          :else
          (ws/make-ws-listener (handlers-fn (ds/squuid))))))

(defn root-handler [_]
  {:status 200 :body "This server manages multiplayer sessions for https://ogres.app, a free and open-source virtual tabletop application."})

(defn create-server
  ([] (create-server {}))
  ([{:keys [port] :or {port 5000}}]
   (server/default-interceptors
    {:env :prod
     ::server/routes #{["/" :get `root-handler]}
     ::server/type :jetty
     ::server/host "0.0.0.0"
     ::server/port port
     ::server/container-options
     {:context-configurator
      (fn [context]
        (ws/add-ws-endpoints
         context
         {"/ws" actions}
         {:listener-fn create-listener}))}})))

(defn create-dev-server
  ([] (create-dev-server {}))
  ([opts] ; Modified to accept opts
   (-> (create-server opts) ; Modified to pass opts
       (merge {:env :dev ::server/join? false})
       (server/dev-interceptors)
       (server/create-server))))

(defn ^:export run-development [args-map] ; Modified to use args-map
  (let [server (create-dev-server args-map)] ; Modified to pass args-map
    (println "Starting the development server on port" (::server/port server))
    (server/start server)))

(defn -main [port]
  (-> (create-server {:port (Integer/parseInt port)})
      (server/create-server)
      (server/start)))
