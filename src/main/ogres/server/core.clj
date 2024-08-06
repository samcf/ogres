(ns ogres.server.core
  (:gen-class)
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.time Instant])
  (:require [clojure.string :refer [upper-case]]
            [clojure.core.async :as async :refer [>!! close!]]
            [cognitect.transit :as transit]
            [datascript.core :as ds]
            [datascript.transit :refer [read-handlers write-handlers]]
            [io.pedestal.http :as http]
            [io.pedestal.websocket :as ws]))

(def conns-chans-xf
  (comp (map val) (map :chan) (filter identity)))

(def sessions (atom {}))

(defn room-create-key []
  (let [keys (:rooms @sessions)]
    (loop []
      (let [code (->> (ds/squuid) (str) (take-last 4) (apply str) (upper-case))]
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

(defn on-open
  "Handles newly created WebSocket connections with the server."
  [session _]
  (let [para (.getRequestParameterMap session)
        host (some-> para (.get "host") (.get 0))
        join (some-> para (.get "join") (.get 0) (upper-case))
        time (.getEpochSecond (Instant/now))
        uuid (.getId session)
        meta {:type :event :time time :src uuid}
        chan (ws/start-ws-connection session {})]

    ;; Increase the max text message size to 10MB to allow for large files to
    ;; be exchanged over the socket, such as image data URLs.
    (.setMaxTextMessageBufferSize session 1e7)

    (cond host
          (do (swap! sessions room-create host uuid chan)
              (->> (assoc meta :data {:name :session/created :room host :uuid uuid} :dst uuid)
                   (marshall)
                   (>!! chan)))

          join
          (let [state (swap! sessions room-join join uuid chan)
                data  (assoc meta :data {:name :session/joined :room join :uuid uuid} :dst uuid)]
            (>!! chan (marshall data))
            (let [conns (disj (get-in state [:rooms join :conns]) uuid)
                  chans (into [] conns-chans-xf (select-keys (:conns state) conns))]
              (doseq [chan chans]
                (->> (assoc meta :data {:name :session/join :room join :uuid uuid})
                     (marshall)
                     (>!! chan)))))

          :else
          (let [room (room-create-key)]
            (swap! sessions room-create room uuid chan)
            (->> (assoc meta :data {:name :session/created :room room :uuid uuid} :dst uuid)
                 (marshall)
                 (>!! chan))))
    {:uuid uuid}))

(defn on-text
  "Handles incoming messages."
  [{uuid :uuid} message]
  (let [state @sessions]
    (if-let [room (uuid->room state uuid)]
      (let [recip (-> (ByteArrayInputStream. (.getBytes message))
                      (transit/reader :json {:handlers read-handlers})
                      (transit/read)
                      (:dst))
            uuids (if (some? recip) #{recip} (disj (:conns room) uuid))
            chans (sequence (comp (map (:conns state)) (map :chan) (filter identity)) uuids)]
        (doseq [chan chans]
          (>!! chan message)))
      (close! (get-in state [:conns uuid :chan])))))

(defn on-close
  "Handles closed connections."
  [_ session _]
  (let [uuid (.getId session)
        sess @sessions
        room (get-in sess [:conns uuid :room])
        self (get-in sess [:conns uuid :chan])
        host (get-in sess [:rooms room :host])
        rest (->> (disj (get-in sess [:rooms room :conns]) uuid)
                  (select-keys (:conns sess))
                  (into [] conns-chans-xf))]

    ;; The connection has been closed; close the associated channel.
    (if (some? self)
      (close! self))

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
    (swap! sessions room-leave uuid)))

(defn on-error
  [_ _ ex]
  (prn ex))

(defn root-handler [_]
  {:status 200 :body "This server manages multiplayer sessions for https://ogres.app, a free and open-source virtual tabletop application."})

(defn create-server
  ([] (create-server {}))
  ([{:keys [port] :or {port 5000}}]
   (->> {::http/routes #{["/" :get `root-handler]}
         ::http/type :jetty
         ::http/host "0.0.0.0"
         ::http/port port
         ::http/websockets
         {"/ws"
          {:on-open  on-open
           :on-text  on-text
           :on-close on-close
           :on-error on-error}}}
        (http/default-interceptors))))

(defn create-dev-server []
  (-> (create-server)
      (merge {::http/join? false})
      (http/dev-interceptors)
      (http/create-server)))

(defn ^:export run-development [& _args]
  (let [server (create-dev-server)]
    (println "Starting the development server on port" (::http/port server))
    (http/start server)))

(defn -main [port]
  (-> (create-server {:port (Integer/parseInt port)})
      (http/create-server)
      (http/start)))
