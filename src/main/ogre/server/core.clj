(ns ogre.server.core
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.time Instant])
  (:require [clojure.core.async :as async :refer [go <! >! >!! close! timeout]]
            [cognitect.transit :as transit]
            [datascript.core :as ds]
            [datascript.transit :refer [read-handlers write-handlers]]
            [hashids.core :as hash]
            [io.pedestal.http :as server]
            [io.pedestal.http.jetty.websockets :as ws]))

(def sessions (atom {}))

(defn room-create-key []
  (let [opts {:min-length 8 :salt "ogre.tools"}
        time (.getEpochSecond (Instant/now))
        rand (rand-int 4096)]
    (hash/encode opts [time rand])))

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
  (let [room (get-in sessions [:conns conn :room])
        host (get-in sessions [:rooms room :host])]
    (if (= host conn)
      (let [conns (get-in sessions [:rooms room :conns])
            seshs (update sessions :rooms dissoc room)]
        (apply update seshs :conns dissoc conns))
      (-> (update-in sessions [:rooms room :conns] disj conn)
          (update :conns dissoc conn)))))

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
    (if-let [param (.. session getUpgradeRequest getParameterMap (get "key"))]

      ;; Join this connection to the session identified by the "key" query parameter,
      ;; send a response message back to the new connection, and notify all other
      ;; connection in the room that someone has joined.
      (let [sess @sessions room (.get param 0)]
        (if (get-in sess [:rooms room])
          (do (swap! sessions room-join room uuid chan)
              (go (<! (timeout 32))
                  (->> {:type :event :data {:name :session/joined :room room :uuid uuid} :time "" :dst uuid} (marshall) (>! chan))
                  (let [conns (disj (get-in sess [:rooms room :conns]) uuid)
                        chans (into [] (comp (map val) (map :chan)) (select-keys (:conns sess) conns))]
                    (doseq [chan chans]
                      (->> {:type :event :data {:name :session/join :room room :uuid uuid} :time "" :src uuid} (marshall) (>! chan))))))

          ;; The session identified by the query parameter doesn't exist; simply
          ;; close the connection.
          (async/close! chan)))

      ;; The relevant query parameter wasn't found so we'll assume the user wants
      ;; to create a new session and be the host of it.
      (let [room (room-create-key)]
        (swap! sessions room-create room uuid chan)
        (go (<! (timeout 32))
            (->> {:type :event :data {:name :session/created :room room :uuid uuid} :time "" :src uuid :dst uuid}
                 (marshall)
                 (>! chan)))))))

(defn on-text
  [uuid]
  (fn [body]
    (let [sesshs @sessions
          stream (ByteArrayInputStream. (.getBytes body))
          reader (transit/reader stream :json {:handlers read-handlers})
          parsed (transit/read reader)
          roomid (get-in sesshs [:conns uuid :room])
          conns  (get-in sesshs [:rooms roomid :conns])
          chans  (if (uuid? (:dst parsed))
                   [(get-in sesshs [:conns (:dst parsed) :chan])]
                   (into [] (comp (map val) (map :chan)) (select-keys (:conns sesshs) (disj conns uuid))))]
      (doseq [chan chans]
        (>!! chan body)))))

(defn on-binary
  [uuid]
  (fn [body offset length]))

(defn on-error
  [uuid]
  (fn [ex]))

(defn on-close
  [uuid]
  (fn [_ _]
    (let [sess  @sessions
          room  (get-in sess [:conns uuid :room])
          host  (get-in sess [:rooms room :host])
          chan  (get-in sess [:conns uuid :chan])
          chans (->> (disj (get-in sess [:rooms room :conns]) uuid)
                     (select-keys (:conns sess))
                     (into [] (comp (map val) (map :chan))))]

      ;; The connection has been closed; close the associated channel.
      (close! chan)

      (if (= uuid host)
        ;; The host has left, destroying the session entirely. Find and close
        ;; all remaining connections.
        (doseq [chan chans]
          (close! chan))

        ;; Notify all other connections in the same session that a connection
        ;; has been closed.
        (doseq [chan chans]
          (->> {:type :event :time "" :data {:name :session/leave :uuid uuid}}
               (marshall)
               (>!! chan))))

      ;; Update the sessions to remove the closing connection, potentially
      ;; also removing the room and closing all related connections within.
      (swap! sessions room-leave uuid))))

(defn actions [uuid]
  {:on-connect (ws/start-ws-connection (on-connect uuid))
   :on-text    (on-text uuid)
   :on-binary  (on-binary uuid)
   :on-error   (on-error uuid)
   :on-close   (on-close uuid)})

(defn create-listener [_ _ handlers-fn]
  (let [uuid (ds/squuid)]
    (ws/make-ws-listener (handlers-fn uuid))))

(defn create-server []
  {:env :dev
   ::server/routes #{}
   ::server/type :jetty
   ::server/port 80
   ::server/container-options
   {:context-configurator
    (fn [context]
      (ws/add-ws-endpoints
       context
       {"/ws" actions}
       {:listener-fn create-listener}))}})

(defn create-dev-server []
  (-> (create-server)
      (merge {:env :dev
              ::server/join? false
              ::server/port 5000
              ::server/allowed-origins
              {:creds true :allowed-origins (constantly true)}
              ::server/secure-headers
              {:content-security-policy-settings
               {:object-src "'none'"}}})
      (server/default-interceptors)
      (server/dev-interceptors)
      (server/create-server)))

(defn run-development [& _args]
  (let [server (create-dev-server)]
    (println "Starting the development server on port" (::server/port server))
    (server/start server)))

(defn -main [& _args]
  (let [server (server/create-server (create-server))]
    (server/start server)))
