(ns ogres.server.core
  (:gen-class)
  (:import [java.io ByteArrayOutputStream ByteArrayInputStream]
           [java.time Instant])
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

    ;; Increase the max text message size to 10MB to allow for large files to
    ;; be exchanged over the socket, such as image data URLs.
    (.setMaxTextMessageSize (.getPolicy session) 1e7)

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

(defn uuid->room
  [sessions uuid]
  (let [room (get-in sessions [:conns uuid :room])]
    (get-in sessions [:rooms room])))

(defn on-text
  [uuid]
  (fn [body]
    (let [state @sessions]
      (if-let [room (uuid->room state uuid)]
        (let [recip (-> (ByteArrayInputStream. (.getBytes body))
                        (transit/reader :json {:handlers read-handlers})
                        (transit/read)
                        (:dst))
              uuids (if (uuid? recip) #{recip} (disj (:conns room) uuid))
              chans (sequence (comp (map (:conns state)) (map :chan) (filter identity)) uuids)]
          (doseq [chan chans]
            (>!! chan body)))
        (close! (get-in state [:conns uuid :chan]))))))

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
   (->> {:env :prod
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
             {:listener-fn create-listener}))}}
        (server/default-interceptors))))

(defn create-dev-server []
  (-> (create-server)
      (merge {:env :dev ::server/join? false})
      (server/dev-interceptors)
      (server/create-server)))

(defn ^:export run-development [& _args]
  (let [server (create-dev-server)]
    (println "Starting the development server on port" (::server/port server))
    (server/start server)))

(defn -main [port]
  (-> (create-server {:port (Integer/parseInt port)})
      (server/create-server)
      (server/start)))
