(ns ogre.server.core
  (:import [java.time Instant] [java.io ByteArrayOutputStream])
  (:require [hashids.core :as hash]
            [clojure.core.async :as async :refer [go <! >! timeout]]
            [io.pedestal.http :as server]
            [io.pedestal.http.jetty.websockets :as ws]
            [cognitect.transit :as transit]))

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

(defn transit-encode [value]
  (let [out (ByteArrayOutputStream.)
        wrt (transit/writer out :json)]
    (transit/write wrt value)
    (.toString out)))

(transit-encode {:foo 42})

(defn on-connect
  [uuid]
  (fn [session chan]
    (if-let [param (.. session getUpgradeRequest getParameterMap (get "key"))]
      (let [room (.get param 0)]
        (if (get-in @sessions [:rooms room])
          (do (swap! sessions room-join room uuid chan)
              (go
                (<! (timeout 32))
                (>! chan (transit-encode {:room room}))))
          (async/close! chan)))
      (let [room (room-create-key)]
        (swap! sessions room-create room uuid chan)
        (go
          (<! (timeout 32))
          (>! chan (transit-encode {:room room})))))))

(defn on-text
  [uuid]
  (fn [body]))

(defn on-binary
  [uuid]
  (fn [body offset length]))

(defn on-error
  [uuid]
  (fn [ex]))

(defn on-close
  [uuid]
  (fn [code reason]
    (let [sess @sessions
          room (get-in sess [:conns uuid :room])
          host (get-in sess [:rooms room :host])]
      (if (= uuid host)
        (let [conns (get-in sess [:rooms room :conns])
              chans (into [] (comp (map val) (map :chan)) (select-keys (:conns sess) conns))]
          (doseq [chan chans]
            (async/close! chan)))
        (if-let [chan (get-in sess [:conns uuid :chan])]
          (async/close! chan)))
      (swap! sessions room-leave uuid))))

(defn actions [uuid]
  {:on-connect (ws/start-ws-connection (on-connect uuid))
   :on-text    (on-text uuid)
   :on-binary  (on-binary uuid)
   :on-error   (on-error uuid)
   :on-close   (on-close uuid)})

(defn create-listener [_ _ handlers-fn]
  (let [uuid (java.util.UUID/randomUUID)]
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
