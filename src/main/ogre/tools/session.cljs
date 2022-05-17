(ns ogre.tools.session
  (:require [datascript.core :as ds]
            [ogre.tools.render :refer [listen!]]
            [ogre.tools.state :refer [state]]
            [uix.core.alpha :as uix]
            [cognitect.transit :as transit]))

;; {:src #uuid "" :dst #uuid "" :time "" :data [] :type :datoms}
;; {:src #uuid "" :dst #uuid "" :time "" :data [] :type :tx}
;; {:src #uuid "" :dst #uuid "" :time "" :data "" :type :image}
;; {:src #uuid "" :dst #uuid "" :time "" :data {:name ""} :type :event}

(defn handle-open [])

(defn handle-close [])

(defmulti handle-message (fn [_ {:keys [type]}] type))

(defmethod handle-message :default [])

(defmethod handle-message :event [{:keys [dispatch]} {:keys [data]}]
  (case (:name data)
    :session/created (dispatch :session/created (:uuid data) (:room data))
    :session/joined  (dispatch :session/joined (:uuid data))
    :session/join    (dispatch :session/join (:uuid data))
    :session/leave   (dispatch :session/leave (:uuid data))
    nil))

(defmethod handle-message :datoms [body])

(defmethod handle-message :tx [body])

(defmethod handle-message :image [body])

(defn handlers []
  (let [[conn dispatch] (uix/context state)
        socket (uix/state nil)]
    
    (uix/effect!
     (fn []
       (let [search (.. js/window -location -search)
             params (js/URLSearchParams. search)
             room   (.get params "join")]
         (if (not (nil? room))
           (let [ws (js/WebSocket. (str "ws://localhost:5000/ws?key=" room))]
             (reset! socket ws))))) [])

    (uix/effect!
     (fn []
       (ds/listen!
        conn :session
        (fn [{[event _ _] :tx-meta}]
          (if (= event :session/request)
            (let [ws (js/WebSocket. "ws://localhost:5000/ws")]
              (reset! socket ws)))))
       (fn [] (ds/unlisten! conn :session))))

    (let [socket  @socket
          reader  (transit/reader :json)
          context {:conn conn :dispatch dispatch}]
      (doseq [event ["open" "close" "message"]]
        (listen!
         (fn [body]
           (case event
             "open"    (handle-open)
             "close"   (handle-close)
             "message" (let [parsed (transit/read reader (.-data body))]
                         (handle-message context parsed)))) socket event [socket]))) nil))
