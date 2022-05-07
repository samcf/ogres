(ns ogre.tools.session 
  (:require [datascript.core :as ds]
            [ogre.tools.render :refer [listen!]]
            [ogre.tools.state :refer [state]]
            [uix.core.alpha :as uix]
            [cognitect.transit :as transit]))

(defn handlers []
  (let [[conn] (uix/context state)
        socket (uix/state nil)]

    (comment
      "Automatically connect to the multiplayer session when the appropriate key
       is present in the URL query parameters.")
    (uix/effect!
     (fn []
       (let [search (.. js/window -location -search)
             params (js/URLSearchParams. search)
             room   (.get params "key")]
         (if (not (nil? room))
           (let [ws (js/WebSocket. (str "ws://localhost:5000/ws?key=" room))]
             (reset! socket ws))))) [])

    (comment
      "Create a new multiplayer lobby from the current canvas when the appropriate
       DataScript transaction is received.")
    (uix/effect!
     (fn []
       (ds/listen!
        conn :lobby
        (fn [{[event _ _] :tx-meta}]
          (if (= event :lobby/create)
            (let [ws (js/WebSocket. "ws://localhost:5000/ws")]
              (reset! socket ws)))))
       (fn [] (ds/unlisten! conn :lobby))))

    (comment
      "Attach listeners for open, message, error, and close events to the WebSocket
       object.")
    (let [socket @socket]
      (listen!
       (fn [body]
         (let [r (transit/reader :json)]
           (println (transit/read r (.-data body))))) socket "message" [socket])
      (listen!
       (fn [body]
         (js/console.log body)) socket "open" [socket])
      (listen!
       (fn [body]
         (js/console.log body)) socket "close" [socket])) nil))
