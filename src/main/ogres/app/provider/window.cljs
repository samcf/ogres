(ns ogres.app.provider.window
  (:require [cognitect.transit :as t]
            [datascript.core :as ds]
            [datascript.transit :as dst]
            [ogres.app.hooks :refer [use-event-listener use-subscribe use-dispatch use-query]]
            [ogres.app.provider.state :as provider.state]
            [ogres.app.util :refer [debounce]]
            [uix.core :refer [defui $ create-context use-callback use-context use-state use-effect]]))

(def ^:private context (create-context))

(def ^:private reader (t/reader :json {:handlers dst/read-handlers}))
(def ^:private writer (t/writer :json {:handlers dst/write-handlers}))

(defn ^:private bounds->vector
  [bounds]
  [(.-x bounds)
   (.-y bounds)
   (.-width bounds)
   (.-height bounds)])

(defui ^:private initialize
  "Registers a DataScript listener in order to manage the view window, the
   player's view of the scene." []
  (let [dispatch             (use-dispatch)
        {:keys [view reset]} (use-context context)]
    (use-subscribe :share/initiate
      (fn []
        (if (nil? view)
          (let [url (.. js/window -location -origin)
                url (str url "?share=true")
                win (.open js/window url "ogres.app" "width=640,height=640")]
            (reset win)
            (dispatch :share/toggle true))
          (reset))))))

(defui ^:private dispatcher
  "Registers a DataScript listener in order to forward transactions from the
   host window to the view window." []
  (let [{:keys [view]} (use-context context)]
    (use-subscribe :tx/commit
      (fn [{[{tx-data :tx-data}] :args}]
        (->>
         #js {:detail (t/write writer tx-data)}
         (js/CustomEvent. "AppStateTx")
         (.dispatchEvent view))))))

(defui ^:private listener
  "Registers an event handler to listen for application state changes in the
   form of serialized EDN DataScript transactions. Unmarshals and transacts
   those against the local DataScript connection." []
  (let [conn (use-context provider.state/context)]
    (use-event-listener "AppStateTx"
      (use-callback
       (fn [event]
         (->>
          (.-detail event)
          (t/read reader)
          (ds/transact! conn))) [conn]))))

(defui ^:private bounds
  "Registers event handlers to watch for changes in the scene dimensions in
   order to put those dimensions in the application state. Dimensions are
   of the form [x y width height]."
  [{:keys [target type]}]
  (let [dispatch (use-dispatch)
        handler  (debounce
                  (fn []
                    (if-let [element (.. target -document (querySelector ".layout-scene"))]
                      (->> (.getBoundingClientRect element)
                           (bounds->vector)
                           (dispatch :bounds/change type)))) 100)
        [observer] (use-state (js/ResizeObserver. handler))
        [scene set-scene] (use-state nil)]
    (use-event-listener "resize" handler)
    (use-effect
     (fn []
       (if (= type :host)
         (.dispatchEvent target (js/Event. "resize")))
       ((fn f []
          (let [element (.querySelector (.-document target) ".layout-scene")]
            (if element
              (set-scene element)
              (.requestAnimationFrame js/window f)))))))

    (use-effect
     (fn []
       (when scene
         (.observe observer scene)
         (fn [] (.unobserve observer scene)))) ^:lint/disable [scene])))

(defui ^:private closers
  "Registers event handlers to listen for the host or view windows being
   closed."
  []
  (let [{:keys [view reset]} (use-context context)]
    (use-event-listener view "visibilitychange"
      (use-callback
       (fn []
         (.. js/window (setTimeout #(when (.-closed view) (reset)) 200)))
       [view reset]))
    (use-event-listener "beforeunload"
      (fn [] (reset)))))

(defui provider
  "Provides a reference to the view window, if any, and registers several
   event handlers needed for them."
  []
  (let [[screen set-screen] (use-state nil)
        dispatch (use-dispatch)
        result   (use-query [:local/type :local/loaded?])
        on-reset (use-callback
                  (fn
                    ([] (when-let [element screen]
                          (.close element)
                          (dispatch :share/toggle false)
                          (set-screen nil)))
                    ([element] (set-screen element))) [dispatch screen])]
    (if (:local/loaded? result)
      ($ (.-Provider context) {:value {:view screen :reset on-reset}}
        (case (:local/type result)
          :host ($ :<>
                  ($ initialize)
                  ($ bounds {:target js/window :type :host})
                  (if-let [target screen]
                    ($ :<>
                      ($ dispatcher)
                      ($ bounds {:target target :type :view})))
                  ($ closers))
          :view ($ listener)
          :conn ($ bounds {:target js/window :type :conn}))))))
