(ns ogres.app.provider.window
  (:require [cognitect.transit :as t]
            [datascript.core :as ds]
            [datascript.transit :as dst]
            [goog.functions :refer [throttle]]
            [ogres.app.hooks :refer [use-event-listener use-subscribe use-dispatch use-query]]
            [ogres.app.provider.state :as provider.state]
            [uix.core :as uix :refer [defui $]]))

(def ^:private context (uix/create-context))

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
        {:keys [view reset]} (uix/use-context context)]
    (use-subscribe :share/initiate
      (fn []
        (if (nil? view)
          (let [params (js/URLSearchParams. #js {"share" true})
                origin (.. js/window -location -origin)
                path   (.. js/window -location -pathname)
                url (str origin path "?" (.toString params))
                win (.open js/window url "ogres.app" "width=640,height=640")]
            (reset win)
            (dispatch :share/toggle true))
          (reset))))))

(defui ^:private dispatcher
  "Registers a DataScript listener in order to forward transactions from the
   host window to the view window." []
  (let [{:keys [view]} (uix/use-context context)]
    (use-subscribe :tx/commit
      (fn [{tx-data :tx-data}]
        (->>
         #js {:detail (t/write writer tx-data)}
         (js/CustomEvent. "AppStateTx")
         (.dispatchEvent view))))))

(defui ^:private listener
  "Registers an event handler to listen for application state changes in the
   form of serialized EDN DataScript transactions. Unmarshals and transacts
   those against the local DataScript connection." []
  (let [conn (uix/use-context provider.state/context)]
    (use-event-listener "AppStateTx"
      (uix/use-callback
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
        handler  (uix/use-memo
                  #(throttle
                    (fn []
                      (if-let [element (.. target -document (querySelector ".layout-scene"))]
                        (->> (.getBoundingClientRect element)
                             (bounds->vector)
                             (dispatch :bounds/change type)))) 100) [dispatch type target])
        [observer] (uix/use-state (js/ResizeObserver. handler))
        [scene set-scene] (uix/use-state nil)]
    (use-event-listener "resize" handler)
    (uix/use-effect
     (fn []
       (if (= type :host)
         (.dispatchEvent target (js/Event. "resize")))
       ((fn f []
          (let [element (.querySelector (.-document target) ".layout-scene")]
            (if element
              (set-scene element)
              (.requestAnimationFrame js/window f)))))))

    (uix/use-effect
     (fn []
       (when scene
         (.observe observer scene)
         (fn [] (.unobserve observer scene)))) ^:lint/disable [scene])))

(defui ^:private closers
  "Registers event handlers to listen for the host or view windows being
   closed."
  []
  (let [{:keys [view reset]} (uix/use-context context)]
    (use-event-listener view "visibilitychange"
      (uix/use-callback
       (fn []
         (.. js/window (setTimeout #(when (.-closed view) (reset)) 200)))
       [view reset]))
    (use-event-listener "beforeunload"
      (fn [] (reset)))))

(defui provider
  "Provides a reference to the view window, if any, and registers several
   event handlers needed for them."
  []
  (let [[screen set-screen] (uix/use-state nil)
        dispatch (use-dispatch)
        result   (use-query [:user/type :user/ready])
        on-reset (uix/use-callback
                  (fn
                    ([] (when-let [element screen]
                          (.close element)
                          (dispatch :share/toggle false)
                          (set-screen nil)))
                    ([element] (set-screen element))) [dispatch screen])]
    (if (:user/ready result)
      ($ context {:value {:view screen :reset on-reset}}
        (case (:user/type result)
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
