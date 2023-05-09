(ns ogres.app.provider.window
  (:require [cognitect.transit :as t]
            [datascript.core :as ds]
            [datascript.transit :as dst]
            [ogres.app.hooks :refer [listen! subscribe! use-dispatch use-query]]
            [ogres.app.provider.state :as provider.state]
            [ogres.app.timing :refer [debounce]]
            [uix.core :refer [defui $ create-context use-context use-state use-effect]]))

(def context (create-context))

(def reader (t/reader :json {:handlers dst/read-handlers}))
(def writer (t/writer :json {:handlers dst/write-handlers}))

(defn bounds->vector
  [bounds]
  [(.-x bounds)
   (.-y bounds)
   (.-width bounds)
   (.-height bounds)])

(defui initialize
  "Registers a DataScript listener in order to manage the view window, the
   player's view of the canvas." []
  (let [dispatch             (use-dispatch)
        {:keys [view reset]} (use-context context)]
    (subscribe!
     (fn []
       (if (nil? view)
         (let [url (.. js/window -location -origin)
               url (str url "?share=true")
               win (.open js/window url "ogres.app" "width=640,height=640")]
           (reset win)
           (dispatch :share/toggle true))
         (reset))) :share/initiate)))

(defui dispatcher
  "Registers a DataScript listener in order to forward transactions from the
   host window to the view window." []
  (let [{:keys [view]} (use-context context)]
    (subscribe!
     (fn [{[{tx-data :tx-data}] :args}]
       (->>
        #js {:detail (t/write writer tx-data)}
        (js/CustomEvent. "AppStateTx")
        (.dispatchEvent view))) :tx/commit)))

(defui listener
  "Registers an event handler to listen for application state changes in the
   form of serialized EDN DataScript transactions. Unmarshals and transacts
   those against the local DataScript connection." []
  (let [conn (use-context provider.state/context)]
    (listen!
     (fn [event]
       (->>
        (.-detail event)
        (t/read reader)
        (ds/transact! conn))) "AppStateTx")))

(defui bounds
  "Registers event handlers to watch for changes in the canvas dimensions in
   order to put those dimensions in the application state. Dimensions are
   of the form [x y width height]."
  [{:keys [target type]}]
  (let [dispatch (use-dispatch)
        handler  (debounce
                  (fn []
                    (if-let [element (.. target -document (querySelector ".layout-canvas"))]
                      (->> (.getBoundingClientRect element)
                           (bounds->vector)
                           (dispatch :bounds/change type)))) 100)
        [observer] (use-state (js/ResizeObserver. handler))
        [canvas set-canvas] (use-state nil)]
    (listen! handler "resize")
    (use-effect
     (fn []
       (if (= type :host)
         (.dispatchEvent target (js/Event. "resize")))
       ((fn f []
          (let [element (.querySelector (.-document target) ".layout-canvas")]
            (if element
              (set-canvas element)
              (.requestAnimationFrame js/window f)))))))

    (use-effect
     (fn []
       (when canvas
         (.observe observer canvas)
         (fn [] (.unobserve observer canvas)))) ^:lint/disable [canvas])))

(defui closers
  "Registers event handlers to listen for the host or view windows being
   closed."
  []
  (let [{:keys [view reset]} (use-context context)]
    (listen!
     (fn []
       (.setTimeout
        js/window
        (fn []
          (when (.-closed view)
            (reset))) 200)) view "visibilitychange")

    (listen!
     (fn []
       (reset)) "beforeunload")))

(defui provider
  "Provides a reference to the view window, if any, and registers several
   event handlers needed for them."
  []
  (let [dispatch (use-dispatch)
        result   (use-query [:local/type :local/loaded?])
        [-window set-window] (use-state nil)
        on-set-window
        (fn
          ([]
           (when-let [element -window]
             (.close element)
             (dispatch :share/toggle false)
             (set-window nil)))
          ([element]
           (set-window element)))]
    (if (:local/loaded? result)
      ($ (.-Provider context) {:value {:view -window :reset on-set-window}}
         (case (:local/type result)
           :host ($ :<>
                    ($ initialize)
                    ($ bounds {:target js/window :type :host})
                    (if-let [target -window]
                      ($ :<>
                         ($ dispatcher)
                         ($ bounds {:target target :type :view})))
                    ($ closers))
           :view ($ listener)
           :conn ($ bounds {:target js/window :type :conn}))))))
