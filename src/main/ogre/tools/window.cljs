(ns ogre.tools.window
  (:require [cognitect.transit :as t]
            [datascript.core :as ds]
            [datascript.transit :as dst]
            [ogre.tools.events :refer [use-dispatch subscribe!]]
            [ogre.tools.render :refer [listen!]]
            [ogre.tools.state :as state :refer [use-query]]
            [ogre.tools.timing :refer [debounce]]
            [uix.core.alpha :as uix :refer [defcontext]]))

(defcontext context)

(def reader (t/reader :json {:handlers dst/read-handlers}))
(def writer (t/writer :json {:handlers dst/write-handlers}))

(defn bounds->vector
  [bounds]
  [(.-x bounds)
   (.-y bounds)
   (.-width bounds)
   (.-height bounds)])

(defn initialize
  "Registers a DataScript listener in order to manage the view window, the
   player's view of the canvas." []
  (let [dispatch             (use-dispatch)
        {:keys [view reset]} (uix/context context)]
    (subscribe!
     (fn []
       (if (nil? @view)
         (let [url (.. js/window -location -origin)
               url (str url "?share=true")
               win (.open js/window url "ogre.tools" "width=640,height=640")]
           (reset win)
           (dispatch :share/toggle true))
         (reset))) :share/initiate []) nil))

(defn dispatcher
  "Registers a DataScript listener in order to forward transactions from the
   host window to the view window." []
  (let [{:keys [view]} (uix/context context)]
    (subscribe!
     (fn [{[{tx-data :tx-data}] :args}]
       (->>
        #js {:detail (t/write writer tx-data)}
        (js/CustomEvent. "AppStateTx")
        (.dispatchEvent @view))) :tx/commit []) nil))

(defn listener
  "Registers an event handler to listen for application state changes in the
   form of serialized EDN DataScript transactions. Unmarshals and transacts
   those against the local DataScript connection." []
  (let [conn (uix/context state/context)]
    (listen!
     (fn [event]
       (->>
        (.-detail event)
        (t/read reader)
        (ds/transact! conn))) "AppStateTx" []) nil))

(defn bounds
  "Registers event handlers to watch for changes in the canvas dimensions in
   order to put those dimensions in the application state. Dimensions are
   of the form [x y width height]."
  [{:keys [target type]}]
  (let [dispatch (use-dispatch)
        canvas   (uix/state nil)
        handler  (debounce
                  (fn []
                    (if-let [element (.. target -document (querySelector ".layout-canvas"))]
                      (->> (.getBoundingClientRect element)
                           (bounds->vector)
                           (dispatch :bounds/change type)))) 100)
        observer (uix/state (js/ResizeObserver. handler))]

    (listen! handler "resize" [])

    (uix/effect!
     (fn []
       (if (= type :host)
         (.dispatchEvent target (js/Event. "resize")))
       ((fn f []
          (let [element (.querySelector (.-document target) ".layout-canvas")]
            (if element
              (reset! canvas element)
              (.requestAnimationFrame js/window f)))))) [])

    (uix/effect!
     (fn []
       (when-let [element @canvas]
         (.observe @observer element)
         (fn [] (.unobserve @observer element)))) [@canvas]) nil))

(defn closers
  "Registers event handlers to listen for the host or view windows being
   closed." []
  (let [{:keys [view reset]} (uix/context context)]
    (listen!
     (fn []
       (.setTimeout
        js/window
        (fn []
          (when (.-closed @view)
            (reset))) 200)) @view "visibilitychange" [@view])

    (listen!
     (fn []
       (reset)) "beforeunload" []) nil))

(defn provider
  "Provides a reference to the view window, if any, and registers several
   event handlers needed for them." []
  (let [dispatch (use-dispatch)
        result   (use-query [:local/type :local/loaded?])
        -window  (uix/state nil)

        on-set-window
        (fn
          ([]
           (when-let [element @-window]
             (.close element)
             (dispatch :share/toggle false)
             (reset! -window nil)))
          ([element]
           (reset! -window element)))]
    (if (:local/loaded? result)
      (uix/context-provider
       [context {:view -window :reset on-set-window}]
       (case (:local/type result)
         :host [:<>
                [initialize]
                [bounds {:target js/window :type :host}]
                (if-let [target @-window]
                  [:<>
                   [dispatcher]
                   [bounds {:target target :type :view}]])
                [closers]]
         :view [listener]
         :conn [bounds {:target js/window :type :conn}])))))
