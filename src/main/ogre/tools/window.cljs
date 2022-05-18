(ns ogre.tools.window
  (:require [cognitect.transit :as t]
            [datascript.core :as ds]
            [ogre.tools.render :refer [listen!]]
            [ogre.tools.state :refer [state use-query]]
            [ogre.tools.timing :refer [debounce]]
            [uix.core.alpha :as uix :refer [defcontext]]))

(defcontext context)

(defn bounds->vector
  [bounds]
  [(.-x bounds)
   (.-y bounds)
   (.-width bounds)
   (.-height bounds)])

(defn initialize
  "Registers a DataScript listener in order to manage the view window, the
   player's view of the canvas." []
  (let [[conn dispatch]       (uix/context state)
        {:keys [view reset]}  (uix/context context)]
    (uix/effect!
     (fn []
       (ds/listen!
        conn :initialize
        (fn [{[event _ _] :tx-meta}]
          (if (= event :share/initiate)
            (if (nil? view)
              (let [url (.. js/window -location -origin)
                    url (str url "?share=true")
                    win (.open js/window url "ogre.tools" "width=640,height=640")]
                (reset win)
                (dispatch :share/toggle true))
              (reset)))))
       (fn [] (ds/unlisten! conn :initialize))) [view]) nil))

(defn dispatcher
  "Registers a DataScript listener in order to forward transactions from the
   host window to the view window." []
  (let [[conn]         (uix/context state)
        {:keys [view]} (uix/context context)
        writer         (t/writer :json)]
    (uix/effect!
     (fn []
       (ds/listen!
        conn :dispatcher
        (fn [{[_ _ tx] :tx-meta}]
          (if view
            (->>
             #js {:detail (t/write writer tx)}
             (js/CustomEvent. "AppStateTx")
             (.dispatchEvent view)))))
       (fn [] (ds/unlisten! conn :dispatcher))) [view]) nil))

(defn listener
  "Registers an event handler to listen for application state changes in the
   form of serialized EDN DataScript transactions. Unmarshals and transacts
   those against the local DataScript connection." []
  (let [[conn] (uix/context state)
        reader (t/reader :json)]
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
  [{:keys [target host?]}]
  (let [[_ dispatch] (uix/context state)
        selector ".layout-canvas"
        canvas   (uix/state nil)
        handler  (debounce
                  (fn []
                    (if-let [element (.. target -document (querySelector selector))]
                      (->> (.getBoundingClientRect element)
                           (bounds->vector)
                           (dispatch :bounds/change host?)))) 100)
        observer (uix/state (js/ResizeObserver. handler))]

    (listen! handler "resize" [])

    (uix/effect!
     (fn []
       (when host?
         (.dispatchEvent target (js/Event. "resize")))) [])

    (uix/effect!
     (fn []
       ((fn f []
          (let [element (.querySelector (.-document target) selector)]
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
          (when (.-closed view)
            (reset))) 200)) view "visibilitychange" [view])

    (listen!
     (fn []
       (reset)) "beforeunload" []) nil))

(defn provider
  "Provides a reference to the view window, if any, and registers several
   event handlers needed for them." []
  (let [[result dispatch] (use-query [[:local/type :default :conn] [:local/loaded? :default false]])
        view              (uix/state nil)
        reset             (fn
                            ([]
                             (when-let [element @view]
                               (.close element)
                               (dispatch :share/toggle false)
                               (reset! view nil)))
                            ([element]
                             (reset! view element)))]
    (if (:local/loaded? result)
      (uix/context-provider
       [context {:view @view :reset reset}]
       (case (:local/type result)
         :host [:<>
                [initialize]
                [dispatcher]
                [bounds {:target js/window :host? true}]
                (if-let [element @view]
                  [bounds {:target element :host? false}])
                [closers]]
         :view [listener]
         :conn [:<>])))))
