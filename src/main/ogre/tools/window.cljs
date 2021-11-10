(ns ogre.tools.window
  (:require [goog.async.Debouncer]
            [cognitect.transit :as t]
            [datascript.core :as ds]
            [uix.core.alpha :as uix :refer [defcontext]]
            [ogre.tools.render :refer [listen!]]
            [ogre.tools.state :refer [state use-query]]))

(defcontext context)

(defn bounds->vector
  [bounds]
  [(.-x bounds)
   (.-y bounds)
   (.-width bounds)
   (.-height bounds)])

(defn debounce [f interval]
  (let [d (goog.async.Debouncer. f interval)]
    (fn [& args]
      (.apply (.-fire d) d (to-array args)))))

(defn initialize
  "Registers a DataScript listener in order to manage the guest window, the
   player's view of the canvas." []
  (let [[conn dispatch]       (uix/context state)
        {:keys [guest reset]} (uix/context context)]
    (uix/effect!
     (fn []
       (ds/listen!
        conn :initialize
        (fn [{[event _ _] :tx-meta}]
          (when (= event :share/initiate)
            (if (nil? guest)
              (let [url (.. js/window -location -origin)
                    url (str url "?share=true")
                    win (.open js/window url "ogre.tools" "width=640,height=640")]
                (reset win)
                (dispatch :share/toggle true))
              (reset)))))
       (fn [] (ds/unlisten! conn :initialize))) [guest]) nil))

(defn dispatcher
  "Registers a DataScript listener in order to forward transactions from the
   host window to the guest window." []
  (let [[conn]          (uix/context state)
        {:keys [guest]} (uix/context context)
        writer          (t/writer :json)]
    (uix/effect!
     (fn []
       (ds/listen!
        conn :dispatcher
        (fn [{[_ _ tx] :tx-meta}]
          (when guest
            (->>
             #js {:detail (t/write writer tx)}
             (js/CustomEvent. "AppStateTx")
             (.dispatchEvent guest)))))
       (fn [] (ds/unlisten! conn :dispatcher))) [guest]) nil))

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
                    (->>
                     (.querySelector (.-document target) selector)
                     (.getBoundingClientRect)
                     (bounds->vector)
                     (dispatch :bounds/change host?))) 100)
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
         (do
           (.observe @observer element)
           (fn [] (.unobserve @observer element))))) [@canvas]) nil))

(defn closers
  "Registers event handlers to listen for the host or guest windows being
   closed." []
  (let [{:keys [guest reset]} (uix/context context)]
    (listen!
     (fn []
       (.setTimeout
        js/window
        (fn []
          (when (.-closed guest)
            (reset))) 200)) guest "visibilitychange" [guest])

    (listen!
     (fn []
       (reset)) "beforeunload" []) nil))

(def query
  {:pull [:root/host? :root/loaded?]})

(defn provider
  "Provides a reference to the guest window, if any, and registers several
   event handlers needed for them." []
  (let [[data dispatch] (use-query query)
        guest           (uix/state nil)
        reset (fn
                ([]
                 (when-let [element @guest]
                   (.close element)
                   (dispatch :share/toggle false)
                   (reset! guest nil)))
                ([element]
                 (reset! guest element)))]

    (if (:root/loaded? data)
      (uix/context-provider
       [context {:guest @guest :reset reset}]
       (if (:root/host? data)
         [:<>
          [initialize]
          [dispatcher]
          [bounds {:target js/window :host? true}]
          (when-let [element @guest]
            [bounds {:target element :host? false}])
          [closers]]
         [listener])))))
