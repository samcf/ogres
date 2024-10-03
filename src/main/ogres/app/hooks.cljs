(ns ogres.app.hooks
  (:require [ogres.app.provider.events   :as provider.events]
            [ogres.app.provider.dispatch :as provider.dispatch]
            [ogres.app.provider.image    :as provider.image]
            [ogres.app.provider.portal   :as provider.portal]
            [ogres.app.provider.state    :as provider.state]
            [ogres.app.provider.storage  :as provider.storage]
            [uix.core                    :refer [use-effect use-state use-ref use-callback]]
            ["@rwh/keystrokes"           :refer [bindKey unbindKey]]))

(def ^{:doc "Returns a function which, when called with a topic and any number
             of additional arguments, will perform the following work:
             1. Publish a new event with the given topic and arguments.
             2. Create a DataScript transaction if the topic is registered
                for one.
             3. Publish a new event of that transaction with the transaction
                report as its only argument."
       :arglists '([])}
  use-dispatch
  provider.dispatch/use-dispatch)

(def ^{:doc "Returns the result of querying the underlying DataScript
             database. Queries must be a pull-style pattern. Optionally
             may be passed an entity ID or equivalent identifier,
             otherwise will use the local user entity by default."
       :arglists '([query] [query identifier])}
  use-query
  provider.state/use-query)

(def ^{:doc "Returns a function which, when called with a topic and
             any relevant arguments, publishes a new event on the
             bus. Events and their listeners are called synchronously
             and in the order they are added.
             ```
             (let [publish (use-publish)]
               (use-subscribe :foo/bar
                 (fn [a b c]
                   (prn a b c)))
               (publish :foo/bar 1 2 3))
             ```"
       :arglists '([])}
  use-publish
  provider.events/use-publish)

(def ^{:doc "Registers a new handler function to the given topic."
       :arglists '([topic f])}
  use-subscribe
  provider.events/use-subscribe)

(def ^{:doc "Returns a URL for the image identified by the given hash.
             The URL is only guaranteed to be usable for the window it
             was created for, and only guaranteed to last for the duration
             of the user session. Returns nil if the image is loading or
             cannot be found."
       :arglists '([hash])}
  use-image
  provider.image/use-image)

(def ^{:doc "Returns a function which accepts an HTML File object as its
             only argument and returns a Promise whose resolved value
             is a map with image properties."}
  use-image-uploader
  provider.image/use-image-uploader)

(def ^{:doc "Creates a new portal element of name `label` with contents
             rendered by `render-fn`. This portal can then be rendered
             into with `use-portal`."
       :arglists '([props])}
  create-portal
  provider.portal/create)

(def ^{:doc "Renders the given children into the portal named by the value
             of :label in `props`. When the value given is nil, will
             render the given children normally."
       :arglists '([props])}
  use-portal
  provider.portal/use)

(def ^{:doc "Returns an instance of a Dexie object. This object is used to
             query and write to the browser's IndexedDB. See
             https://dexie.org/docs/Dexie/Dexie for more information."
       :arglists '([])}
  use-store
  provider.storage/use-store)

(defn use-event-listener
  "Creates a DOM event listener on the given DOM object `element` for `event`,
   calling `f` with the DOM event."
  ([event f]
   (use-event-listener js/window event f))
  ([element event f]
   (use-effect
    (fn [] (if element (.addEventListener element event f #js {:passive false}))
      (fn [] (if element (.removeEventListener element event f #js {:passive false}))))
    [element event f])))

(defn use-interval
  "Calls the given function `f` with no arguments every `delay` milliseconds."
  [f delay]
  (use-effect
   (fn []
     (let [id (js/window.setInterval f delay)]
       (fn [] (js/window.clearInterval id)))) [delay f]))

(defn use-modal
  "Returns a tuple of [state set-state ref], changing `state` to false when
   there is a click event that does not occur within the element identified by
   `ref`. Pass `ref` to the element (or modal) you want to close when the user
   clicks outside of it."
  []
  (let [[state set-state] (use-state false) ref (use-ref)]
    (use-event-listener (if state js/document false) "click"
      (use-callback
       (fn [event]
         (if-let [node (deref ref)]
           (if (not (.contains node (.-target event)))
             (set-state false)))) []))
    [state set-state ref]))

(defn use-shortcut
  "Binds one or more keyboard codes to the callback function given by f which
   receives a custom event as its only argument."
  [keys f]
  (use-effect
   (fn [] (doseq [key keys] (bindKey key f))
     (fn [] (doseq [key keys] (unbindKey key f)))) [keys f]))
