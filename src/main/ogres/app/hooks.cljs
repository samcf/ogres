(ns ogres.app.hooks
  (:require [ogres.app.provider.events :as provider.events]
            [ogres.app.provider.image :as provider.image]
            [ogres.app.provider.portal :as provider.portal]
            [ogres.app.provider.state :as provider.state]
            [ogres.app.provider.storage :as provider.storage]
            [perfect-cursors :refer [PerfectCursor]]
            [uix.core :refer [use-effect use-state use-layout-effect use-ref use-callback]]))

(def ^{:doc "Returns a function which, when called with a topic and any number
             of additional arguments, will perform the following work:
             1. Publish a new event with the given topic and arguments.
             2. Create a DataScript transaction if the topic is registered
                for one.
             3. Publish a new event of that transaction with the transaction
                report as its only argument."
       :arglists '([])}
  use-dispatch
  provider.state/use-dispatch)

(def ^{:doc "Returns the result of querying the underlying DataScript
             database. Queries must be a pull-style pattern. Optionally
             may be passed an entity ID or equivalent identifier,
             otherwise will use the local user entity by default."
       :arglists '([query] [query identifier])}
  use-query
  provider.state/use-query)

(def ^{:doc "Returns a function which publishes the given event map
             onto the global event publication."
       :arglists '([])}
  use-publish
  provider.events/use-publish)

(def ^{:doc "Creates a new subscription to `topic`, calling the given
             `handler-fn` with the event map when an event with that
             topic is published."
       :arglists '([f] [f topic] [f topic chan])}
  use-subscribe
  provider.events/use-subscribe)

(def ^{:doc "Returns a URL for the image identified by the given checksum.
             The URL is only guaranteed to be usable for the window it
             was created for, and only guaranteed to last for the duration
             of the user session. Returns nil if the image is loading or
             cannot be found."
       :arglists '([checksum])}
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

(defn use-listen
  "Creates a DOM event listener on the given DOM object `element` for `event`,
   calling `f` with the DOM event."
  ([f event]
   (use-listen f js/window event))
  ([f element event]
   (use-effect
    (fn [] (if element (.addEventListener element event f #js {:passive false}))
      (fn [] (if element (.removeEventListener element event f #js {:passive false}))))
    [f element event])))

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
    (use-listen
     (use-callback
      (fn [event]
        (if-let [node (deref ref)]
          (if (not (.contains node (.-target event)))
            (set-state false)))) [])
     (if state js/document false) "click")
    [state set-state ref]))

(defn use-cursor
  "Returns a function which should be called with updates to an element's
   position from some external source, such as a WebSocket event. Accepts a
   callback function which is called with an interpolated position; use this
   new position to update the element's actual rendered position."
  [callback [x y]]
  (let [[cursor] (use-state (PerfectCursor. callback))]
    (use-layout-effect
     (fn []
       (.addPoint cursor #js [x y])
       (fn [] (.dispose cursor))) ^:lint/disable [cursor])
    (use-callback
     (fn [[x y]]
       (.addPoint cursor #js [x y])) [cursor])))
