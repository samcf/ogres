(ns ogre.tools.hooks
  (:require [ogre.tools.provider.events :as provider.events]
            [ogre.tools.provider.image :as provider.image]
            [ogre.tools.provider.portal :as provider.portal]
            [ogre.tools.provider.state :as provider.state]
            [ogre.tools.provider.storage :as provider.storage]
            [uix.core.alpha :as uix]))

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
       :arglists '([f] [f deps] [f topic deps] [f topic chan deps])}
  subscribe!
  provider.events/subscribe!)

(def ^{:doc "Creates one or more subscriptions for each given topic-fn pairs"
       :arglists '([& topic-fn-pairs])}
  subscribe-many!
  provider.events/subscribe-many!)

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
       :arglists '([render-fn label])}
  create-portal
  provider.portal/create)

(def ^{:doc "Renders the given children into the portal named by the value
             of :label in `props`. When the value given is nil, will
             render the given children normally."
       :arglists '([props children])}
  use-portal
  provider.portal/use)

(def ^{:doc "Returns an instance of a Dexie object. This object is used to
             query and write to the browser's IndexedDB. See
             https://dexie.org/docs/Dexie/Dexie for more information."
       :arglists '([])}
  use-store
  provider.storage/use-store)

(defn listen!
  "Creates a DOM event listener on the given DOM object `element` for `event`,
   calling `f` with the DOM event."
  ([f event]      (listen! f event []))
  ([f event deps] (listen! f js/window event deps))
  ([f element event deps]
   (uix/effect!
    (fn [] (if element (.addEventListener element event f #js {:passive false}))
      (fn [] (if element (.removeEventListener element event f #js {:passive false})))) deps)))

(defn use-interval
  "Calls the given function `f` with no arguments every `delay` milliseconds."
  [f delay]
  (uix/effect!
   (fn []
     (let [id (js/window.setInterval f delay)]
       (fn [] (js/window.clearInterval id))))))

(defn use-modal
  "Returns a tuple of [state ref], changing `state` to false when there is
   a click event that does not occur within the element identified by `ref`.
   Pass `ref` to the element (or modal) you want to close when the user
   clicks outside of it."
  []
  (let [ref (uix/ref) state (uix/state false)]
    (listen!
     (fn [event]
       (if (and @ref (not (.contains @ref (.-target event))))
         (swap! state not))) (if @state js/document false) "click" [@state])
    [state ref]))
