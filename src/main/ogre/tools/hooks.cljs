(ns ogre.tools.hooks
  (:require [ogre.tools.provider.events :as provider.events]
            [ogre.tools.provider.state :as provider.state]))

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
