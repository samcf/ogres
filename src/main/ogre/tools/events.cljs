(ns ogre.tools.events
  (:require [uix.core.alpha :as uix :refer [defcontext]]
            [clojure.core.async :refer [chan mult tap untap pub sub unsub go-loop put! <! close!]]))

(defcontext context)

(defn provider
  "Provides an event publication object, write channel, and channel multiplexer
   for the given children. Refer to `use-dispatch` and `subscribe!` to publish
   and subscribe to events, respectively."
  [children]
  (let [ch-src (chan 1)
        ch-pub (chan 1)
        multi  (mult ch-src)]
    (tap multi ch-pub)
    (uix/context-provider [context [(pub ch-pub :topic) ch-src multi]] children)))

(defn use-dispatch
  "Returns a function that must be called with a topic as the first argument
   and the event arguments as the rest. Components may subscribe to published
   events with subscribe!"
  []
  (let [[_ ch] (uix/context context)]
    (fn [topic & args]
      (let [event {:topic topic :args args}]
        (put! ch event)))))

(defn subscribe!
  "Subscribes the given handler to the event bus, optionally given a topic.
   May be passed a topic and channel for more nuanced control."
  ([f]
   (let [[_ _ multi] (uix/context context)
         ch          (chan 1)]
     (tap multi ch)
     (uix/effect!
      (fn []
        (go-loop []
          (when-some [event (<! ch)]
            (f event)
            (recur)))
        (fn []
          (close! ch)
          (untap multi ch))))))
  ([f topic]
   (subscribe! f topic (chan 1)))
  ([f topic ch]
   (let [[pub _] (uix/context context)]
     (uix/effect!
      (fn []
        (sub pub topic ch)
        (go-loop []
          (when-some [event (<! ch)]
            (f event)
            (recur)))
        (fn []
          (close! ch)
          (unsub pub topic ch))) []))))
