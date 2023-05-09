(ns ogres.app.provider.events
  (:require [clojure.core.async :refer [chan mult tap untap pub sub unsub go-loop timeout put! <! close!]]
            [uix.core :refer [defui $ create-context use-callback use-context use-effect]]))

(defn create-initial-value []
  (let [ch-src (chan 1)
        ch-pub (chan 1)
        multi  (mult ch-src)]
    (tap multi ch-pub)
    [(pub ch-pub :topic) ch-src multi]))

(def context (create-context (create-initial-value)))

(defonce context-value (create-initial-value))

(defui provider
  "Provides an event publication object, write channel, and channel multiplexer
   for the given children. Refer to `use-publish` and `subscribe!` to publish
   and subscribe to events, respectively."
  [{:keys [children]}]
  ($ (.-Provider context) {:value context-value} children))

(defui use-publish
  "Returns a function that must be called with a topic as the first argument
   and the event arguments as the rest. Components may subscribe to published
   events with subscribe!"
  []
  (let [[_ ch] (use-context context)]
    (use-callback
     (fn [event]
       (put! ch event)) ^:lint/disable [])))

(defn subscribe!
  "Subscribes the given handler to the event bus, optionally given a topic.
   May be passed a topic and channel for more nuanced control."
  ([f]
   (let [[_ _ multi] (use-context context)]
     (use-effect
      (fn []
        (let [ch (chan 1)]
          (tap multi ch)
          (go-loop []
            (if-some [event (<! ch)]
              (do (f event)
                  (recur))))
          (fn []
            (close! ch)
            (untap multi ch)))) [multi f pub])))
  ([f topic]
   (subscribe! f topic {:chan (chan 1)}))
  ([f topic opts]
   (let [{ch :chan} opts
         [pub _]    (use-context context)]
     (use-effect
      (fn []
        (sub pub topic ch)
        (go-loop []
          (when-some [event (<! ch)]
            (if (> (:rate-limit opts) 0)
              (<! (timeout (:rate-limit opts))))
            (f event)
            (recur)))
        (fn []
          (unsub pub topic ch))) ^:lint/disable [f]))))
