(ns ogres.app.provider.events
  (:require [uix.core.alpha :as uix :refer [defcontext]]
            [clojure.core.async :refer [chan mult tap untap pub sub unsub go-loop timeout put! <! close! alts!]]))

(defn create-initial-value []
  (let [ch-src (chan 1)
        ch-pub (chan 1)
        multi  (mult ch-src)]
    (tap multi ch-pub)
    [(pub ch-pub :topic) ch-src multi]))

(defcontext context (create-initial-value))

(defonce context-value (create-initial-value))

(defn provider
  "Provides an event publication object, write channel, and channel multiplexer
   for the given children. Refer to `use-publish` and `subscribe!` to publish
   and subscribe to events, respectively."
  [children]
  (uix/context-provider [context context-value] children))

(defn use-publish
  "Returns a function that must be called with a topic as the first argument
   and the event arguments as the rest. Components may subscribe to published
   events with subscribe!"
  []
  (let [[_ ch] (uix/context context)]
    (fn [event]
      (put! ch event))))

(defn subscribe!
  "Subscribes the given handler to the event bus, optionally given a topic.
   May be passed a topic and channel for more nuanced control."
  ([f]
   (subscribe! f []))
  ([f deps]
   (let [[_ _ multi] (uix/context context)]
     (uix/effect!
      (fn []
        (let [ch (chan 1)]
          (tap multi ch)
          (go-loop []
            (if-some [event (<! ch)]
              (do (f event)
                  (recur))))
          (fn []
            (close! ch)
            (untap multi ch)))) deps)))
  ([f topic deps]
   (subscribe! f topic {:chan (chan 1)} deps))
  ([f topic opts deps]
   (let [{ch :chan} opts
         [pub _]    (uix/context context)]
     (uix/effect!
      (fn []
        (sub pub topic ch)
        (go-loop []
          (when-some [event (<! ch)]
            (if (> (:rate-limit opts) 0)
              (<! (timeout (:rate-limit opts))))
            (f event)
            (recur)))
        (fn []
          (unsub pub topic ch)
          (close! ch))) deps))))

(defn subscribe-many!
  "Subscribes to multiple topics as given by each topic-fn pair in the form
   of `topic handler-fn`."
  [& topic-fns]
  (let [[pub _ _]   (uix/context context)
        topic-fns   (into {} (partition-all 2) topic-fns)
        chans       (repeatedly (count topic-fns) #(chan 1))
        topic-chans (partition 2 (interleave (keys topic-fns) chans))]
    (uix/effect!
     (fn []
       (doseq [[topic ch] topic-chans]
         (sub pub topic ch))
       (go-loop []
         (let [[event _] (alts! chans)]
           (if (not (nil? event))
             (do ((topic-fns (:topic event)) event)
                 (recur)))))
       (fn []
         (doseq [[topic ch] topic-chans]
           (unsub pub topic ch)
           (close! ch)))) [topic-fns])))
