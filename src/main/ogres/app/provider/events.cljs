(ns ogres.app.provider.events
  (:require [clojure.core.async :refer [chan mult tap untap pub sub unsub go-loop timeout put! <! close!]]
            [uix.core :refer [defui $ create-context use-callback use-context use-effect]]))

(defn ^:private create-initial-value []
  (let [ch-src (chan 1)
        ch-pub (chan 1)
        multi  (mult ch-src)]
    (tap multi ch-pub)
    [(pub ch-pub :topic) ch-src multi]))

(def ^:private context
  (create-context (create-initial-value)))

(defonce context-value (create-initial-value))

(defui provider
  "Provides an event publication object, write channel, and channel multiplexer
   for the given children. Refer to `use-publish` and `use-subscribe` to publish
   and subscribe to events, respectively."
  [{:keys [children]}]
  ($ (.-Provider context) {:value context-value} children))

(defui use-publish
  "Returns a function that must be called with a topic as the first argument
   and the event arguments as the rest. Components may subscribe to published
   events with `use-subscribe`."
  []
  (let [[_ ch] (use-context context)]
    (use-callback
     (fn [event]
       (put! ch event)) ^:lint/disable [])))

(defn use-subscribe
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
  ([topic f]
   (use-subscribe topic {:chan (chan 1)} f))
  ([topic opts f]
   (let [disabled (:disabled opts)
         rate     (:rate-limit opts)
         [pub _]  (use-context context)
         dst      (or (:chan opts) (chan 1))]
     (use-effect
      (fn []
        (when (not disabled)
          (sub pub topic dst)
          (go-loop []
            (when-some [event (<! dst)]
              (if (> rate 0)
                (<! (timeout rate)))
              (f event)
              (recur))))
        (fn []
          (unsub pub topic dst))) ^:lint/disable [f rate disabled]))))
