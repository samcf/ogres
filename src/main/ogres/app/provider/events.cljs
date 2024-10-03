(ns ogres.app.provider.events
  (:require [uix.core :refer [$ defui create-context use-callback use-context use-effect]]))

(def ^:private context (create-context (js/EventTarget.)))

(defn use-subscribe [topic f]
  (let [target (use-context context)
        topic  (str topic)]
    (use-effect
     (fn []
       (let [handler (fn [event] (apply f (.-detail event)))]
         (.addEventListener target topic handler)
         (fn []
           (.removeEventListener target topic handler)))) [target topic f])))

(defn use-publish []
  (let [target (use-context context)]
    (use-callback
     (fn [topic & args]
       (.dispatchEvent target (js/CustomEvent. (str topic) #js {"detail" args}))) [target])))

(defui provider [props]
  ($ context {:value (js/EventTarget.)}
    (:children props)))
