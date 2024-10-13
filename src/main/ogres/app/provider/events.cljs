(ns ogres.app.provider.events
  (:require [uix.core :as uix :refer [defui $]]))

(def ^:private context (uix/create-context (js/EventTarget.)))

(defn use-subscribe [topic f]
  (let [target (uix/use-context context)
        topic  (str topic)]
    (uix/use-effect
     (fn []
       (let [handler (fn [event] (apply f (.-detail event)))]
         (.addEventListener target topic handler)
         (fn []
           (.removeEventListener target topic handler)))) [target topic f])))

(defn use-publish []
  (let [target (uix/use-context context)]
    (uix/use-callback
     (fn [topic & args]
       (.dispatchEvent target (js/CustomEvent. (str topic) #js {"detail" args}))) [target])))

(defui provider [props]
  (let [[target] (uix/use-state (js/EventTarget.))]
    ($ context {:value target}
      (:children props))))
