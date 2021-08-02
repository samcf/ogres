(ns ogre.tools.render
  (:require [uix.core.alpha :as uix :refer [defcontext]]
            [datascript.core :as ds]
            [react :as react]
            [spade.core :refer [defclass]]
            [ogre.tools.txs :refer [transact]]
            [ogre.tools.query :as query]))

(defcontext context)

(defn css
  [& class-names]
  (->> (reduce (fn [names value]
                 (cond
                   (string?  value) (conj names (clojure.string/trim value))
                   (keyword? value) (conj names value)
                   (number?  value) (conj names (str value))
                   (vector?  value) (vec (concat names value))
                   (map?     value) (->> (reduce
                                          (fn [names [k v]]
                                            (if v (conj names k) names)) [] value)
                                         (concat names)
                                         vec)
                   :else            names)) [] class-names)
       (mapv name)
       (clojure.string/join " ")))

(defn use-image [board]
  #_(let [[url set-url!] (rum/use-state (:map/url board))
          context        (.useContext js/React context)]
      (if (string? url)
        url
        (-> (.get (.-images (:store context)) (:map/id board))
            (.then
             (fn [record]
               (set-url! (.-data record)))))))
  "foo")

(defn root [props child]
  (let [data  (uix/state (:data props))
        value {:data      @data
               :store     (:store props)
               :workspace (query/workspace @data)
               :dispatch  (fn [event & args]
                            (let [tx (apply transact @data event args)]
                              (reset! data (ds/db-with @data tx))))}]
    (uix/context-provider [context value] child)))
