(ns ogre.tools.render
  (:require [datascript.core :as ds]
            [dexie :as dexie]
            [rum.core :as rum]
            [spade.core :refer [defclass]]))

(rum/defcontext context)

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

(rum/defc root [props children]
  (let [{:keys [data transact]} props]
    (let [[state update!] (rum/use-state data)
          store           (new dexie "ogre.tools")
          handler         (fn [event & args]
                            (update! (fn [current]
                                       (ds/db-with current (apply transact state event args)))))]
      (-> (.version store 1)
          (.stores #js {:images "checksum"}))

      (rum/bind-context
       [context {:data state :dispatch handler :store store}] children))))
