(ns ogre.tools.query
  (:require [datascript.core :as ds]))

(defn viewer [data]
  (ds/entity data (:e (first (ds/datoms data :aevt :viewer/workspace)))))

(defn workspaces [data]
  (->> (ds/q '[:find ?ws ?tx :where [_ :viewer/workspaces ?ws ?tx]] data)
       (sort-by second)
       (map first)
       (map #(ds/entity data %))))

(defn boards [data]
  (->> (ds/q '[:find ?id ?tx :where [?id :map/id _ ?tx]] data)
       (sort-by second)
       (map first)
       (map #(ds/entity data %))))
