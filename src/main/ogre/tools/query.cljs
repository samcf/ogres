(ns ogre.tools.query
  (:require [datascript.core :as ds]))

(defn workspace [data]
  (:viewer/workspace (ds/entity data [:db/ident :viewer])))

(defn workspaces [data]
  (->> (ds/q '[:find ?id ?tx :where [?id :element/type :workspace ?tx]] data)
       (sort-by second)
       (map first)
       (map #(ds/entity data %))))

(defn boards [data]
  (->> (ds/q '[:find ?id ?tx :where [?id :map/id _ ?tx]] data)
       (sort-by second)
       (map first)
       (map #(ds/entity data %))))
