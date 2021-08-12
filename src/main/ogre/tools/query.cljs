(ns ogre.tools.query
  (:require [datascript.core :as ds]))

(defn workspace [data]
  (:viewer/workspace (ds/entity data [:db/ident :viewer])))

(defn workspaces [data]
  (->> (ds/q '[:find ?id ?tx :where [?id :element/type :canvas ?tx]] data)
       (sort-by second)
       (map first)
       (map #(ds/entity data %))))

(defn boards [data]
  (->> (ds/q '[:find ?id ?tx :where [?id :image/checksum _ ?tx]] data)
       (sort-by second)
       (map first)
       (map #(ds/entity data %))))

(defn templates [data]
  (:viewer/tokens (ds/entity data [:db/ident :viewer])))

(defn elements [data type]
  (->> (ds/q '[:find [?id ...]
               :in $ ?type
               :where
               [_ :viewer/workspace ?ws]
               [?ws :canvas/elements ?id]
               [?id :element/type ?type]] data type)
       (map #(ds/entity data %))))
