(ns ogre.tools.query
  (:require [datascript.core :as ds]))

(defn viewer [data]
  (ds/entity data [:db/ident :viewer]))

(defn workspace [data]
  (:viewer/workspace (ds/entity data [:db/ident :viewer])))

(defn workspaces [data]
  (->> (ds/q '[:find ?id ?tx :where [?id :element/type :canvas ?tx]] data)
       (sort-by second)
       (map first)
       (map #(ds/entity data %))))

(defn initiating [data]
  (map (partial ds/entity data)
       (ds/q '[:find [?id ...]
               :where
               [_   :viewer/workspace ?ws]
               [?ws :canvas/tokens ?id]
               [?id :initiative/member? true]] data)))
