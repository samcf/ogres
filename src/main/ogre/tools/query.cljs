(ns ogre.tools.query
  (:require [datascript.core :as ds]))

(defn viewer [data]
  (ds/entity data [:db/ident :viewer]))

(defn workspace [data]
  (:viewer/workspace (ds/entity data [:db/ident :viewer])))
