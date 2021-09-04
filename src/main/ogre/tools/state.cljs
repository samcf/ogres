(ns ogre.tools.state
  (:require [uix.core.alpha :as uix :refer [defcontext]]
            [datascript.core :as ds]
            [ogre.tools.query :as query]
            [ogre.tools.txs :as txs]))

(def schema
  {:db/ident         {:db/unique :db.unique/identity}
   :viewer/workspace {:db/valueType :db.type/ref}
   :viewer/tokens    {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :canvas/elements  {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :canvas/map       {:db/valueType :db.type/ref}
   :canvas/selected  {:db/valueType :db.type/ref}
   :image/checksum   {:db/index true}
   :element/flags    {:db/cardinality :db.cardinality/many}})

(defn initial-data []
  (ds/db-with
   (ds/empty-db schema)
   [[:db/add -1 :db/ident :viewer]
    [:db/add -1 :viewer/loaded? false]
    [:db/add -1 :viewer/workspace -2]
    [:db/add -1 :viewer/tokens -3]
    [:db/add -1 :viewer/host? true]
    [:db/add -1 :bounds/self [0 0 0 0]]
    [:db/add -1 :bounds/host [0 0 0 0]]
    [:db/add -1 :bounds/guest [0 0 0 0]]
    [:db/add -2 :element/type :canvas]
    [:db/add -2 :canvas/mode :select]
    [:db/add -2 :canvas/lighting :bright]
    [:db/add -2 :canvas/theme :light]
    [:db/add -2 :pos/vec [0 0]]
    [:db/add -2 :grid/size 70]
    [:db/add -2 :grid/origin [0 0]]
    [:db/add -2 :grid/show true]
    [:db/add -2 :zoom/scales [0.5 0.75 1 1.25 1.50]]
    [:db/add -2 :zoom/scale 1]
    [:db/add -3 :element/type :token]
    [:db/add -3 :token/light [5 5]]
    [:db/add -3 :token/size {:name :medium :size 5}]
    [:db/add -3 :aura/label ""]
    [:db/add -3 :aura/radius 0]]))

(defcontext state)

(defn provider
  "Provides a DataScript in-memory database to the application and causes
   re-renders when transactions are performed."
  [child]
  (let [bean (uix/state 0)
        pawn (uix/state (ds/conn-from-db (initial-data)))
        conn (deref pawn)
        data (deref conn)]

    (uix/effect!
     (fn []
       (ds/listen!
        conn :rerender
        (fn [{:keys [db-after]}]
          (let [{:keys [viewer/host? share/paused?]} (query/viewer db-after)]
            (when (or host? (not paused?))
              (swap! bean inc)))))
       (fn [] (ds/unlisten! conn :rerender))) [nil])

    (uix/context-provider
     [state
      {:conn      conn
       :data      data
       :workspace (query/workspace data)
       :dispatch  (fn [event & args]
                    (let [tx (apply txs/transact data event args)]
                      (ds/transact! conn tx [event args tx])))}] child)))
