(ns ogre.tools.state
  (:require [uix.core.alpha :as uix :refer [defcontext]]
            [datascript.core :as ds]
            [ogre.tools.txs :as txs]
            [ogre.tools.query :as query]))

(defcontext state)

(defn provider
  "Provides a DataScript in-memory database to the application and causes
   re-renders when transactions are performed."
  [child]
  (let [bean (uix/state 0)
        pawn (uix/state (ds/conn-from-db (txs/initial-data)))
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
