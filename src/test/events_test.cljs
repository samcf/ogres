(ns events-test
  (:require [cljs.test :refer-macros [deftest is]]
            [datascript.core :as ds :refer [transact! entity]]
            [ogres.app.events :refer [event-tx-fn]]
            [ogres.app.provider.state :refer [initial-data]]))

(defn dispatch [conn event & args]
  (transact! conn [[:db.fn/call (fn [db] (apply event-tx-fn db event args))]]))

(defn local [conn]
  (entity @conn [:db/ident :local]))

(deftest test-panel
  (let [conn (ds/conn-from-db (initial-data :host))]
    (dispatch conn :local/select-panel :session)
    (is (= (:panel/selected (local conn)) :session))

    (dispatch conn :local/toggle-panel)
    (is (= (:panel/expanded (local conn)) false))

    (dispatch conn :local/toggle-panel)
    (is (= (:panel/expanded (local conn)) true))

    (dispatch conn :local/toggle-panel)
    (dispatch conn :local/select-panel :tokens)
    (is (= (:panel/expanded (local conn)) true))
    (is (= (:panel/selected (local conn)) :tokens))))

(deftest test-scene-focus
  (let [db (initial-data :host)
        sc (:db/id (:camera/scene (:local/camera (entity db [:db/ident :local]))))
        tx [{:db/ident :root
             :root/session
             {:db/ident :session
              :session/host {:db/ident :local}
              :session/conns
              [{:local/type :conn :local/uuid (random-uuid) :local/cameras {:db/id -1 :camera/scene sc} :local/camera -1}
               {:local/type :conn :local/uuid (random-uuid) :local/cameras {:db/id -2 :camera/scene sc} :local/camera -2}
               {:local/type :conn :local/uuid (random-uuid) :local/cameras {:db/id -3 :camera/scene sc} :local/camera -3}]}}]
        conn (ds/conn-from-db (ds/db-with db tx))]
    (dispatch conn :scenes/create)
    (dispatch conn :session/focus)
    (let [{conns :session/conns} (entity @conn [:db/ident :session])]
      (is (every? (comp #{2} count :local/cameras) conns)
          "New cameras are created for users that don't already have one for the newly focused scene.")
      (is (every? #{(:camera/scene (:local/camera (local conn)))} (map (comp :camera/scene :local/camera) conns))
          "Every user is viewing the same scene as the host.")
      (is (= (count (into #{} (map :local/camera) conns)) 3)
          "Every user has a distinct camera entity."))
    (dispatch conn :scenes/change sc)
    (let [{conns :session/conns} (entity @conn [:db/ident :session])]
      (is (every? (comp not #{sc} :db/id :camera/scene :local/camera) conns)
          "Users remain on the scene even if the host changes theirs."))))
