(ns ogre.tools.core
  (:require [rum.core :as rum]
            [datascript.core :as ds]
            [ogre.tools.render :as render]))

(defn viewer [data]
  (ds/entity data (:e (first (ds/datoms data :aevt :viewer/workspace)))))

(def schema
  {:viewer/workspace   {:db/valueType :db.type/ref}
   :viewer/workspaces  {:db/valueType   :db.type/ref
                        :db/cardinality :db.cardinality/many}
   :workspace/name     {}
   :workspace/camera   {:db/valueType   :db.type/ref
                        :db/isComponent true}
   :workspace/board    {:db/valueType   :db.type/ref
                        :db/isComponent true}
   :workspace/elements {:db/valueType   :db.type/ref
                        :db/cardinality :db.cardinality/many
                        :db/isComponent true}
   :workspace/viewing  {:db/valueType   :db.type/ref
                        :db/cardinality :db.cardinality/many}
   :camera/x           {}
   :camera/y           {}
   :element/type       {}
   :board/imageURL     {}})

(defn create-workspace []
  {:workspace/name   "New Workspace"
   :workspace/board  {:element/type :board
                      :board/size   64}
   :workspace/camera {:camera/x 0
                      :camera/y 0}})

(defmulti transact
  (fn [data event & args] event))

(defmethod transact :workspace/create
  [data event]
  (let [viewer (viewer data)
        workspace (-> (create-workspace) (assoc :db/id -1))]
    [[:db/add (:db/id viewer) :viewer/workspace -1]
     [:db/add (:db/id viewer) :viewer/workspaces -1]
     workspace]))

(defmethod transact :workspace/change
  [data event id]
  [{:db/id (:db/id (viewer data)) :viewer/workspace id}])

(defmethod transact :workspace/remove
  [data event id]
  (let [viewer (viewer data) workspace (ds/entity data id)]
    (cond
      (= (count (:viewer/workspaces viewer)) 1)
      (let [next (-> (create-workspace) (assoc :db/id -1))]
        [[:db.fn/retractEntity id]
         [:db/add (:db/id viewer) :viewer/workspace -1]
         [:db/add (:db/id viewer) :viewer/workspaces -1]
         next])

      (= (:db/id (:viewer/workspace viewer)) id)
      (let [next (-> (:viewer/workspaces viewer) (disj workspace) (first))]
        [[:db.fn/retractEntity id]
         [:db/add (:db/id viewer) :viewer/workspace (:db/id next)]])

      :else
      [[:db.fn/retractEntity id]])))

(defmethod transact :workspace/update-name
  [data event id name]
  [[:db/add id :workspace/name name]])

(defmethod transact :workspace/view-board-settings
  [data event id]
  (let [workspace (ds/entity data id)]
    [[:db/add id :workspace/viewing (:db/id (:workspace/board workspace))]]))

(defmethod transact :element/update
  [data event id attr value]
  [[:db/add id attr value]])

(defmethod transact :camera/translate [data event id x y]
  [[:db/add id :camera/x x]
   [:db/add id :camera/y y]])

(defn initial-state []
  (ds/db-with (ds/empty-db schema)
              (let [workspace (-> (create-workspace) (assoc :db/id -2))]
                [[:db/add -1 :viewer/workspace -2]
                 [:db/add -1 :viewer/workspaces -2]
                 workspace])))

(defn main []
  (let [props {:data (initial-state) :transact transact}
        element (.querySelector js/document "#root")]
    (rum/mount (render/root props) element)))

(.addEventListener js/window "DOMContentLoaded" main)
