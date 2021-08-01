(ns ogre.tools.core
  (:require [rum.core :as rum]
            [datascript.core :as ds]
            [ogre.tools.render :as render]
            [ogre.tools.query :as query]
            [ogre.tools.render.layout :refer [layout]]))

(def schema
  {:viewer/workspace   {:db/valueType :db.type/ref}
   :viewer/workspaces  {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :workspace/viewing  {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :workspace/map      {:db/valueType :db.type/ref}
   :position/x         {}
   :position/y         {}
   :element/type       {}
   :element/name       {}
   :map/id             {:db/index true}
   :map/width          {}
   :map/height         {}
   :map/url            {}})

(defn initial-workspace []
  {:element/type :workspace :position/x 0 :position/y 0})

(defmulti transact
  (fn [data event & args] event))

(defmethod transact :workspace/create
  [data event]
  (let [viewer (query/viewer data)
        workspace (-> (initial-workspace) (assoc :db/id -1))]
    [[:db/add (:db/id viewer) :viewer/workspace -1]
     [:db/add (:db/id viewer) :viewer/workspaces -1]
     workspace]))

(defmethod transact :workspace/change
  [data event id]
  [{:db/id (:db/id (query/viewer data)) :viewer/workspace id}])

(defmethod transact :workspace/remove
  [data event id]
  (let [viewer (query/viewer data) workspace (ds/entity data id)]
    (cond
      (= (count (:viewer/workspaces viewer)) 1)
      (let [next (-> (initial-workspace) (assoc :db/id -1))]
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

(defmethod transact :workspace/change-map
  [data event workspace map]
  [[:db/add workspace :workspace/map map]])

(defmethod transact :element/update
  [data event id attr value]
  [[:db/add id attr value]])

(defmethod transact :view/toggle-settings
  [data event id]
  (let [entity (ds/entity data id)]
    (if (contains? (:workspace/viewing entity) entity)
      [[:db/retract id :workspace/viewing (:db/id entity)]]
      [[:db/add id :workspace/viewing (:db/id entity)]])))

(defmethod transact :view/close
  [data event workspace element]
  [[:db/retract workspace :workspace/viewing element]])

(defmethod transact :camera/translate
  [data event id x y]
  [[:db/add id :position/x x]
   [:db/add id :position/y y]])

(defmethod transact :map/create
  [data event workspace map-data]
  (let [existing (first (ds/datoms data :avet :map/id (:map/id map-data)))]
    (if (nil? existing)
      [(assoc map-data :db/id -1)
       [:db/add (:db/id workspace) :workspace/map -1]]
      [[:db/add (:db/id workspace) :workspace/map (first existing)]])))

(defmethod transact :map/remove
  [data event map]
  [[:db/retractEntity map]])

(defn initial-state []
  (ds/db-with (ds/empty-db schema)
              (let [workspace (-> (initial-workspace) (assoc :db/id -2))]
                [[:db/add -1 :viewer/workspace -2]
                 [:db/add -1 :viewer/workspaces -2]
                 workspace])))

(defn main []
  (let [props {:data (initial-state) :transact transact}
        element (.querySelector js/document "#root")]
    (rum/mount (render/root props (layout)) element)))

(.addEventListener js/window "DOMContentLoaded" main)
