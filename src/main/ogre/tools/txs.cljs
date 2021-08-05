(ns ogre.tools.txs
  (:require [datascript.core :as ds]
            [ogre.tools.query :as query]))

(defn round [[x y] n]
  [(* (js/Math.round (/ x n)) n)
   (* (js/Math.round (/ y n)) n)])

(def schema
  {:db/ident           {:db/unique :db.unique/identity}
   :viewer/workspace   {:db/valueType :db.type/ref}
   :viewer/tokens      {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :workspace/elements {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :workspace/map      {:db/valueType :db.type/ref}
   :workspace/mode     {}
   :workspace/selected {:db/valueType :db.type/ref}
   :grid/size          {}
   :grid/origin        {}
   :position/x         {}
   :position/y         {}
   :element/type       {}
   :element/name       {}
   :image/checksum     {:db/index true}
   :image/width        {}
   :image/height       {}})

(defn initial-data []
  (ds/db-with
   (ds/empty-db schema)
   [[:db/add -1 :db/ident :viewer]
    [:db/add -1 :viewer/workspace -2]
    [:db/add -1 :viewer/tokens -3]
    [:db/add -2 :element/type :workspace]
    [:db/add -2 :workspace/mode :select]
    [:db/add -2 :position/x 0]
    [:db/add -2 :position/y 0]
    [:db/add -2 :grid/size 70]
    [:db/add -2 :grid/origin [0 0]]
    [:db/add -2 :grid/show false]
    [:db/add -3 :element/type :token]]))

(defn initial-workspace []
  {:element/type :workspace :position/x 0 :position/y 0})

(defmulti transact
  (fn [data event & args] event))

(defmethod transact :workspace/create
  [data event]
  [[:db/add [:db/ident :viewer] :viewer/workspace -1]
   (assoc (initial-workspace) :db/id -1)])

(defmethod transact :workspace/change
  [data event id]
  [{:db/id [:db/ident :viewer] :viewer/workspace id}])

(defmethod transact :workspace/remove
  [data event id]
  (let [workspaces (query/workspaces data)
        workspace  (ds/entity data id)]
    (cond
      (= (count workspaces) 1)
      (let [next (-> (initial-workspace) (assoc :db/id -1))]
        [[:db.fn/retractEntity id]
         [:db/add [:db/ident :viewer] :viewer/workspace -1]
         next])

      (= (:db/id workspace) id)
      (let [next (-> (into #{} workspaces) (disj workspace) (first))]
        [[:db.fn/retractEntity id]
         [:db/add [:db/ident :viewer] :viewer/workspace (:db/id next)]])

      :else
      [[:db.fn/retractEntity id]])))

(defmethod transact :workspace/change-map
  [data event map]
  (let [workspace (query/workspace data)]
    [[:db/add (:db/id workspace) :workspace/map map]]))

(defmethod transact :workspace/toggle-board-options
  [data event]
  (let [{:keys [db/id workspace/selected] :as workspace} (query/workspace data)]
    (if (= selected workspace)
      [[:db/retract id :workspace/selected]]
      [[:db/add id :workspace/mode :select]
       [:db/add id :workspace/selected id]])))

(defmethod transact :workspace/toggle-grid-options
  [data event]
  (let [{:keys [db/id workspace/mode]} (query/workspace data)]
    (if (= mode :grid)
      [[:db/add id :workspace/mode :select]]
      [[:db/add id :workspace/mode :grid]
       [:db/retract id :workspace/selected]])))

(defmethod transact :element/update
  [data event id attr value]
  [[:db/add id attr value]])

(defmethod transact :view/toggle
  [data event element]
  (let [workspace (query/workspace data)]
    (if (= (:db/id (:workspace/selected workspace)) element)
      [[:db/retract (:db/id workspace) :workspace/selected element]]
      [[:db/add (:db/id workspace) :workspace/selected element]])))

(defmethod transact :view/clear
  [data]
  (let [workspace (query/workspace data)]
    (when (not (= (:element/type (:workspace/selected workspace)) :workspace))
      [[:db/retract (:db/id workspace) :workspace/selected]])))

(defmethod transact :camera/translate
  [data event id x y]
  [[:db/add id :position/x x]
   [:db/add id :position/y y]])

(defmethod transact :map/create
  [data event workspace map-data]
  (let [existing (first (ds/datoms data :avet :image/checksum (:image/checksum map-data)))]
    (if (nil? existing)
      [(assoc map-data :db/id -1)
       [:db/add (:db/id workspace) :workspace/map -1]]
      [[:db/add (:db/id workspace) :workspace/map (first existing)]])))

(defmethod transact :map/remove
  [data event map]
  [[:db/retractEntity map]])

(defmethod transact :token/create
  [data event id token tx ty]
  (let [workspace (ds/entity data id)
        {:keys [position/x position/y]} workspace]
    [(assoc token
            :db/id -1
            :position/x (- tx x)
            :position/y (- ty y))
     [:db/add id :workspace/elements -1]
     [:db/add id :workspace/selected -1]]))

(defmethod transact :token/translate
  [data event id x y]
  [[:db/add id :position/x x]
   [:db/add id :position/y y]])

(defmethod transact :grid/change-size
  [data event size]
  (let [{:keys [db/id]} (query/workspace data)]
    [[:db/add id :grid/size size]]))

(defmethod transact :grid/draw
  [data event ox oy size]
  (let [workspace (query/workspace data)
        {:keys [db/id workspace/map]}   workspace
        {:keys [position/x position/y]} workspace
        {:keys [image/width]}           map
        origin [(- ox x) (- oy y)]]
    (if (nil? map)
      [[:db/add id :grid/size size]
       [:db/add id :grid/origin origin]]
      (let [next (->> (range (- size 4) (+ size 4))
                      (reduce (fn [_ n] (when (zero? (mod width n)) (reduced n)))))]
        [[:db/add id :grid/size (or next size)]
         [:db/add id :grid/origin (if next (round origin next) origin)]]))))

(defmethod transact :grid/toggle
  [data event]
  (let [{:keys [db/id grid/show]} (query/workspace data)]
    [[:db/add id :grid/show (not show)]]))
