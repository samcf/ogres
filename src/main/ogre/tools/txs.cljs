(ns ogre.tools.txs
  (:require [datascript.core :as ds]
            [ogre.tools.query :as query]))

(defn round [[x y] n]
  [(* (js/Math.round (/ x n)) n)
   (* (js/Math.round (/ y n)) n)])

(defn find-index [coll val]
  (reduce (fn [i _]
            (if (= (coll i) val)
              (reduced i)
              (inc i))) 0 coll))

(defn constrain [number minimum maximum]
  (max (min number maximum) minimum))

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
   :zoom/scales        {}
   :zoom/scale         {}
   :element/type       {}
   :element/name       {}
   :token/size         {}
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
    [:db/add -2 :lighting/level :bright]
    [:db/add -2 :grid/show true]
    [:db/add -2 :zoom/scales [0.5 0.75 1 1.25 1.50]]
    [:db/add -2 :zoom/scale 1]
    [:db/add -3 :element/type :token]
    [:db/add -3 :token/light [20 20]]
    [:db/add -3 :token/size {:name :medium :size 5}]]))

(defn initial-workspace []
  {:element/type :workspace
   :position/x 0
   :position/y 0
   :workspace/mode :select
   :grid/size 70
   :grid/origin [0 0]
   :grid/show true
   :zoom/scales [0.5 0.75 1 1.25 1.50]
   :zoom/scale 1
   :lighting/level :bright})

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

(defmethod transact :workspace/set-select-mode
  [data event]
  (let [{:keys [db/id]} (query/workspace data)]
    [[:db/add id :workspace/mode :select]
     [:db/retract id :workspace/selected]]))

(defmethod transact :workspace/toggle-board-options
  [data event]
  (let [{:keys [db/id workspace/selected] :as workspace} (query/workspace data)]
    (if (= selected workspace)
      [[:db/add id :workspace/mode :select]
       [:db/retract id :workspace/selected]]
      [[:db/add id :workspace/mode :board]
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
      [[:db/add (:db/id workspace) :workspace/mode :select]
       [:db/add (:db/id workspace) :workspace/selected element]])))

(defmethod transact :view/clear
  [data]
  (let [workspace (query/workspace data)]
    (when (not (= (:element/type (:workspace/selected workspace)) :workspace))
      [[:db/retract (:db/id workspace) :workspace/selected]])))

(defmethod transact :camera/translate
  [data event x y]
  (let [{:keys [db/id]} (query/workspace data)]
    [[:db/add id :position/x x]
     [:db/add id :position/y y]]))

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
  [data event token tx ty]
  (let [{:keys [db/id position/x position/y zoom/scale]} (query/workspace data)
        template (ds/entity data token)]
    [[:db/add id :workspace/elements -1]
     [:db/add id :workspace/selected -1]
     (merge
      (into {} (ds/touch template))
      {:db/id -1
       :position/x (- (/ tx scale) x)
       :position/y (- (/ ty scale) y)})]))

(defmethod transact :token/remove
  [data event token]
  [[:db/retractEntity token]])

(defmethod transact :token/translate
  [data event id x y]
  [[:db/add id :position/x x]
   [:db/add id :position/y y]])

(defmethod transact :token/change-light
  [data event token bright dim]
  [[:db/add token :token/light [bright dim]]])

(defmethod transact :token/change-size
  [data event token name size]
  [[:db/add token :token/size {:name name :size size}]])

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

(defmethod transact :lighting/change-level
  [data event level]
  (let [{:keys [db/id]} (query/workspace data)]
    [[:db/add id :lighting/level level]]))

(defmethod transact :zoom/step
  [data event step]
  (let [{:keys [db/id zoom/scale zoom/scales]} (query/workspace data)]
    [[:db/add id :zoom/scale (-> (find-index scales scale) (+ step)
                                 (constrain 0 (- (count scales) 1))
                                 (scales))]]))

(defmethod transact :zoom/in [data event]
  (transact data :zoom/step 1))

(defmethod transact :zoom/out [data event]
  (transact data :zoom/step -1))
