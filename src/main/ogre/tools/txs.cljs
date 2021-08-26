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
  {:db/ident         {:db/unique :db.unique/identity}
   :viewer/workspace {:db/valueType :db.type/ref}
   :viewer/tokens    {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many}
   :canvas/elements  {:db/valueType :db.type/ref :db/cardinality :db.cardinality/many :db/isComponent true}
   :canvas/map       {:db/valueType :db.type/ref}
   :canvas/selected  {:db/valueType :db.type/ref}
   :image/checksum   {:db/index true}})

(defn initial-data []
  (ds/db-with
   (ds/empty-db schema)
   [[:db/add -1 :db/ident :viewer]
    [:db/add -1 :viewer/loaded? false]
    [:db/add -1 :viewer/workspace -2]
    [:db/add -1 :viewer/tokens -3]
    [:db/add -1 :viewer/host? true]
    [:db/add -1 :canvas/bounds [0 0 0 0]]
    [:db/add -1 :canvas/guest-bounds [0 0 0 0]]
    [:db/add -2 :element/type :canvas]
    [:db/add -2 :canvas/mode :select]
    [:db/add -2 :pos/vec [0 0]]
    [:db/add -2 :grid/size 70]
    [:db/add -2 :grid/origin [0 0]]
    [:db/add -2 :canvas/lighting :bright]
    [:db/add -2 :grid/show true]
    [:db/add -2 :zoom/scales [0.5 0.75 1 1.25 1.50]]
    [:db/add -2 :zoom/scale 1]
    [:db/add -3 :element/type :token]
    [:db/add -3 :token/light [0 5]]
    [:db/add -3 :token/size {:name :medium :size 5}]
    [:db/add -3 :aura/label ""]
    [:db/add -3 :aura/radius 0]]))

(defn initial-canvas []
  {:element/type :canvas
   :pos/vec [0 0]
   :canvas/mode :select
   :canvas/lighting :bright
   :grid/size 70
   :grid/origin [0 0]
   :grid/show true
   :zoom/scales [0.5 0.75 1 1.25 1.50]
   :zoom/scale 1})

(defmulti transact
  (fn [data event & args] event))

(defmethod transact :workspace/create
  [data event]
  [[:db/add [:db/ident :viewer] :viewer/workspace -1]
   (assoc (initial-canvas) :db/id -1)])

(defmethod transact :workspace/change
  [data event id]
  [{:db/id [:db/ident :viewer] :viewer/workspace id}])

(defmethod transact :workspace/remove
  [data event id]
  (let [workspaces (query/workspaces data)
        workspace  (ds/entity data id)]
    (cond
      (= (count workspaces) 1)
      (let [next (-> (initial-canvas) (assoc :db/id -1))]
        [[:db.fn/retractEntity id]
         [:db/add [:db/ident :viewer] :viewer/workspace -1]
         next])

      (= (:db/id workspace) id)
      (let [next (-> (into #{} workspaces) (disj workspace) (first))]
        [[:db.fn/retractEntity id]
         [:db/add [:db/ident :viewer] :viewer/workspace (:db/id next)]])

      :else
      [[:db.fn/retractEntity id]])))

(defmethod transact :canvas/change-map
  [data event map]
  (let [workspace (query/workspace data)]
    [[:db/add (:db/id workspace) :canvas/map map]]))

(defmethod transact :canvas/toggle-mode
  [data event mode]
  (let [{id :db/id current :canvas/mode} (query/workspace data)]
    [[:db/add id :canvas/mode (if (= current mode) :select mode)]
     [:db/retract id :canvas/selected]]))

(defmethod transact :canvas/change-bounds
  [data event w-host? x y w h]
  (let [{v-host? :viewer/host?} (query/viewer data)]
    (cond
      (= w-host? v-host?)
      [[:db/add [:db/ident :viewer] :canvas/bounds [x y w h]]]

      (and (not w-host?) v-host?)
      [[:db/add [:db/ident :viewer] :canvas/guest-bounds [x y w h]]]

      :else [])))

(defmethod transact :element/update
  [data event id attr value]
  [[:db/add id attr value]])

(defmethod transact :element/select
  [data event element]
  (let [{:keys [db/id canvas/selected]} (query/workspace data)]
    (if (= (:db/id selected) element)
      [[:db/retract id :canvas/selected element]]
      [[:db/add id :canvas/mode :select]
       [:db/add id :canvas/selected element]])))

(defmethod transact :element/remove
  [data event token]
  [[:db/retractEntity token]])

(defmethod transact :view/clear
  [data]
  (let [workspace (query/workspace data)]
    (when (not (= (:element/type (:canvas/selected workspace)) :canvas))
      [[:db/retract (:db/id workspace) :canvas/selected]])))

(defmethod transact :camera/translate
  [data event x y]
  (let [{:keys [db/id]} (query/workspace data)]
    [[:db/add id :pos/vec (round [x y] 1)]]))

(defmethod transact :map/create
  [data event workspace map-data]
  (let [existing (first (ds/datoms data :avet :image/checksum (:image/checksum map-data)))]
    (if (nil? existing)
      [(assoc map-data :db/id -1)
       [:db/add (:db/id workspace) :canvas/map -1]]
      [[:db/add (:db/id workspace) :canvas/map (first existing)]])))

(defmethod transact :map/remove
  [data event map]
  [[:db/retractEntity map]])

(defmethod transact :token/create
  [data event token vector]
  (let [{:keys [db/id]} (query/workspace data)
        template        (ds/entity data token)]
    [[:db/add id :canvas/elements -1]
     [:db/add id :canvas/selected -1]
     [:db/add id :canvas/mode :select]
     (merge
      (into {} (ds/touch template))
      {:db/id -1 :pos/vec vector})]))

(defmethod transact :token/translate
  [data event id x y]
  [[:db/add id :pos/vec (round [x y] 1)]])

(defmethod transact :token/change-light
  [data event token bright dim]
  [[:db/add token :token/light [bright dim]]])

(defmethod transact :token/change-size
  [data event token name size]
  [[:db/add token :token/size {:name name :size size}]])

(defmethod transact :shape/create
  [data event kind vecs]
  (let [{id :db/id} (query/workspace data)]
    [[:db/add -1 :element/type :shape]
     [:db/add -1 :shape/kind kind]
     [:db/add -1 :pos/vec [0 0]]
     [:db/add -1 :shape/vecs vecs]
     [:db/add -1 :shape/color "#f44336"]
     [:db/add -1 :shape/opacity 0.25]
     [:db/add -1 :shape/pattern :solid]
     [:db/add id :canvas/elements -1]]))

(defmethod transact :shape/translate
  [data event id x y]
  [[:db/add id :pos/vec (round [x y] 1)]])

(defmethod transact :grid/change-size
  [data event size]
  (let [{:keys [db/id]} (query/workspace data)]
    [[:db/add id :grid/size size]]))

(defmethod transact :grid/draw
  [data event ox oy size]
  (let [workspace (query/workspace data)
        {id :db/id scale :zoom/scale [x y] :pos/vec image :canvas/map} workspace
        {width :image/width} image
        size    (js/Math.round (/ size scale))
        [sx sy] [(/ ox scale) (/ oy scale)]
        origin  [(- sx x) (- sy y)]]
    (if (nil? image)
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

(defmethod transact :canvas/change-lighting
  [data event level]
  (let [{:keys [db/id]} (query/workspace data)]
    [[:db/add id :canvas/lighting level]]))

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

(defmethod transact :aura/change-label
  [data event token label]
  [[:db/add token :aura/label label]])

(defmethod transact :aura/change-radius
  [data event token radius]
  [[:db/add token :aura/radius radius]])

(defmethod transact :aura/change-color
  [data event token color]
  [[:db/add token :aura/color color]])

(defmethod transact :share/initiate []
  [])

(defmethod transact :share/toggle [data _ open?]
  (let [{:keys [viewer/host?]} (query/viewer data)]
    [[:db/add [:db/ident :viewer] :share/open? open?]
     [:db/add [:db/ident :viewer] :share/paused? false]
     [:db/add [:db/ident :viewer] :viewer/privileged? (and host? open?)]]))

(defmethod transact :share/switch
  ([data event]
   (let [viewer (ds/entity data [:db/ident :viewer])]
     (transact data event (not (:share/paused? viewer)))))
  ([data event paused?]
   [[:db/add [:db/ident :viewer] :share/paused? paused?]]))

(defmethod transact :storage/reset []
  [])
