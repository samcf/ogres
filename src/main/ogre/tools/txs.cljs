(ns ogre.tools.txs
  (:require [datascript.core :as ds]
            [clojure.set :refer [union]]
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

(defn normalize [[ax ay bx by]]
  [(min ax bx) (min ay by) (max ax bx) (max ay by)])

(defn within? [x y [ax ay bx by]]
  (and (> x ax) (> y ay) (< x bx) (< y by)))

(defn with-suffix-txs
  "Returns a vector of tx tuples `[[:db/add id :initiative/suffix suffix] ...]`
   for entities whose names are duplicates within the collection given and do
   not already have a unique suffix assigned yet."
  [entities]
  (->>
   (reduce
    (fn [m {:keys [db/id element/name initiative/suffix]}]
      (update
       m name
       (fn [[idents current]]
         [(if (= suffix nil) (conj idents id) idents)
          (max current suffix)]))) {} entities)
   (mapcat
    (fn [[_ [idents suffix]]]
      (if (or (> (count idents) 1) (number? suffix))
        (loop [idents idents suffix (inc suffix) txs []]
          (if-let [id (first idents)]
            (recur
             (rest idents)
             (inc suffix)
             (conj txs [:db/add id :initiative/suffix suffix]))
            txs)) [])))))

(def initiative-attrs
  #{:initiative/member?
    :initiative/roll
    :initiative/health
    :initiative/suffix})

(defn initial-canvas []
  {:element/type :canvas
   :pos/vec [0 0]
   :canvas/mode :select
   :canvas/lighting :bright
   :canvas/theme :light
   :grid/size 70
   :grid/origin [0 0]
   :grid/show true
   :zoom/scales [0.25 0.5 0.75 1 (/ 1 0.75) (/ 1 0.50) (/ 1 0.25)]
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
    [[:db/add (:db/id workspace) :canvas/map map]
     [:db/add (:db/id workspace) :pos/vec [0 0]]]))

(defmethod transact :canvas/toggle-mode
  [data event mode]
  (let [{id :db/id current :canvas/mode} (query/workspace data)]
    [[:db/add id :canvas/mode mode]
     (if (not= current mode)
       [:db/retract id :canvas/selected])
     (if (contains? #{:circle :rect :cone :line} mode)
       [:db/add id :canvas/last-shape mode])]))

(defmethod transact :canvas/toggle-theme
  [data event]
  (let [{:keys [db/id canvas/theme]} (query/workspace data)]
    [[:db/add id :canvas/theme (if (= theme :dark) :light :dark)]]))

(defmethod transact :canvas/modifier-start
  [data event modifier]
  (let [{:keys [db/id]} (query/workspace data)]
    [[:db/add id :canvas/modifier modifier]]))

(defmethod transact :canvas/modifier-release
  [data event]
  (let [{:keys [db/id]} (query/workspace data)]
    [[:db/retract id :canvas/modifier]]))

(defmethod transact :element/update
  [data event idents attr value]
  (for [id idents]
    [:db/add id attr value]))

(defmethod transact :element/select
  [data event element]
  (let [{:keys [db/id canvas/selected]} (query/workspace data)]
    [[:db/retract id :canvas/selected]
     [:db/add id :canvas/selected element]]))

(defmethod transact :element/remove
  [data event idents]
  (for [id idents]
    [:db/retractEntity id]))

(defmethod transact :element/flag
  [data event idents flag add?]
  (for [id idents]
    [(if add? :db/add :db/retract) id :element/flags flag]))

(defmethod transact :camera/translate
  [data event x y]
  (let [{:keys [db/id]} (query/workspace data)]
    [[:db/add id :pos/vec (round [x y] 1)]]))

(defmethod transact :map/create
  [data event workspace map-data]
  (let [checksum (:image/checksum map-data)
        existing (ds/entity data [:image/checksum checksum])]
    (if (nil? existing)
      [(assoc map-data :db/id -1)
       [:db/add (:db/id workspace) :canvas/map -1]]
      [[:db/add (:db/id workspace) :canvas/map (:db/id existing)]])))

(defmethod transact :image/set-url
  [data event checksum url]
  [[:db/add [:image/checksum checksum] :image/url url]])

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

(defmethod transact :token/translate-all
  [data event idents ox oy]
  (for [id idents]
    (let [{[x y] :pos/vec} (ds/entity data id)]
      [:db/add id :pos/vec (round [(+ x ox) (+ y oy)] 1)])))

(defmethod transact :token/change-light
  [data event idents bright dim]
  (for [id idents]
    [:db/add id :token/light [bright dim]]))

(defmethod transact :token/change-size
  [data event idents name size]
  (for [id idents]
    [:db/add id :token/size {:name name :size size}]))

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
  [data event step mx my]
  (let [workspace (query/workspace data)
        {:keys [zoom/scale zoom/scales]} workspace
        {[cx cy] :pos/vec} workspace

        next
        (-> (find-index scales scale)
            (+ step)
            (constrain 0 (- (count scales) 1))
            (scales))

        factor
        (/ next scale)

        [dx dy]
        [(/ (- (* mx factor) mx) next)
         (/ (- (* my factor) my) next)]]
    [[:db/add (:db/id workspace) :pos/vec (round [(- cx dx) (- cy dy)] 1)]
     [:db/add (:db/id workspace) :zoom/scale next]]))

(defmethod transact :zoom/in [data event x y]
  (transact data :zoom/step 1 x y))

(defmethod transact :zoom/out [data event x y]
  (transact data :zoom/step -1 x y))

(defmethod transact :aura/change-label
  [data event idents label]
  (for [id idents]
    [:db/add id :aura/label label]))

(defmethod transact :aura/change-radius
  [data event idents radius]
  (for [id idents]
    [:db/add id :aura/radius radius]))

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

(defmethod transact :bounds/change
  [data event w-host? bounds]
  (let [{id :db/id v-host? :viewer/host?} (query/viewer data)]
    (cond-> []
      (= w-host? v-host?) (conj [:db/add id :bounds/self bounds])
      (= w-host? true) (conj [:db/add id :bounds/host bounds])
      (= w-host? false) (conj [:db/add id :bounds/guest bounds]))))

(defmethod transact :selection/from-rect
  [data event vecs]
  (let [{:keys [db/id canvas/elements]} (query/workspace data)]
    (concat [[:db/add id :canvas/mode :select]]
            (for [element elements
                  :let [{type :element/type [x y] :pos/vec} element]
                  :when (and (= type :token) (within? x y (normalize vecs)))]
              [:db/add id :canvas/selected (:db/id element)]))))

(defmethod transact :selection/clear
  [data event]
  (let [{:keys [db/id]} (query/workspace data)]
    [[:db/retract id :canvas/selected]]))

(defmethod transact :selection/remove
  [data event]
  (let [{:keys [canvas/selected]} (query/workspace data)]
    (for [{:keys [db/id]} selected]
      [:db/retractEntity id])))

(defmethod transact :initiative/toggle
  [data event idents add?]
  (if add?
    (let [adding (map #(ds/entity data %) idents)
          exists (query/initiating data)]
      (->> (union (set adding) (set exists))
           (with-suffix-txs)
           (concat
            (for [id idents]
              [:db/add id :initiative/member? true]))))
    (for [id idents attr initiative-attrs]
      [:db/retract id attr])))

(defmethod transact :initiative/change-roll
  [data event id roll]
  (let [parsed (.parseFloat js/window roll)]
    (cond
      (or (nil? roll) (= roll ""))
      [[:db/retract id :initiative/roll]]

      (.isNaN js/Number parsed)
      []

      :else
      [[:db/add id :initiative/roll parsed]])))

(defmethod transact :initiative/roll-inc
  [data event id]
  (let [{:keys [initiative/roll]} (ds/entity data id)]
    [[:db/add id :initiative/roll (inc roll)]]))

(defmethod transact :initiative/roll-dec
  [data event id]
  (let [{:keys [initiative/roll]} (ds/entity data id)]
    [[:db/add id :initiative/roll (if (nil? roll) 0 (dec roll))]]))

(defmethod transact :initiative/roll-all
  [data event]
  (for [entity (query/initiating data)
        :let [{:keys [db/id initiative/roll]} entity]
        :when (and (nil? roll)
                   (not (contains? (:element/flags entity) :player)))]
    [:db/add id :initiative/roll (inc (rand-int 20))]))

(defmethod transact :initiative/reset-rolls
  [data event]
  (let [elements (query/initiating data)]
    (for [{:keys [db/id]} elements]
      [:db/retract id :initiative/roll])))

(defmethod transact :initiative/change-health
  [data event id f value]
  (let [parsed (.parseFloat js/window value)]
    (if (.isNaN js/Number parsed) []
        (let [{:keys [initiative/health]} (ds/entity data id)]
          [[:db/add id :initiative/health (f health parsed)]]))))

(defmethod transact :initiative/leave
  [data]
  (for [entity (query/initiating data)
        attr   initiative-attrs]
    [:db/retract (:db/id entity) attr]))

(defmethod transact :storage/reset []
  [])
