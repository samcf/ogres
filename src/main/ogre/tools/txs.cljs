(ns ogre.tools.txs
  (:require [datascript.core :as ds]
            [clojure.set :refer [union]]
            [ogre.tools.vec :refer [normalize within?]]))

(def zoom-scales
  [0.15 0.30 0.50 0.75 0.90 1 1.25 1.50 2 3 4])

(defn round [[x y] n]
  [(* (js/Math.round (/ x n)) n)
   (* (js/Math.round (/ y n)) n)])

(defn to-precision [n p]
  (js/Number (.toFixed (js/Number.parseFloat n) p)))

(defn constrain [n min max]
  (clojure.core/max (clojure.core/min n max) min))

(defn suffix-txs
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
  #{:initiative/roll
    :initiative/health
    :initiative/suffix})

(defmulti transact
  (fn [data event & args] event))

(defmethod transact :workspace/create
  [data event]
  [[:db/retract [:db/ident :canvas] :db/ident]
   [:db/add [:db/ident :root] :root/canvases -1]
   [:db/add [:db/ident :root] :root/canvas -1]
   {:db/id -1 :db/ident :canvas :element/name ""}])

(defmethod transact :workspace/change
  [data event id]
  [[:db/retract [:db/ident :canvas] :db/ident]
   [:db/add id :db/ident :canvas]
   [:db/add [:db/ident :root] :root/canvas id]])

(defmethod transact :workspace/remove
  [data event id]
  (let [result   (ds/pull data '[{:root/canvases [*]}] [:db/ident :root])
        canvas   (ds/pull data '[*] [:db/ident :canvas])
        canvases (:root/canvases result)]
    (cond
      (= (count canvases) 1)
      (let [next {:db/id -1 :db/ident :canvas :element/name ""}]
        [[:db.fn/retractEntity id]
         [:db/add [:db/ident :root] :root/canvases -1]
         [:db/add [:db/ident :root] :root/canvas -1]
         next])

      (= (:db/id canvas) id)
      (let [next (-> (set canvases) (disj canvas) (first))]
        [[:db.fn/retractEntity id]
         [:db/add [:db/ident :root] :root/canvas (:db/id next)]
         [:db/add (:db/id next) :db/ident :canvas]])

      :else
      [[:db.fn/retractEntity id]])))

(defmethod transact :canvas/change-scene
  [data event scene]
  [[:db/add [:db/ident :canvas] :canvas/scene scene]
   [:db/add [:db/ident :canvas] :pos/vec [0 0]]])

(defmethod transact :canvas/toggle-mode
  [data event mode]
  (let [ident  [:db/ident :canvas]
        select [:db/id :canvas/mode :canvas/selected :panel/prev]
        result (ds/pull data select ident)
        {curr :canvas/mode
         prev :panel/prev
         selected :canvas/selected} result]
    (concat [[:db/add ident :canvas/mode mode]]
            (if (not= mode curr)
              [[:db/retract ident :canvas/selected]
               [:db/add ident :panel/curr (or prev :canvas)]]))))

(defmethod transact :canvas/toggle-theme
  [data event]
  (let [ident  [:db/ident :canvas]
        canvas (ds/pull data [:canvas/theme] ident)]
    [[:db/add ident :canvas/theme (if (= (:canvas/theme canvas) :dark) :light :dark)]]))

(defmethod transact :canvas/modifier-start
  [data event modifier]
  [[:db/add [:db/ident :canvas] :canvas/modifier modifier]])

(defmethod transact :canvas/modifier-release
  [data event]
  [[:db/retract [:db/ident :canvas] :canvas/modifier]])

(defmethod transact :element/update
  [data event idents attr value]
  (for [id idents]
    [:db/add id attr value]))

(defmethod transact :element/select
  [data event element]
  (let [{:keys [element/type]} (ds/pull data [:element/type] element)]
    [[:db/retract [:db/ident :canvas] :canvas/selected]
     [:db/add [:db/ident :canvas] :canvas/selected element]
     [:db/add [:db/ident :canvas] :panel/curr type]]))

(defmethod transact :element/remove
  [data event idents]
  (let [{:keys [panel/prev]} (ds/pull data [[:panel/prev :default :canvas]] [:db/ident :canvas])]
    (into [[:db/add [:db/ident :canvas] :panel/curr prev]]
          (for [id idents] [:db/retractEntity id]))))

(defmethod transact :element/flag
  [data event idents flag add?]
  (let [tokens (ds/pull-many data [:db/id :element/flags] idents)]
    (for [{:keys [db/id element/flags] :or {flags #{}}} tokens]
      [:db/add id :element/flags ((if add? conj disj) flags flag)])))

(defmethod transact :camera/translate
  [data event x y]
  [[:db/add [:db/ident :canvas] :pos/vec (round [x y] 1)]])

(defmethod transact :scene/create
  [data event map-data]
  (let [checksum (:image/checksum map-data)
        existing (ds/entity data [:image/checksum checksum])]
    (if (nil? existing)
      [(assoc map-data :db/id -1)
       [:db/add [:db/ident :root] :root/scenes -1]
       [:db/add [:db/ident :canvas] :canvas/scene -1]]
      [[:db/add [:db/ident :canvas] :canvas/scene [:image/checksum checksum]]])))

(defmethod transact :map/remove
  [data event map]
  [[:db/retractEntity map]])

(defmethod transact :token/create
  [data event vector]
  [[:db/add [:db/ident :canvas] :canvas/tokens -1]
   [:db/add [:db/ident :canvas] :canvas/selected -1]
   [:db/add [:db/ident :canvas] :canvas/mode :select]
   [:db/add [:db/ident :canvas] :panel/curr :token]
   {:db/id -1 :element/type :token :pos/vec vector}])

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

(defmethod transact :token/change-stamp
  [data event idents checksum]
  (for [id idents]
    [:db/add id :token/stamp [:image/checksum checksum]]))

(defmethod transact :token/remove-stamp
  [data event idents]
  (for [id idents]
    [:db/retract id :token/stamp]))

(defmethod transact :shape/create
  [data event kind vecs]
  [[:db/add -1 :element/type :shape]
   [:db/add -1 :shape/kind kind]
   [:db/add -1 :shape/vecs vecs]
   [:db/add [:db/ident :canvas] :canvas/shapes -1]
   [:db/add [:db/ident :canvas] :canvas/mode :select]
   [:db/add [:db/ident :canvas] :canvas/selected -1]
   [:db/add [:db/ident :canvas] :panel/curr :shape]])

(defmethod transact :shape/translate
  [data event id x y]
  [[:db/add id :pos/vec (round [x y] 1)]])

(defmethod transact :grid/change-size
  [data event size]
  [[:db/add [:db/ident :canvas] :grid/size size]])

(defmethod transact :grid/draw
  [data _ _ _ size]
  (let [ident  [:db/ident :canvas]
        select [:db/id [:zoom/scale :default 1] {:canvas/scene [:image/width]}]
        result (ds/pull data select ident)
        {scale :zoom/scale
         scene :canvas/scene
         {width :image/width} :canvas/scene} result
        size (js/Math.round (/ size scale))]
    (if (nil? scene)
      [[:db/add ident :grid/size size]
       [:db/add ident :canvas/mode :select]]
      (let [pattern [size (+ size 1) (- size 1) (+ size 2) (- size 2) (+ size 3) (- size 3) (+ size 4) (- size 4)]
            next    (reduce (fn [_ n] (when (zero? (mod width n)) (reduced n))) pattern)]
        [[:db/add ident :grid/size (or next size)]
         [:db/add ident :canvas/mode :select]]))))

(defmethod transact :grid/toggle
  [data event]
  (let [{:keys [grid/show]} (ds/pull data [[:grid/show :default :true]] [:db/ident :canvas])]
    [[:db/add [:db/ident :canvas] :grid/show (not show)]]))

(defmethod transact :canvas/change-lighting
  [data event level]
  [[:db/add [:db/ident :canvas] :canvas/lighting level]])

(defmethod transact :canvas/change-color
  [data event color]
  [[:db/add [:db/ident :canvas] :canvas/color color]])

(defmethod transact :zoom/change
  ([data event]
   (transact data event 1))
  ([data event next]
   (let [result    (ds/pull data [:bounds/self] [:db/ident :root])
         [_ _ w h] (:bounds/self result)]
     (transact data event next (/ w 2) (/ h 2))))
  ([data event next x y]
   (let [select [[:zoom/scale :default 1] [:pos/vec :default [0 0]]]
         result (ds/pull data select [:db/ident :canvas])
         {prev :zoom/scale [cx cy] :pos/vec} result
         fx (/ next prev)
         dx (/ (- (* x fx) x) next)
         dy (/ (- (* y fx) y) next)]
     [[:db/add [:db/ident :canvas] :pos/vec (round [(- cx dx) (- cy dy)] 1)]
      [:db/add [:db/ident :canvas] :zoom/scale next]])))

(defmethod transact :zoom/delta
  [data event delta x y]
  (let [canvas (ds/pull data [[:zoom/scale :default 1]] [:db/ident :canvas])
        next   (-> (:zoom/scale canvas)
                   (js/Math.log)
                   (+ delta)
                   (js/Math.exp)
                   (to-precision 2)
                   (constrain 0.15 4))]
    (transact data :zoom/change next x y)))

(defmethod transact :zoom/in [data]
  (let [info (ds/pull data [[:zoom/scale :default 1]] [:db/ident :canvas])
        prev (:zoom/scale info)
        next (reduce (fn [n s] (if (> s prev) (reduced s) n)) prev zoom-scales)]
    (transact data :zoom/change next)))

(defmethod transact :zoom/out [data]
  (let [info (ds/pull data [[:zoom/scale :default 1]] [:db/ident :canvas])
        prev (:zoom/scale info)
        next (reduce (fn [n s] (if (< s prev) (reduced s) n)) prev (reverse zoom-scales))]
    (transact data :zoom/change next)))

(defmethod transact :zoom/reset [data]
  (transact data :zoom/change 1))

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
  (let [{:keys [root/host?]} (ds/pull data [:root/host?] [:db/ident :root])]
    [[:db/add [:db/ident :root] :share/open? open?]
     [:db/add [:db/ident :root] :share/paused? false]
     [:db/add [:db/ident :root] :root/privileged? (and host? open?)]]))

(defmethod transact :share/switch
  ([data event]
   (let [{:keys [share/paused?]} (ds/pull data [:share/paused?] [:db/ident :root])]
     (transact data event (not paused?))))
  ([data event paused?]
   [[:db/add [:db/ident :root] :share/paused? paused?]]))

(defmethod transact :bounds/change
  [data event w-host? bounds]
  (let [{id :db/id v-host? :root/host?} (ds/pull data [:db/id :root/host?] [:db/ident :root])]
    (cond-> []
      (= w-host? v-host?) (conj [:db/add id :bounds/self bounds])
      (= w-host? true) (conj [:db/add id :bounds/host bounds])
      (= w-host? false) (conj [:db/add id :bounds/guest bounds]))))

(defmethod transact :selection/from-rect
  [data event vecs]
  (let [selector   [{:root/canvas [:db/id {:canvas/tokens [:db/id :pos/vec]}]}]
        root       (ds/pull data selector [:db/ident :root])
        canvas     (:root/canvas root)
        normalized (normalize vecs)
        selected   (filter
                    (fn [{[x y] :pos/vec}]
                      (within? x y normalized)) (:canvas/tokens canvas))]
    (concat [[:db/add (:db/id canvas) :canvas/mode :select]]
            (if (seq selected)
              [[:db/add (:db/id canvas) :panel/curr :token]])
            (for [entity selected]
              [:db/add (:db/id canvas) :canvas/selected (:db/id entity)]))))

(defmethod transact :selection/clear
  [data event]
  (let [{:keys [panel/prev]} (ds/pull data [:panel/prev] [:db/ident :canvas])]
    [[:db/retract [:db/ident :canvas] :canvas/selected]
     [:db/add [:db/ident :canvas] :panel/curr (or prev :canvas)]]))

(defmethod transact :selection/remove
  [data event]
  (let [select [:canvas/selected :panel/prev]
        result (ds/pull data select [:db/ident :canvas])
        {:keys [canvas/selected panel/prev]} result]
    (into [[:db/add [:db/ident :canvas] :panel/curr (or prev :canvas)]]
          (for [entity selected]
            [:db/retractEntity (:db/id entity)]))))

(defmethod transact :initiative/toggle
  [data event idents adding?]
  (let [select-t [:db/id :element/name :initiative/suffix]
        select-r [{:root/canvas [:db/id {:canvas/initiative select-t}]}]
        result   (ds/pull data select-r [:db/ident :root])
        adding   (ds/pull-many data select-t idents)
        {{id :db/id exists :canvas/initiative} :root/canvas} result]
    (if adding?
      (->> (union (set adding) (set exists))
           (suffix-txs)
           (into [{:db/id id :canvas/initiative idents}]))
      (into (for [tk idents] [:db/retract id :canvas/initiative tk])
            (for [tk idents attr initiative-attrs] [:db/retract tk attr])))))

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

(defmethod transact :initiative/roll-all
  [data event]
  (let [attrs  [:db/id :element/flags :initiative/roll]
        select [{:root/canvas [{:canvas/initiative attrs}]}]
        result (ds/pull data select [:db/ident :root])
        {{initiative :canvas/initiative} :root/canvas} result]
    (for [{:keys [db/id initiative/roll element/flags]} initiative
          :when (and (nil? roll) (not (contains? flags :player)))]
      [:db/add id :initiative/roll (inc (rand-int 20))])))

(defmethod transact :initiative/reset-rolls
  [data event]
  (let [select     [{:root/canvas [:canvas/initiative]}]
        result     (ds/pull data select [:db/ident :root])
        initiative (-> result :root/canvas :canvas/initiative)]
    (for [{:keys [db/id]} initiative]
      [:db/retract id :initiative/roll])))

(defmethod transact :initiative/change-health
  [data event id f value]
  (let [parsed (.parseFloat js/window value)]
    (if (.isNaN js/Number parsed) []
        (let [{:keys [initiative/health]} (ds/entity data id)]
          [[:db/add id :initiative/health (f health parsed)]]))))

(defmethod transact :initiative/leave
  [data]
  (let [select [{:root/canvas [:db/id :canvas/initiative]}]
        result (ds/pull data select [:db/ident :root])
        {{id :db/id initiative :canvas/initiative} :root/canvas} result]
    (into [[:db/retract id :canvas/initiative]]
          (for [{:keys [db/id]} initiative attr initiative-attrs]
            [:db/retract id attr]))))

(defmethod transact :storage/reset []
  [])

(defmethod transact :interface/toggle-shortcuts
  [_ _ display?]
  [[:db/add [:db/ident :root] :root/shortcuts? display?]])

(defmethod transact :interface/toggle-tooltips
  [_ _ display?]
  [[:db/add [:db/ident :root] :root/tooltips? display?]])

(defmethod transact :interface/change-panel
  [data _ panel]
  [[:db/add [:db/ident :canvas] :panel/curr panel]
   [:db/add [:db/ident :canvas] :panel/prev panel]
   [:db/add [:db/ident :canvas] :panel/collapsed? false]])

(defmethod transact :interface/toggle-panel
  [data event]
  (let [{:keys [panel/collapsed?]} (ds/pull data [:panel/collapsed?] [:db/ident :canvas])]
    [[:db/add [:db/ident :canvas] :panel/collapsed? (not (or collapsed? false))]]))

(defmethod transact :stamp/create
  [data event stamp-data]
  (let [checksum (:image/checksum stamp-data)
        exists?  (ds/entity data [:image/checksum checksum])]
    (if-not exists?
      [(assoc stamp-data :db/id -1)
       [:db/add [:db/ident :root] :root/stamps -1]]
      [])))

(defmethod transact :stamp/remove
  [data event checksum]
  [[:db/retractEntity [:image/checksum checksum]]])
