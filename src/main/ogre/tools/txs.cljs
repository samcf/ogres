(ns ogre.tools.txs
  (:require [datascript.core :as ds]
            [clojure.set :refer [union]]
            [clojure.string :refer [trim]]
            [ogre.tools.geom :refer [normalize within?]]))

(defn round [x n]
  (* (js/Math.round (/ x n)) n))

(def zoom-scales
  [0.15 0.30 0.50 0.75 0.90 1 1.25 1.50 2 3 4])

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

(defmulti transact (fn [{:keys [event]}] event))

(defmethod transact :workspace/create [_ key]
  [[:db/retract [:db/ident :canvas] :db/ident]
   [:db/add [:db/ident :root] :root/canvases -1]
   [:db/add [:db/ident :root] :root/canvas -1]
   {:db/id -1 :db/ident :canvas :entity/key key :element/name ""}])

(defmethod transact :workspace/change
  [_ key]
  [[:db/retract [:db/ident :canvas] :db/ident]
   [:db/add [:entity/key key] :db/ident :canvas]
   [:db/add [:db/ident :root] :root/canvas [:entity/key key]]])

(defmethod transact :workspace/remove
  [{:keys [data]} key new-key]
  (let [result   (ds/pull data '[{:root/canvases [*]}] [:db/ident :root])
        canvas   (ds/pull data '[*] [:db/ident :canvas])
        canvases (:root/canvases result)]
    (cond
      (= (count canvases) 1)
      [[:db.fn/retractEntity [:entity/key key]]
       [:db/add [:db/ident :root] :root/canvases -1]
       [:db/add [:db/ident :root] :root/canvas -1]
       {:db/id -1 :db/ident :canvas :entity/key new-key :element/name ""}]

      (= (:entity/key canvas) key)
      (let [next (-> (set canvases) (disj canvas) (first))]
        [[:db.fn/retractEntity [:entity/key key]]
         [:db/add [:db/ident :root] :root/canvas (:db/id next)]
         [:db/add (:db/id next) :db/ident :canvas]])

      :else
      [[:db.fn/retractEntity [:entity/key key]]])))

(defmethod transact :canvas/change-scene
  [_ checksum]
  [[:db/add [:db/ident :canvas] :canvas/scene [:image/checksum checksum]]
   [:db/add [:db/ident :canvas] :pos/vec [0 0]]])

(defmethod transact :canvas/toggle-mode
  [{:keys [data]} mode]
  (let [ident  [:db/ident :canvas]
        select [:canvas/mode :canvas/selected]
        result (ds/pull data select ident)
        {curr :canvas/mode} result]
    (concat [[:db/add ident :canvas/mode mode]]
            (if (not= mode curr)
              [[:db/retract ident :canvas/selected]]
              [[:db/add ident :canvas/mode :select]]))))

(defmethod transact :canvas/toggle-theme
  [{:keys [data]}]
  (let [ident  [:db/ident :canvas]
        canvas (ds/pull data [:canvas/theme] ident)]
    [[:db/add ident :canvas/theme (if (= (:canvas/theme canvas) :dark) :light :dark)]]))

(defmethod transact :canvas/modifier-start
  [_ modifier]
  [[:db/add [:db/ident :canvas] :canvas/modifier modifier]])

(defmethod transact :canvas/modifier-release []
  [[:db/retract [:db/ident :canvas] :canvas/modifier]])

(defmethod transact :element/update
  [_ keys attr value]
  (for [key keys]
    [:db/add [:entity/key key] attr value]))

(defmethod transact :element/select
  [{:keys [data]} key replace?]
  (let [entity [:entity/key key]
        result (ds/pull data [:canvas/_selected] entity)]
    [(if replace?
       [:db/retract [:db/ident :canvas] :canvas/selected])
     (if (and (not replace?) (:canvas/_selected result))
       [:db/retract [:db/ident :canvas] :canvas/selected entity]
       [:db/add [:db/ident :canvas] :canvas/selected entity])]))

(defmethod transact :element/remove
  [_ keys]
  (for [key keys]
    [:db/retractEntity [:entity/key key]]))

(defmethod transact :element/flag
  [{:keys [data]} keys flag add?]
  (let [idents (map (fn [key] [:entity/key key]) keys)
        tokens (ds/pull-many data [:db/id :element/flags] idents)]
    (for [{:keys [db/id element/flags] :or {flags #{}}} tokens]
      [:db/add id :element/flags ((if add? conj disj) flags flag)])))

(defmethod transact :camera/translate [_ x y]
  [[:db/add [:db/ident :canvas] :pos/vec [(round x 1) (round y 1)]]])

(defmethod transact :scene/create
  [{:keys [data]} checksum filename width height]
  (let [existing (ds/entity data [:image/checksum checksum])]
    (if (nil? existing)
      [[:db/add -1 :image/checksum checksum]
       [:db/add -1 :image/filename filename]
       [:db/add -1 :image/width width]
       [:db/add -1 :image/height height]
       [:db/add [:db/ident :root] :root/scenes -1]
       [:db/add [:db/ident :canvas] :canvas/scene -1]]
      [[:db/add [:db/ident :canvas] :canvas/scene [:image/checksum checksum]]])))

(defmethod transact :map/remove
  [_ checksum]
  [[:db/retractEntity [:image/checksum checksum]]])

(defmethod transact :token/create
  [_ key x y]
  [[:db/add [:db/ident :canvas] :canvas/tokens -1]
   [:db/add [:db/ident :canvas] :canvas/selected -1]
   [:db/add [:db/ident :canvas] :canvas/mode :select]
   {:db/id -1 :entity/key key :pos/vec [x y]}])

(defmethod transact :token/translate
  [{:keys [data]} key x y align?]
  (let [{size :grid/size} (ds/pull data [[:grid/size :default 70]] [:db/ident :canvas])
        radius            (if align? (/ size 2) 1)]
    [[:db/add [:entity/key key] :pos/vec [(round x radius) (round y radius)]]]))

(defmethod transact :token/translate-all
  [{:keys [data]} keys x y align?]
  (let [{size :grid/size} (ds/pull data [[:grid/size :default 70]] [:db/ident :canvas])
        lookup            (map (fn [key] [:entity/key key]) keys)
        tokens            (ds/pull-many data [:db/id :pos/vec] lookup)
        radius            (if align? (/ size 2) 1)]
    (for [{id :db/id [tx ty] :pos/vec} tokens]
      [:db/add id :pos/vec [(round (+ x tx) radius) (round (+ y ty) radius)]])))

(defmethod transact :token/change-label
  [_ keys value]
  (for [key keys]
    [:db/add [:entity/key key] :element/name (trim value)]))

(defmethod transact :token/change-size
  [_ keys radius]
  (for [key keys]
    [:db/add [:entity/key key] :token/size radius]))

(defmethod transact :token/change-light
  [_ keys radius]
  (for [key keys]
    [:db/add [:entity/key key] :token/light radius]))

(defmethod transact :token/change-aura
  [_ keys radius]
  (for [key keys]
    [:db/add [:entity/key key] :aura/radius radius]))

(defmethod transact :token/change-stamp
  [_ keys checksum]
  (for [key keys]
    [:db/add [:entity/key key] :token/stamp [:image/checksum checksum]]))

(defmethod transact :token/remove-stamp
  [_ keys]
  (for [key keys]
    [:db/retract [:entity/key key] :token/stamp]))

(defmethod transact :shape/create
  [_ key kind vecs]
  [[:db/add -1 :entity/key key]
   [:db/add -1 :shape/kind kind]
   [:db/add -1 :shape/vecs vecs]
   [:db/add [:db/ident :canvas] :canvas/shapes -1]
   [:db/add [:db/ident :canvas] :canvas/mode :select]
   [:db/add [:db/ident :canvas] :canvas/selected -1]])

(defn trans-xf [x y]
  (comp (partition-all 2) (drop 1) (map (fn [[ax ay]] [(+ ax x) (+ ay y)])) cat))

(defmethod transact :shape/translate
  [{:keys [data]} key x y align?]
  (let [{[ax ay] :shape/vecs vecs :shape/vecs} (ds/pull data [:shape/vecs] [:entity/key key])
        {size :grid/size} (ds/pull data [[:grid/size :default 70]] [:db/ident :canvas])
        r (if align? (/ size 2) 1)
        x (round x r)
        y (round y r)]
    [[:db/add [:entity/key key] :shape/vecs (into [x y] (trans-xf (- x ax) (- y ay)) vecs)]]))

(defmethod transact :grid/change-size
  [_ size]
  [[:db/add [:db/ident :canvas] :grid/size size]])

(defmethod transact :grid/draw
  [{:keys [data]} key _ _ size]
  (let [lookup (ds/entid data [:entity/key key])
        select [:db/id [:zoom/scale :default 1] {:canvas/scene [:image/width]}]
        result (ds/pull data select lookup)
        {scale :zoom/scale
         scene :canvas/scene
         {width :image/width} :canvas/scene} result
        size (js/Math.round (/ size scale))]
    (if (nil? scene)
      [[:db/add lookup :grid/size size]
       [:db/add lookup :canvas/mode :select]]
      (let [pattern [size (+ size 1) (- size 1) (+ size 2) (- size 2) (+ size 3) (- size 3) (+ size 4) (- size 4)]
            next    (reduce (fn [_ n] (when (zero? (mod width n)) (reduced n))) pattern)]
        [[:db/add lookup :grid/size (or next size)]
         [:db/add lookup :canvas/mode :select]]))))

(defmethod transact :grid/toggle
  [_ key value]
  [[:db/add [:entity/key key] :grid/show value]])

(defmethod transact :grid/align
  [_ key value]
  [[:db/add [:entity/key key] :grid/align value]])

(defmethod transact :canvas/change-visibility
  [_ key value]
  [[:db/add [:entity/key key] :canvas/visibility value]])

(defmethod transact :canvas/change-color
  [_ key value]
  [[:db/add [:entity/key key] :canvas/color value]])

(defmethod transact :zoom/change
  ([context key]
   (transact context key 1))
  ([{:keys [data] :as context} key next]
   (let [result    (ds/pull data [:bounds/self] [:db/ident :root])
         [_ _ w h] (:bounds/self result)]
     (transact context key next (/ w 2) (/ h 2))))
  ([{:keys [data]} key next x y]
   (let [lookup (ds/entid data [:entity/key key])
         select [[:zoom/scale :default 1] [:pos/vec :default [0 0]]]
         result (ds/pull data select lookup)
         {prev :zoom/scale [cx cy] :pos/vec} result
         fx (/ next prev)
         dx (/ (- (* x fx) x) next)
         dy (/ (- (* y fx) y) next)]
     [[:db/add lookup :pos/vec [(round (- cx dx) 1) (round (- cy dy) 1)]]
      [:db/add lookup :zoom/scale next]])))

(defmethod transact :zoom/delta
  [{:keys [data]} key delta x y]
  (let [canvas (ds/pull data [[:zoom/scale :default 1]] [:db/ident :canvas])
        next   (-> (:zoom/scale canvas)
                   (js/Math.log)
                   (+ delta)
                   (js/Math.exp)
                   (to-precision 2)
                   (constrain 0.15 4))]
    (transact {:data data :event :zoom/change} key next x y)))

(defmethod transact :zoom/in [{:keys [data]} key]
  (let [info (ds/pull data [[:zoom/scale :default 1]] [:db/ident :canvas])
        prev (:zoom/scale info)
        next (reduce (fn [n s] (if (> s prev) (reduced s) n)) prev zoom-scales)]
    (transact {:data data :event :zoom/change} key next)))

(defmethod transact :zoom/out [{:keys [data]} key]
  (let [info (ds/pull data [[:zoom/scale :default 1]] [:db/ident :canvas])
        prev (:zoom/scale info)
        next (reduce (fn [n s] (if (< s prev) (reduced s) n)) prev (reverse zoom-scales))]
    (transact {:data data :event :zoom/change} key next)))

(defmethod transact :zoom/reset [{:keys [data]} key]
  (transact {:data data :event :zoom/change} key 1))

(defmethod transact :share/initiate []
  [])

(defmethod transact :share/toggle [{:keys [data]} open?]
  (let [{:keys [root/host?]} (ds/pull data [:root/host?] [:db/ident :root])]
    [[:db/add [:db/ident :root] :share/open? open?]
     [:db/add [:db/ident :root] :share/paused? false]
     [:db/add [:db/ident :root] :root/privileged? (and host? open?)]]))

(defmethod transact :share/switch
  ([{:keys [data] :as context}]
   (let [{:keys [share/paused?]} (ds/pull data [:share/paused?] [:db/ident :root])]
     (transact context (not paused?))))
  ([_ paused?]
   [[:db/add [:db/ident :root] :share/paused? paused?]]))

(defmethod transact :bounds/change
  [{:keys [data]} w-host? bounds]
  (let [{id :db/id v-host? :root/host?} (ds/pull data [:db/id :root/host?] [:db/ident :root])]
    (cond-> []
      (= w-host? v-host?) (conj [:db/add id :bounds/self bounds])
      (= w-host? true)    (conj [:db/add id :bounds/host bounds])
      (= w-host? false)   (conj [:db/add id :bounds/guest bounds]))))

(defmethod transact :selection/from-rect
  [{:keys [data]} vecs]
  (let [selector   [{:root/canvas [:db/id {:canvas/tokens [:db/id :pos/vec]}]}]
        root       (ds/pull data selector [:db/ident :root])
        canvas     (:root/canvas root)
        normalized (normalize vecs)
        selected   (filter
                    (fn [{[x y] :pos/vec}]
                      (within? x y normalized)) (:canvas/tokens canvas))]
    (concat [[:db/add (:db/id canvas) :canvas/mode :select]]
            (for [entity selected]
              [:db/add (:db/id canvas) :canvas/selected (:db/id entity)]))))

(defmethod transact :selection/clear
  [_]
  [[:db/retract [:db/ident :canvas] :canvas/selected]])

(defmethod transact :selection/remove
  [{:keys [data]}]
  (let [select [:canvas/selected]
        result (ds/pull data select [:db/ident :canvas])
        {:keys [canvas/selected]} result]
    (for [entity selected]
      [:db/retractEntity (:db/id entity)])))

(defmethod transact :initiative/toggle
  [{:keys [data]} keys adding?]
  (let [lookup   (map (fn [key] [:entity/key key]) keys)
        select-t [:db/id :element/name :initiative/suffix [:element/flags :default #{}]]
        select-r [{:root/canvas [:db/id {:canvas/initiative select-t}]}]
        result   (ds/pull data select-r [:db/ident :root])
        adding   (ds/pull-many data select-t lookup)
        {{id :db/id exists :canvas/initiative} :root/canvas} result]
    (if adding?
      (->> (union (set exists) (set adding))
           (filter (fn [t] (nil? ((:element/flags t) :player))))
           (suffix-txs)
           (into [{:db/id id :canvas/initiative lookup}]))
      (into (for [tk lookup] [:db/retract id :canvas/initiative tk])
            (for [tk lookup attr initiative-attrs] [:db/retract tk attr])))))

(defmethod transact :initiative/change-roll
  [_ key roll]
  (let [parsed (.parseFloat js/window roll)]
    (cond
      (or (nil? roll) (= roll ""))
      [[:db/retract [:entity/key key] :initiative/roll]]

      (.isNaN js/Number parsed)
      []

      :else
      [[:db/add [:entity/key key] :initiative/roll parsed]])))

(defmethod transact :initiative/roll-all
  [{:keys [data]} key]
  (let [select [{:canvas/initiative [:db/id :element/flags :initiative/roll]}]
        result (ds/pull data select [:entity/key key])
        {initiative :canvas/initiative} result]
    (for [{:keys [db/id initiative/roll element/flags]} initiative
          :when (and (nil? roll) (not (contains? flags :player)))]
      [:db/add id :initiative/roll (inc (rand-int 20))])))

(defmethod transact :initiative/reset-rolls
  [{:keys [data]} key]
  (let [result (ds/pull data [[:canvas/initiative :default #{}]] [:entity/key key])]
    (for [{:keys [db/id]} (:canvas/initiative result)]
      [:db/retract id :initiative/roll])))

(defmethod transact :initiative/change-health
  [{:keys [data]} key f value]
  (let [parsed (.parseFloat js/window value)]
    (if (.isNaN js/Number parsed) []
        (let [{:keys [initiative/health]} (ds/entity data [:entity/key key])]
          [[:db/add [:entity/key key] :initiative/health (f health parsed)]]))))

(defmethod transact :initiative/leave
  [{:keys [data]} key]
  (let [select [:db/id :canvas/initiative]
        result (ds/pull data select [:entity/key key])
        {id :db/id initiative :canvas/initiative} result]
    (into [[:db/retract id :canvas/initiative]]
          (for [{:keys [db/id]} initiative attr initiative-attrs]
            [:db/retract id attr]))))

(defmethod transact :storage/reset [] [])

(defmethod transact :interface/toggle-shortcuts
  [_ display?]
  [[:db/add [:db/ident :root] :root/shortcuts? display?]])

(defmethod transact :interface/toggle-tooltips
  [_ display?]
  [[:db/add [:db/ident :root] :root/tooltips? display?]])

(defmethod transact :interface/change-panel
  [_ panel]
  [[:db/add [:db/ident :canvas] :panel/curr panel]
   [:db/add [:db/ident :canvas] :panel/collapsed? false]])

(defmethod transact :interface/toggle-panel
  [{:keys [data]}]
  (let [{:keys [panel/collapsed?]} (ds/pull data [:panel/collapsed?] [:db/ident :canvas])]
    [[:db/add [:db/ident :canvas] :panel/collapsed? (not (or collapsed? false))]]))

(defmethod transact :stamp/create
  [{:keys [data]} checksum filename width height]
  (let [existing (ds/entity data [:image/checksum checksum])]
    (if (not existing)
      [[:db/add -1 :image/checksum checksum]
       [:db/add -1 :image/filename filename]
       [:db/add -1 :image/width width]
       [:db/add -1 :image/height height]
       [:db/add [:db/ident :root] :root/stamps -1]]
      [])))

(defmethod transact :stamp/remove [_ checksum]
  [[:db/retractEntity [:image/checksum checksum]]])

(defmethod transact :mask/fill [key]
  [[:db/add [:entity/key key] :mask/filled? true]
   [:db/retract [:entity/key key] :canvas/masks]])

(defmethod transact :mask/clear [key]
  [[:db/add [:entity/key key] :mask/filled? false]
   [:db/retract [:entity/key key] :canvas/masks]])

(defmethod transact :mask/create
  [_ canvas key state vecs]
  [[:db/add [:entity/key canvas] :canvas/masks -1]
   [:db/add -1 :entity/key key]
   [:db/add -1 :mask/enabled? state]
   [:db/add -1 :mask/vecs vecs]])

(defmethod transact :mask/toggle
  [_ key state]
  [[:db/add [:entity/key key] :mask/enabled? state]])

(defmethod transact :mask/remove
  [_ key]
  [[:db/retractEntity [:entity/key key]]])
