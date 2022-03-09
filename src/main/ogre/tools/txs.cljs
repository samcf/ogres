(ns ogre.tools.txs
  (:require [datascript.core :as ds]
            [clojure.set :refer [union]]
            [ogre.tools.geom :refer [normalize within?]]
            [ogre.tools.vec :as vec]))

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

(defmethod transact :workspace/create []
  [[:db/retract [:db/ident :canvas] :db/ident]
   [:db/add [:db/ident :root] :root/canvases -1]
   [:db/add [:db/ident :root] :root/canvas -1]
   {:db/id -1 :db/ident :canvas :element/name ""}])

(defmethod transact :workspace/change
  [_ id]
  [[:db/retract [:db/ident :canvas] :db/ident]
   [:db/add id :db/ident :canvas]
   [:db/add [:db/ident :root] :root/canvas id]])

(defmethod transact :workspace/remove
  [{:keys [data]} id]
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
  [_ scene]
  [[:db/add [:db/ident :canvas] :canvas/scene scene]
   [:db/add [:db/ident :canvas] :pos/vec [0 0]]])

(defmethod transact :canvas/toggle-mode
  [{:keys [data]} mode]
  (let [ident  [:db/ident :canvas]
        select [:db/id :canvas/mode :canvas/selected :panel/prev]
        result (ds/pull data select ident)
        {curr :canvas/mode
         prev :panel/prev} result]
    (concat [[:db/add ident :canvas/mode mode]]
            (if (not= mode curr)
              [[:db/retract ident :canvas/selected]
               [:db/add ident :panel/curr (or prev :canvas)]]
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
  [_ idents attr value]
  (for [id idents]
    [:db/add id attr value]))

(defmethod transact :element/select
  ([context element]
   (transact context element true))
  ([{:keys [data]} element replace?]
   (let [select [:element/type :canvas/_selected]
         result (ds/pull data select element)
         tx-fn  (if (:canvas/_selected result) :db/retract :db/add)]
     [(if replace?
        [:db/retract [:db/ident :canvas] :canvas/selected])
      [tx-fn [:db/ident :canvas] :canvas/selected element]
      [:db/add [:db/ident :canvas] :panel/curr (:element/type result)]])))

(defmethod transact :element/remove
  [{:keys [data]} idents]
  (let [{:keys [panel/prev]} (ds/pull data [[:panel/prev :default :canvas]] [:db/ident :canvas])]
    (into [[:db/add [:db/ident :canvas] :panel/curr prev]]
          (for [id idents] [:db/retractEntity id]))))

(defmethod transact :element/flag
  [{:keys [data]} idents flag add?]
  (let [tokens (ds/pull-many data [:db/id :element/flags] idents)]
    (for [{:keys [db/id element/flags] :or {flags #{}}} tokens]
      [:db/add id :element/flags ((if add? conj disj) flags flag)])))

(defmethod transact :camera/translate
  [_ dst]
  [[:db/add [:db/ident :canvas] :pos/vec (vec/r 1 dst)]])

(defmethod transact :scene/create
  [{:keys [data]} map-data]
  (let [checksum (:image/checksum map-data)
        existing (ds/entity data [:image/checksum checksum])]
    (if (nil? existing)
      [(assoc map-data :db/id -1)
       [:db/add [:db/ident :root] :root/scenes -1]
       [:db/add [:db/ident :canvas] :canvas/scene -1]]
      [[:db/add [:db/ident :canvas] :canvas/scene [:image/checksum checksum]]])))

(defmethod transact :map/remove
  [_ map]
  [[:db/retractEntity map]])

(defmethod transact :token/create
  [_ vector]
  [[:db/add [:db/ident :canvas] :canvas/tokens -1]
   [:db/add [:db/ident :canvas] :canvas/selected -1]
   [:db/add [:db/ident :canvas] :canvas/mode :select]
   [:db/add [:db/ident :canvas] :panel/curr :token]
   {:db/id -1 :element/type :token :pos/vec vector}])

(defmethod transact :token/translate
  [{:keys [data]} id dst align?]
  (let [{size :grid/size} (ds/pull data [[:grid/size :default 70]] [:db/ident :canvas])]
    (if align?
      [[:db/add id :pos/vec (vec/r (/ size 2) dst)]]
      [[:db/add id :pos/vec (vec/r 1 dst)]])))

(defmethod transact :token/translate-all
  [{:keys [data]} idents dst align?]
  (let [{size :grid/size} (ds/pull data [[:grid/size :default 70]] [:db/ident :canvas])
        tokens            (ds/pull-many data [:db/id :pos/vec] idents)]
    (for [{id :db/id src :pos/vec} tokens]
      (if align?
        [:db/add id :pos/vec (->> src (vec/+ dst) (vec/r (/ size 2)))]
        [:db/add id :pos/vec (->> src (vec/+ dst) (vec/r 1))]))))

(defmethod transact :token/change-light
  [_ idents radius]
  (for [id idents]
    [:db/add id :token/light radius]))

(defmethod transact :token/change-size
  [_ idents name size]
  (for [id idents]
    [:db/add id :token/size {:name name :size size}]))

(defmethod transact :token/change-stamp
  [_ idents checksum]
  (for [id idents]
    [:db/add id :token/stamp [:image/checksum checksum]]))

(defmethod transact :token/remove-stamp
  [_ idents]
  (for [id idents]
    [:db/retract id :token/stamp]))

(defmethod transact :shape/create
  [_ kind vecs]
  [[:db/add -1 :element/type :shape]
   [:db/add -1 :shape/kind kind]
   [:db/add -1 :shape/vecs vecs]
   [:db/add [:db/ident :canvas] :canvas/shapes -1]
   [:db/add [:db/ident :canvas] :canvas/mode :select]
   [:db/add [:db/ident :canvas] :canvas/selected -1]
   [:db/add [:db/ident :canvas] :panel/curr :shape]])

(defn trans-xf [x y]
  (comp (partition-all 2) (drop 1) (map (fn [[ax ay]] [(+ ax x) (+ ay y)])) cat))

(defmethod transact :shape/translate
  [{:keys [data]} id x y align?]
  (let [{[ax ay] :shape/vecs vecs :shape/vecs} (ds/pull data [:shape/vecs] id)
        {size :grid/size} (ds/pull data [[:grid/size :default 70]] [:db/ident :canvas])
        [x y] (if align? (vec/r (/ size 2) [x y]) [x y])]
    [[:db/add id :shape/vecs (into [x y] (trans-xf (- x ax) (- y ay)) vecs)]]))

(defmethod transact :grid/change-size
  [_ size]
  [[:db/add [:db/ident :canvas] :grid/size size]])

(defmethod transact :grid/draw
  [{:keys [data]} _ _ size]
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
  [{:keys [data]}]
  (let [{show? :grid/show} (ds/pull data [[:grid/show :default true]] [:db/ident :canvas])]
    [[:db/add [:db/ident :canvas] :grid/show (not show?)]]))

(defmethod transact :grid/align
  [{:keys [data]}]
  (let [{align? :grid/align} (ds/pull data [[:grid/align :default false]] [:db/ident :canvas])]
    [[:db/add [:db/ident :canvas] :grid/align (not align?)]]))

(defmethod transact :canvas/change-visibility
  [_ type]
  [[:db/add [:db/ident :canvas] :canvas/visibility type]])

(defmethod transact :canvas/change-color
  [_ color]
  [[:db/add [:db/ident :canvas] :canvas/color color]])

(defmethod transact :zoom/change
  ([context]
   (transact context 1))
  ([{:keys [data] :as context} next]
   (let [result    (ds/pull data [:bounds/self] [:db/ident :root])
         [_ _ w h] (:bounds/self result)]
     (transact context next (/ w 2) (/ h 2))))
  ([{:keys [data]} next x y]
   (let [select [[:zoom/scale :default 1] [:pos/vec :default [0 0]]]
         result (ds/pull data select [:db/ident :canvas])
         {prev :zoom/scale [cx cy] :pos/vec} result
         fx (/ next prev)
         dx (/ (- (* x fx) x) next)
         dy (/ (- (* y fx) y) next)]
     [[:db/add [:db/ident :canvas] :pos/vec (vec/r 1 [(- cx dx) (- cy dy)])]
      [:db/add [:db/ident :canvas] :zoom/scale next]])))

(defmethod transact :zoom/delta
  [{:keys [data]} delta x y]
  (let [canvas (ds/pull data [[:zoom/scale :default 1]] [:db/ident :canvas])
        next   (-> (:zoom/scale canvas)
                   (js/Math.log)
                   (+ delta)
                   (js/Math.exp)
                   (to-precision 2)
                   (constrain 0.15 4))]
    (transact {:data data :event :zoom/change} next x y)))

(defmethod transact :zoom/in [{:keys [data]}]
  (let [info (ds/pull data [[:zoom/scale :default 1]] [:db/ident :canvas])
        prev (:zoom/scale info)
        next (reduce (fn [n s] (if (> s prev) (reduced s) n)) prev zoom-scales)]
    (transact {:data data :event :zoom/change} next)))

(defmethod transact :zoom/out [{:keys [data]}]
  (let [info (ds/pull data [[:zoom/scale :default 1]] [:db/ident :canvas])
        prev (:zoom/scale info)
        next (reduce (fn [n s] (if (< s prev) (reduced s) n)) prev (reverse zoom-scales))]
    (transact {:data data :event :zoom/change} next)))

(defmethod transact :zoom/reset [{:keys [data]}]
  (transact {:data data :event :zoom/change} 1))

(defmethod transact :aura/change-label
  [_ idents label]
  (for [id idents]
    [:db/add id :aura/label label]))

(defmethod transact :aura/change-radius
  [_ idents radius]
  (for [id idents]
    [:db/add id :aura/radius radius]))

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
      (= w-host? true) (conj [:db/add id :bounds/host bounds])
      (= w-host? false) (conj [:db/add id :bounds/guest bounds]))))

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
            (if (seq selected)
              [[:db/add (:db/id canvas) :panel/curr :token]])
            (for [entity selected]
              [:db/add (:db/id canvas) :canvas/selected (:db/id entity)]))))

(defmethod transact :selection/clear
  [{:keys [data]}]
  (let [{:keys [panel/prev]} (ds/pull data [:panel/prev] [:db/ident :canvas])]
    [[:db/retract [:db/ident :canvas] :canvas/selected]
     [:db/add [:db/ident :canvas] :panel/curr (or prev :canvas)]]))

(defmethod transact :selection/remove
  [{:keys [data]}]
  (let [select [:canvas/selected :panel/prev]
        result (ds/pull data select [:db/ident :canvas])
        {:keys [canvas/selected panel/prev]} result]
    (into [[:db/add [:db/ident :canvas] :panel/curr (or prev :canvas)]]
          (for [entity selected]
            [:db/retractEntity (:db/id entity)]))))

(defmethod transact :initiative/toggle
  [{:keys [data]} idents adding?]
  (let [select-t [:db/id :element/name :initiative/suffix [:element/flags :default #{}]]
        select-r [{:root/canvas [:db/id {:canvas/initiative select-t}]}]
        result   (ds/pull data select-r [:db/ident :root])
        adding   (ds/pull-many data select-t idents)
        {{id :db/id exists :canvas/initiative} :root/canvas} result]
    (if adding?
      (->> (union (set exists) (set adding))
           (filter (fn [t] (nil? ((:element/flags t) :player))))
           (suffix-txs)
           (into [{:db/id id :canvas/initiative idents}]))
      (into (for [tk idents] [:db/retract id :canvas/initiative tk])
            (for [tk idents attr initiative-attrs] [:db/retract tk attr])))))

(defmethod transact :initiative/change-roll
  [_ id roll]
  (let [parsed (.parseFloat js/window roll)]
    (cond
      (or (nil? roll) (= roll ""))
      [[:db/retract id :initiative/roll]]

      (.isNaN js/Number parsed)
      []

      :else
      [[:db/add id :initiative/roll parsed]])))

(defmethod transact :initiative/roll-all
  [{:keys [data]}]
  (let [attrs  [:db/id :element/flags :initiative/roll]
        select [{:root/canvas [{:canvas/initiative attrs}]}]
        result (ds/pull data select [:db/ident :root])
        {{initiative :canvas/initiative} :root/canvas} result]
    (for [{:keys [db/id initiative/roll element/flags]} initiative
          :when (and (nil? roll) (not (contains? flags :player)))]
      [:db/add id :initiative/roll (inc (rand-int 20))])))

(defmethod transact :initiative/reset-rolls
  [{:keys [data]}]
  (let [select     [{:root/canvas [:canvas/initiative]}]
        result     (ds/pull data select [:db/ident :root])
        initiative (-> result :root/canvas :canvas/initiative)]
    (for [{:keys [db/id]} initiative]
      [:db/retract id :initiative/roll])))

(defmethod transact :initiative/change-health
  [{:keys [data]} id f value]
  (let [parsed (.parseFloat js/window value)]
    (if (.isNaN js/Number parsed) []
        (let [{:keys [initiative/health]} (ds/entity data id)]
          [[:db/add id :initiative/health (f health parsed)]]))))

(defmethod transact :initiative/leave
  [{:keys [data]}]
  (let [select [{:root/canvas [:db/id :canvas/initiative]}]
        result (ds/pull data select [:db/ident :root])
        {{id :db/id initiative :canvas/initiative} :root/canvas} result]
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
   [:db/add [:db/ident :canvas] :panel/prev panel]
   [:db/add [:db/ident :canvas] :panel/collapsed? false]])

(defmethod transact :interface/toggle-panel
  [{:keys [data]}]
  (let [{:keys [panel/collapsed?]} (ds/pull data [:panel/collapsed?] [:db/ident :canvas])]
    [[:db/add [:db/ident :canvas] :panel/collapsed? (not (or collapsed? false))]]))

(defmethod transact :stamp/create
  [{:keys [data]} stamp-data]
  (let [checksum (:image/checksum stamp-data)
        exists?  (ds/entity data [:image/checksum checksum])]
    (if-not exists?
      [(assoc stamp-data :db/id -1)
       [:db/add [:db/ident :root] :root/stamps -1]]
      [])))

(defmethod transact :stamp/remove [_ checksum]
  [[:db/retractEntity [:image/checksum checksum]]])

(defmethod transact :mask/fill []
  [[:db/add [:db/ident :canvas] :mask/filled? true]
   [:db/retract [:db/ident :canvas] :canvas/masks]])

(defmethod transact :mask/clear []
  [[:db/add [:db/ident :canvas] :mask/filled? false]
   [:db/retract [:db/ident :canvas] :canvas/masks]])

(defmethod transact :mask/create
  [_ state vecs]
  [[:db/add -1 :mask/enabled? state]
   [:db/add -1 :mask/vecs vecs]
   [:db/add [:db/ident :canvas] :canvas/masks -1]])

(defmethod transact :mask/toggle
  [_ id state]
  [[:db/add id :mask/enabled? state]])

(defmethod transact :mask/remove
  [_ id]
  [[:db/retract [:db/ident :canvas] :canvas/masks id]])
