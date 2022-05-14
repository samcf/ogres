(ns ogre.tools.txs
  (:require [datascript.core :as ds :refer [squuid]]
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
    (fn [m {:keys [entity/key token/label initiative/suffix]}]
      (update
       m label
       (fn [[idents current]]
         [(if (= suffix nil) (conj idents key) idents)
          (max current suffix)]))) {} entities)
   (mapcat
    (fn [[_ [idents suffix]]]
      (if (or (> (count idents) 1) (number? suffix))
        (loop [idents idents suffix (inc suffix) txs []]
          (if-let [key (first idents)]
            (recur
             (rest idents)
             (inc suffix)
             (conj txs [:db/add [:entity/key key] :initiative/suffix suffix]))
            txs)) [])))))

(def initiative-attrs
  #{:initiative/roll
    :initiative/health
    :initiative/suffix})

(defmulti transact (fn [{:keys [event]}] event))

(defmethod transact :workspace/create
  [{:keys [local]}]
  [[:db/add -1 :entity/key (squuid)]
   [:db/add -1 :window/canvas -2]
   [:db/add -2 :entity/key (squuid)]
   [:db/add [:entity/key local] :local/window -1]
   [:db/add [:entity/key local] :local/windows -1]
   [:db/add [:db/ident :root] :root/canvases -2]])

(defmethod transact :workspace/change
  [{:keys [local]} key]
  [[:db/add [:entity/key local] :local/window [:entity/key key]]])

(defmethod transact :workspace/remove
  [{:keys [data local]} key]
  (let [lookup [:entity/key local]
        select [{:local/window [:entity/key]} {:local/windows [:entity/key]}]
        result (ds/pull data select lookup)
        window (ds/pull data [:entity/key {:window/canvas [:entity/key]}] [:entity/key key])
        {current :local/window
         windows :local/windows} result]
    (cond
      (= (count windows) 1)
      [[:db/retractEntity [:entity/key key]]
       [:db/retractEntity [:entity/key (:entity/key (:window/canvas window))]]
       [:db/add -1 :entity/key (squuid)]
       [:db/add -1 :window/canvas -2]
       [:db/add -2 :entity/key (squuid)]
       [:db/add lookup :local/window -1]
       [:db/add lookup :local/windows -1]]

      (= (:entity/key window) (:entity/key current))
      (let [next (first (disj (set windows) current))]
        [[:db/retractEntity [:entity/key key]]
         [:db/retractEntity [:entity/key (:entity/key (:window/canvas window))]]
         [:db/add lookup :local/window [:entity/key (:entity/key next)]]])

      :else
      [[:db/retractEntity [:entity/key key]]
       [:db/retractEntity [:entity/key (:entity/key (:window/canvas window))]]])))

(defmethod transact :window/change-label
  [{:keys [window]} label]
  [[:db/add [:entity/key window] :window/label label]])

(defmethod transact :window/translate
  [{:keys [window]} x y]
  [[:db/add [:entity/key window] :window/vec [(round x 1) (round y 1)]]])

(defmethod transact :window/change-mode
  [{:keys [data window]} mode]
  (let [lookup [:entity/key window]
        entity (ds/entity data lookup)]
    (concat [[:db/add lookup :window/draw-mode mode]]
            (if (not= mode (:window/draw-mode entity))
              [[:db/retract lookup :window/selected]]
              [[:db/add lookup :window/draw-mode :select]]))))

(defmethod transact :window/modifier-start
  [{:keys [window]} modifier]
  [[:db/add [:entity/key window] :window/modifier modifier]])

(defmethod transact :window/modifier-release
  [{:keys [window]}]
  [[:db/retract [:entity/key window] :window/modifier]])

(defmethod transact :window/change-grid-show
  [{:keys [window]} value]
  [[:db/add [:entity/key window] :window/show-grid value]])

(defmethod transact :window/change-grid-snap
  [{:keys [window]} value]
  [[:db/add [:entity/key window] :window/snap-grid value]])

(defmethod transact :zoom/change
  ([context]
   (transact context 1))
  ([{:keys [data local] :as context} next]
   (let [select    [[:bounds/self :default [0 0 0 0]]]
         result    (ds/pull data select [:entity/key local])
         [_ _ w h] (:bounds/self result)]
     (transact context next (/ w 2) (/ h 2))))
  ([{:keys [data window]} next x y]
   (let [select [[:window/scale :default 1] [:window/vec :default [0 0]]]
         result (ds/pull data select [:entity/key window])
         {prev :window/scale [cx cy] :window/vec} result
         fx (/ next prev)
         dx (/ (- (* x fx) x) next)
         dy (/ (- (* y fx) y) next)]
     [[:db/add [:entity/key window] :window/vec [(round (- cx dx) 1) (round (- cy dy) 1)]]
      [:db/add [:entity/key window] :window/scale next]])))

(defmethod transact :zoom/delta
  [{:keys [data window] :as context} delta x y]
  (let [result (ds/pull data [[:window/scale :default 1]] [:entity/key window])
        next   (-> (:window/scale result)
                   (js/Math.log)
                   (+ delta)
                   (js/Math.exp)
                   (to-precision 2)
                   (constrain 0.15 4))]
    (transact (assoc context :event :zoom/change) next x y)))

(defmethod transact :zoom/in
  [{:keys [data window] :as context}]
  (let [info (ds/pull data [[:window/scale :default 1]] [:entity/key window])
        prev (:window/scale info)
        next (reduce (fn [n s] (if (> s prev) (reduced s) n)) prev zoom-scales)]
    (transact (assoc context :event :zoom/change) next)))

(defmethod transact :zoom/out
  [{:keys [data window] :as context}]
  (let [info (ds/pull data [[:window/scale :default 1]] [:entity/key window])
        prev (:window/scale info)
        next (reduce (fn [n s] (if (< s prev) (reduced s) n)) prev (reverse zoom-scales))]
    (transact (assoc context :event :zoom/change) next)))

(defmethod transact :zoom/reset [context]
  (transact (assoc context :event :zoom/change) 1))

(defmethod transact :canvas/change-scene
  [{:keys [canvas]} checksum]
  [[:db/add [:entity/key canvas] :canvas/image [:image/checksum checksum]]])

(defmethod transact :canvas/change-grid-size
  [{:keys [canvas]} size]
  [[:db/add [:entity/key canvas] :grid/size size]])

(defmethod transact :canvas/draw-grid-size
  [{:keys [data window canvas]} _ _ size]
  (let [lookup [:entity/key window]
        select [[:window/scale :default 1] {:window/canvas [{:canvas/image [:image/width]}]}]
        result (ds/pull data select lookup)
        {scale :window/scale
         {scene :canvas/image
          {width :image/width} :canvas/image} :window/canvas} result
        size (js/Math.round (/ size scale))]
    (if (nil? scene)
      [[:db/add [:entity/key canvas] :grid/size size]
       [:db/add [:entity/key window] :window/draw-mode :select]]
      (let [pattern [size (+ size 1) (- size 1) (+ size 2) (- size 2) (+ size 3) (- size 3) (+ size 4) (- size 4)]
            next    (reduce (fn [_ n] (when (zero? (mod width n)) (reduced n))) pattern)]
        [[:db/add [:entity/key canvas] :grid/size (or next size)]
         [:db/add [:entity/key window] :window/draw-mode :select]]))))

(defmethod transact :canvas/change-theme
  [{:keys [canvas]} theme]
  [[:db/add [:entity/key canvas] :canvas/theme theme]])

(defmethod transact :canvas/change-visibility
  [{:keys [canvas]} value]
  [[:db/add [:entity/key canvas] :canvas/visibility value]])

(defmethod transact :canvas/change-color
  [{:keys [canvas]} value]
  [[:db/add [:entity/key canvas] :canvas/color value]])

(defmethod transact :element/update
  [_ keys attr value]
  (for [key keys]
    [:db/add [:entity/key key] attr value]))

(defmethod transact :element/select
  [{:keys [data window]} key replace?]
  (let [lookup [:entity/key key]
        entity (ds/entity data lookup)]
    [(if replace?
       [:db/retract [:entity/key window] :window/selected])
     (if (and (not replace?) (:window/_selected entity))
       [:db/retract [:entity/key window] :window/selected lookup]
       [:db/add [:entity/key window] :window/selected lookup])]))

(defmethod transact :element/remove
  [_ keys]
  (for [key keys]
    [:db/retractEntity [:entity/key key]]))

(defmethod transact :scene/create
  [{:keys [data canvas]} checksum filename width height]
  (let [lookup   [:image/checksum checksum]
        existing (ds/entity data lookup)]
    (if (nil? existing)
      [[:db/add -1 :image/checksum checksum]
       [:db/add -1 :image/filename filename]
       [:db/add -1 :image/width width]
       [:db/add -1 :image/height height]
       [:db/add [:db/ident :root] :root/scenes -1]
       [:db/add [:entity/key canvas] :canvas/image -1]]
      [[:db/add [:entity/key canvas] :canvas/image lookup]])))

(defmethod transact :map/remove
  [_ checksum]
  [[:db/retractEntity [:image/checksum checksum]]])

(defmethod transact :token/create
  [{:keys [window canvas]} x y]
  [[:db/add [:entity/key canvas] :canvas/tokens -1]
   [:db/add [:entity/key window] :window/selected -1]
   [:db/add [:entity/key window] :window/draw-mode :select]
   {:db/id -1 :entity/key (squuid) :token/vec [x y]}])

(defmethod transact :token/translate
  [{:keys [data canvas]} key x y align?]
  (let [{size :grid/size} (ds/pull data [[:grid/size :default 70]] [:entity/key canvas])
        radius            (if align? (/ size 2) 1)]
    [[:db/add [:entity/key key] :token/vec [(round x radius) (round y radius)]]]))

(defmethod transact :token/change-flag
  [{:keys [data]} keys flag add?]
  (let [idents (map (fn [key] [:entity/key key]) keys)
        tokens (ds/pull-many data [:entity/key :token/flags] idents)]
    (for [{:keys [entity/key token/flags] :or {flags #{}}} tokens]
      [:db/add [:entity/key key] :token/flags ((if add? conj disj) flags flag)])))

(defmethod transact :token/translate-all
  [{:keys [data canvas]} keys x y align?]
  (let [{size :grid/size} (ds/pull data [[:grid/size :default 70]] [:entity/key canvas])
        lookup            (map (fn [key] [:entity/key key]) keys)
        tokens            (ds/pull-many data [:entity/key :token/vec] lookup)
        radius            (if align? (/ size 2) 1)]
    (for [{key :entity/key [tx ty] :token/vec} tokens]
      [:db/add [:entity/key key] :token/vec [(round (+ x tx) radius) (round (+ y ty) radius)]])))

(defmethod transact :token/change-label
  [_ keys value]
  (for [key keys]
    [:db/add [:entity/key key] :token/label (trim value)]))

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
    [:db/add [:entity/key key] :token/image [:image/checksum checksum]]))

(defmethod transact :token/remove-stamp
  [_ keys]
  (for [key keys]
    [:db/retract [:entity/key key] :token/image]))

(defmethod transact :shape/create
  [{:keys [window canvas]} kind vecs]
  [[:db/add -1 :entity/key (squuid)]
   [:db/add -1 :shape/kind kind]
   [:db/add -1 :shape/vecs vecs]
   [:db/add [:entity/key canvas] :canvas/shapes -1]
   [:db/add [:entity/key window] :window/draw-mode :select]
   [:db/add [:entity/key window] :window/selected -1]])

(defn trans-xf [x y]
  (comp (partition-all 2) (drop 1) (map (fn [[ax ay]] [(+ ax x) (+ ay y)])) cat))

(defmethod transact :shape/translate
  [{:keys [data canvas]} key x y align?]
  (let [{[ax ay] :shape/vecs vecs :shape/vecs} (ds/pull data [:shape/vecs] [:entity/key key])
        {size :grid/size} (ds/pull data [[:grid/size :default 70]] [:entity/key canvas])
        r (if align? (/ size 2) 1)
        x (round x r)
        y (round y r)]
    [[:db/add [:entity/key key] :shape/vecs (into [x y] (trans-xf (- x ax) (- y ay)) vecs)]]))

(defmethod transact :share/initiate [] [])

(defmethod transact :share/toggle [{:keys [data local]} open?]
  (let [{:keys [local/host?]} (ds/pull data [:local/host?] [:db/ident :local])]
    [[:db/add [:entity/key local] :local/sharing? open?]
     [:db/add [:entity/key local] :local/paused? false]
     [:db/add [:entity/key local] :local/privileged? (and host? open?)]]))

(defmethod transact :share/switch
  ([{:keys [data local] :as context}]
   (let [{:keys [local/paused?]} (ds/pull data [:local/paused?] [:entity/key local])]
     (transact context (not paused?))))
  ([{:keys [local]} paused?]
   [[:db/add [:entity/key local] :local/paused? paused?]]))

(defmethod transact :bounds/change
  [{:keys [data local]} w-host? bounds]
  (let [{v-host? :local/host?} (ds/pull data [:local/host?] [:db/ident :local])]
    (cond-> []
      (= w-host? v-host?) (conj [:db/add [:entity/key local] :bounds/self bounds])
      (= w-host? true)    (conj [:db/add [:entity/key local] :bounds/host bounds])
      (= w-host? false)   (conj [:db/add [:entity/key local] :bounds/guest bounds]))))

(defmethod transact :selection/from-rect
  [{:keys [data window canvas]} vecs]
  (let [result (ds/pull data [{:canvas/tokens [:entity/key :token/vec]}] [:entity/key canvas])
        normal (normalize vecs)
        select (filter
                (fn [{[x y] :token/vec}]
                  (within? x y normal)) (:canvas/tokens result))]
    (concat [[:db/add [:entity/key window] :window/draw-mode :select]]
            (for [{:keys [entity/key]} select :let [ref [:entity/key key]]]
              [:db/add [:entity/key window] :window/selected ref]))))

(defmethod transact :selection/clear
  [{:keys [window]}]
  [[:db/retract [:entity/key window] :window/selected]])

(defmethod transact :selection/remove
  [{:keys [data window]}]
  (let [result (ds/pull data [{:window/selected [:entity/key]}] [:entity/key window])]
    (for [{:keys [entity/key]} (:window/selected result)]
      [:db/retractEntity [:entity/key key]])))

(defmethod transact :initiative/toggle
  [{:keys [data canvas]} keys adding?]
  (let [tokens   (map (fn [key] [:entity/key key]) keys)
        select-t [:entity/key :token/label :initiative/suffix [:token/flags :default #{}]]
        select-r [:entity/key {:canvas/initiative select-t}]
        result   (ds/pull data select-r [:entity/key canvas])
        adding   (ds/pull-many data select-t tokens)
        {key :entity/key exists :canvas/initiative} result]
    (if adding?
      (->> (union (set exists) (set adding))
           (filter (fn [t] (nil? ((:token/flags t) :player))))
           (suffix-txs)
           (into [{:db/id [:entity/key canvas] :canvas/initiative tokens}]))
      (into (for [tk tokens] [:db/retract [:entity/key key] :canvas/initiative tk])
            (for [tk tokens attr initiative-attrs] [:db/retract tk attr])))))

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
  [{:keys [data canvas]}]
  (let [select [{:canvas/initiative [:entity/key :token/flags :initiative/roll]}]
        result (ds/pull data select [:entity/key canvas])
        {initiative :canvas/initiative} result]
    (for [{:keys [entity/key initiative/roll token/flags]} initiative
          :when (and (nil? roll) (not (contains? flags :player)))]
      [:db/add [:entity/key key] :initiative/roll (inc (rand-int 20))])))

(defmethod transact :initiative/reset-rolls
  [{:keys [data canvas]}]
  (let [result (ds/pull data [{:canvas/initiative [:entity/key]}] [:entity/key canvas])]
    (for [{:keys [entity/key]} (:canvas/initiative result)]
      [:db/retract [:entity/key key] :initiative/roll])))

(defmethod transact :initiative/change-health
  [{:keys [data]} key f value]
  (let [parsed (.parseFloat js/window value)]
    (if (.isNaN js/Number parsed) []
        (let [{:keys [initiative/health]} (ds/entity data [:entity/key key])]
          [[:db/add [:entity/key key] :initiative/health (f health parsed)]]))))

(defmethod transact :initiative/leave
  [{:keys [data canvas]}]
  (let [select [{:canvas/initiative [:entity/key]}]
        result (ds/pull data select [:entity/key canvas])
        {initiative :canvas/initiative} result]
    (into [[:db/retract [:entity/key canvas] :canvas/initiative]]
          (for [{:keys [entity/key]} initiative attr initiative-attrs]
            [:db/retract [:entity/key key] attr]))))

(defmethod transact :storage/reset [] [])

(defmethod transact :interface/toggle-shortcuts
  [{:keys [local]} display?]
  [[:db/add [:entity/key local] :local/shortcuts? display?]])

(defmethod transact :interface/toggle-tooltips
  [{:keys [local]} display?]
  [[:db/add [:entity/key local] :local/tooltips? display?]])

(defmethod transact :interface/change-panel
  [{:keys [window]} panel]
  [[:db/add [:entity/key window] :panel/current panel]
   [:db/add [:entity/key window] :panel/collapsed? false]])

(defmethod transact :interface/toggle-panel
  [{:keys [data window]}]
  (let [entity (ds/entity data [:entity/key window])]
    [[:db/add [:entity/key window] :panel/collapsed? (not (:panel/collapsed? entity))]]))

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

(defmethod transact :mask/fill
  [{:keys [canvas]}]
  [[:db/add [:entity/key canvas] :mask/filled? true]
   [:db/retract [:entity/key canvas] :canvas/masks]])

(defmethod transact :mask/clear
  [{:keys [canvas]}]
  [[:db/add [:entity/key canvas] :mask/filled? false]
   [:db/retract [:entity/key canvas] :canvas/masks]])

(defmethod transact :mask/create
  [{:keys [canvas]} state vecs]
  [[:db/add [:entity/key canvas] :canvas/masks -1]
   [:db/add -1 :entity/key (squuid)]
   [:db/add -1 :mask/enabled? state]
   [:db/add -1 :mask/vecs vecs]])

(defmethod transact :mask/toggle
  [_ mask state]
  [[:db/add [:entity/key mask] :mask/enabled? state]])

(defmethod transact :mask/remove
  [_ mask]
  [[:db/retractEntity [:entity/key mask]]])
