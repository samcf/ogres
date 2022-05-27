(ns ogre.tools.txs
  (:require [datascript.core :as ds :refer [squuid]]
            [clojure.set :refer [union]]
            [clojure.string :refer [trim]]
            [ogre.tools.geom :refer [normalize within?]]))

(def suffix-max-xf
  (map (fn [[label tokens]] [label (apply max (map :initiative/suffix tokens))])))

(defn indexed
  "Returns a transducer which decorates each element with a decreasing
   negative index suitable for use as temporary ids in a DataScript
   transaction. Optionally receives an offset integer to begin counting."
  ([] (indexed 1))
  ([offset] (map-indexed (fn [idx val] [(* -1 (+ idx offset)) val]))))

(defn suffixes
  "Returns a map of `{entity key => suffix}` for the given token entities.
   Each suffix represents a unique and stable identity for a token within
   the group of tokens by which it shares a label. Suffixes are intended to
   help decorate tokens that may otherwise be difficult to distinguish
   when they share the same image and label."
  [tokens]
  (let [groups (group-by :token/label tokens)
        offset (into {} suffix-max-xf groups)]
    (loop [tokens tokens index {} result {}]
      (if (seq tokens)
        (let [token (first tokens) {label :token/label} token]
          (if (or (= (count (groups label)) 1)
                  (:initiative/suffix token)
                  (contains? (:token/flags token) :player))
            (recur (rest tokens) index result)
            (recur (rest tokens)
                   (update index label inc)
                   (assoc result (:entity/key token) (+ (offset label) (index label) 1)))))
        result))))

(defn round [x n]
  (* (js/Math.round (/ x n)) n))

(def zoom-scales
  [0.15 0.30 0.50 0.75 0.90 1 1.25 1.50 2 3 4])

(defn to-precision [n p]
  (js/Number (.toFixed (js/Number.parseFloat n) p)))

(defn constrain [n min max]
  (clojure.core/max (clojure.core/min n max) min))

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
  [[:db/add -1 :entity/key window]
   [:db/add -1 :window/vec [(round x 1) (round y 1)]]])

(defmethod transact :window/change-mode
  [{:keys [data window]} mode]
  (let [entity (ds/entity data [:entity/key window])]
    [[:db/add -1 :entity/key window]
     [:db/add -1 :window/draw-mode mode]
     (if (= mode (:window/draw-mode entity))
       [:db/add -1 :window/draw-mode :select]
       [:db/retract [:entity/key window] :window/selected])]))

(defmethod transact :window/modifier-start
  [{:keys [window]} modifier]
  [[:db/add -1 :entity/key window]
   [:db/add -1 :window/modifier modifier]])

(defmethod transact :window/modifier-release
  [{:keys [window]}]
  [[:db/add -1 :entity/key window]
   [:db/retract [:entity/key window] :window/modifier]])

(defmethod transact :window/change-grid-show
  [{:keys [window]} value]
  [[:db/add -1 :entity/key window]
   [:db/add -1 :window/show-grid value]])

(defmethod transact :window/change-grid-snap
  [{:keys [window]} value]
  [[:db/add -1 :entity/key window]
   [:db/add -1 :window/snap-grid value]])

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
     [[:db/add -1 :entity/key window]
      [:db/add -1 :window/vec [(round (- cx dx) 1) (round (- cy dy) 1)]]
      [:db/add -1 :window/scale next]])))

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
  [{:keys [data canvas]} checksum]
  (let [{:image/keys [width height filename]} (ds/entity data [:image/checksum checksum])]
    [[:db/add -1 :entity/key canvas]
     [:db/add -1 :canvas/image -2]
     [:db/add -2 :image/checksum checksum]
     [:db/add -2 :image/width width]
     [:db/add -2 :image/height height]
     [:db/add -2 :image/filename filename]]))

(defmethod transact :canvas/change-grid-size
  [{:keys [canvas]} size]
  [[:db/add -1 :entity/key canvas]
   [:db/add -1 :grid/size size]])

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
      [[:db/add -1 :entity/key canvas]
       [:db/add -1 :grid/size size]
       [:db/add -2 :entity/key window]
       [:db/add -2 :window/draw-mode :select]]
      (let [pattern [size (+ size 1) (- size 1) (+ size 2) (- size 2) (+ size 3) (- size 3) (+ size 4) (- size 4)]
            next    (reduce (fn [_ n] (when (zero? (mod width n)) (reduced n))) pattern)]
        [[:db/add -1 :entity/key canvas]
         [:db/add -1 :grid/size (or next size)]
         [:db/add -2 :entity/key window]
         [:db/add -2 :window/draw-mode :select]]))))

(defmethod transact :canvas/change-theme
  [{:keys [canvas]} theme]
  [[:db/add -1 :entity/key canvas]
   [:db/add -1 :canvas/theme theme]])

(defmethod transact :canvas/change-visibility
  [{:keys [canvas]} value]
  [[:db/add -1 :entity/key canvas]
   [:db/add -1 :canvas/visibility value]])

(defmethod transact :canvas/change-color
  [{:keys [canvas]} value]
  [[:db/add -1 :entity/key canvas]
   [:db/add -1 :canvas/color value]])

(defmethod transact :element/update
  [_ keys attr value]
  (for [[id key] (sequence (indexed) keys)]
    (assoc {:db/id id :entity/key key} attr value)))

(defmethod transact :element/select
  [{:keys [data window]} key replace?]
  (let [lookup [:entity/key key]
        entity (ds/entity data lookup)]
    [[:db/add -1 :entity/key window]
     (if replace?
       [:db/retract [:entity/key window] :window/selected])
     (if (and (not replace?) (:window/_selected entity))
       [:db/retract [:entity/key window] :window/selected lookup]
       [:db/add -1 :window/selected lookup])]))

(defmethod transact :element/remove
  [_ keys]
  (for [key keys]
    [:db/retractEntity [:entity/key key]]))

(defmethod transact :scene/create
  [{:keys [canvas]} checksum filename width height]
  [[:db/add -1 :image/checksum checksum]
   [:db/add -1 :image/filename filename]
   [:db/add -1 :image/width width]
   [:db/add -1 :image/height height]
   [:db/add [:db/ident :root] :root/scenes -1]
   [:db/add -2 :entity/key canvas]
   [:db/add -2 :canvas/image -1]])

(defmethod transact :map/remove
  [_ checksum]
  [[:db/retractEntity [:image/checksum checksum]]])

(defmethod transact :token/create
  [{:keys [window canvas]} x y]
  [[:db/add -1 :entity/key (squuid)]
   [:db/add -1 :token/vec [x y]]
   [:db/add -2 :entity/key window]
   [:db/add -2 :window/selected -1]
   [:db/add -2 :window/draw-mode :select]
   [:db/add -3 :entity/key canvas]
   [:db/add -3 :canvas/tokens -1]])

(defmethod transact :token/translate
  [{:keys [data canvas]} token x y align?]
  (let [{size :grid/size} (ds/pull data [[:grid/size :default 70]] [:entity/key canvas])
        radius            (if align? (/ size 2) 1)]
    [[:db/add -1 :entity/key token]
     [:db/add -1 :token/vec [(round x radius) (round y radius)]]]))

(defmethod transact :token/change-flag
  [{:keys [data]} keys flag add?]
  (let [idents (map (fn [key] [:entity/key key]) keys)
        tokens (ds/pull-many data [:entity/key :token/flags] idents)]
    (for [[id {:keys [entity/key token/flags] :or {flags #{}}}] (sequence (indexed) tokens)]
      {:db/id id :entity/key key :token/flags ((if add? conj disj) flags flag)})))

(defmethod transact :token/translate-all
  [{:keys [data canvas]} keys x y align?]
  (let [{size :grid/size} (ds/pull data [[:grid/size :default 70]] [:entity/key canvas])
        lookup            (map (fn [key] [:entity/key key]) keys)
        tokens            (ds/pull-many data [:entity/key :token/vec] lookup)
        radius            (if align? (/ size 2) 1)]
    (for [[id {key :entity/key [tx ty] :token/vec}] (sequence (indexed) tokens)]
      {:db/id id :entity/key key :token/vec [(round (+ x tx) radius) (round (+ y ty) radius)]})))

(defmethod transact :token/change-label
  [_ keys value]
  (for [[id key] (sequence (indexed) keys)]
    {:db/id id :entity/key key :token/label (trim value)}))

(defmethod transact :token/change-size
  [_ keys radius]
  (for [[id key] (sequence (indexed) keys)]
    {:db/id id :entity/key key :token/size radius}))

(defmethod transact :token/change-light
  [_ keys radius]
  (for [[id key] (sequence (indexed) keys)]
    {:db/id id :entity/key key :token/light radius}))

(defmethod transact :token/change-aura
  [_ keys radius]
  (for [[id key] (sequence (indexed) keys)]
    {:db/id id :entity/key key :aura/radius radius}))

(defmethod transact :token/change-stamp
  [_ keys checksum]
  (concat [[:db/add -1 :image/checksum checksum]]
          (for [[id key] (sequence (indexed 2) keys)]
            {:db/id id :entity/key key :token/image -1})))

(transact {:event :token/change-stamp} [\a \b \c] "foo")

(defmethod transact :token/remove-stamp
  [_ keys]
  (->> (for [[id key] (sequence (indexed) keys)]
         [[:db/add id :entity/key key]
          [:db/retract [:entity/key key] :token/image]])
       (into [] cat)))

(defmethod transact :shape/create
  [{:keys [window canvas]} kind vecs]
  [[:db/add -1 :entity/key (squuid)]
   [:db/add -1 :shape/kind kind]
   [:db/add -1 :shape/vecs vecs]
   [:db/add -2 :entity/key canvas]
   [:db/add -2 :canvas/shapes -1]
   [:db/add -3 :entity/key window]
   [:db/add -3 :window/draw-mode :select]
   [:db/add -3 :window/selected -1]])

(defn trans-xf [x y]
  (comp (partition-all 2) (drop 1) (map (fn [[ax ay]] [(+ ax x) (+ ay y)])) cat))

(defmethod transact :shape/translate
  [{:keys [data canvas]} key x y align?]
  (let [{[ax ay] :shape/vecs vecs :shape/vecs} (ds/pull data [:shape/vecs] [:entity/key key])
        {size :grid/size} (ds/pull data [[:grid/size :default 70]] [:entity/key canvas])
        r (if align? (/ size 2) 1)
        x (round x r)
        y (round y r)]
    [[:db/add -1 :entity/key key]
     [:db/add -1 :shape/vecs (into [x y] (trans-xf (- x ax) (- y ay)) vecs)]]))

(defmethod transact :share/initiate [] [])

(defmethod transact :share/toggle
  [{:keys [data local]} open?]
  (let [{:keys [local/type]} (ds/pull data [:local/type] [:db/ident :local])]
    [{:db/id -1
      :entity/key local
      :local/sharing? open?
      :local/paused? false
      :local/privileged? (and (= type :host) open?)}]))

(defmethod transact :share/switch
  ([{:keys [data local] :as context}]
   (let [{:keys [local/paused?]} (ds/pull data [:local/paused?] [:entity/key local])]
     (transact context (not paused?))))
  ([{:keys [local]} paused?]
   [[:db/add -1 :entity/key local]
    [:db/add -1 :local/paused? paused?]]))

(defmethod transact :bounds/change
  [{:keys [data local]} w-type bounds]
  (let [{type :local/type} (ds/pull data [:local/type] [:db/ident :local])]
    [[:db/add -1 :entity/key local]
     (if (= w-type type)
       [:db/add -1 :bounds/self bounds])
     [:db/add -1 (keyword :bounds w-type) bounds]]))

(defmethod transact :selection/from-rect
  [{:keys [data window canvas]} vecs]
  (let [select [{:canvas/tokens [:entity/key :token/vec]}]
        result (ds/pull data select [:entity/key canvas])
        bounds (normalize vecs)]
    [{:db/id -1
      :entity/key window
      :window/draw-mode :select
      :window/selected
      (for [[idx token] (sequence (indexed 2) (:canvas/tokens result))
            :let  [{[x y] :token/vec key :entity/key} token]
            :when (within? x y bounds)]
        {:db/id idx :entity/key key})}]))

(defmethod transact :selection/clear
  [{:keys [window]}]
  [[:db/add -1 :entity/key window]
   [:db/retract [:entity/key window] :window/selected]])

(defmethod transact :selection/remove
  [{:keys [data window]}]
  (let [result (ds/pull data [{:window/selected [:entity/key]}] [:entity/key window])]
    (for [{:keys [entity/key]} (:window/selected result)]
      [:db/retractEntity [:entity/key key]])))

(defmethod transact :initiative/toggle
  [{:keys [data canvas]} keys adding?]
  (let [tokens   (map (fn [key] [:entity/key key]) keys)
        select-t [:entity/key :token/label :initiative/suffix [:token/flags :default #{}]]
        select-r [{:canvas/initiative select-t}]
        result   (ds/pull data select-r [:entity/key canvas])
        change   (into #{} (ds/pull-many data select-t tokens))
        exists   (into #{} (:canvas/initiative result))]
    (if adding?
      [{:db/id -1
        :entity/key canvas
        :canvas/initiative
        (let [merge (union exists change)
              sffxs (suffixes merge)]
          (for [[idx token] (sequence (indexed 2) merge) :let [key (:entity/key token)]]
            (if-let [suffix (sffxs key)]
              {:db/id idx :entity/key key :initiative/suffix suffix}
              {:db/id idx :entity/key key})))}]
      (apply concat
             [[:db/add -1 :entity/key canvas]]
             (for [[idx {key :entity/key}] (sequence (indexed 2) change)]
               [[:db/add idx :entity/key key]
                [:db/retract [:entity/key key] :initiative/suffix]
                [:db/retract [:entity/key key] :initiative/roll]
                [:db/retract [:entity/key key] :initiative/health]
                [:db/retract [:entity/key canvas] :canvas/initiative idx]])))))

(defmethod transact :initiative/change-roll
  [_ key roll]
  (let [parsed (.parseFloat js/window roll)]
    (cond
      (or (nil? roll) (= roll ""))
      [[:db/add -1 :entity/key key]
       [:db/retract [:entity/key key] :initiative/roll]]

      (.isNaN js/Number parsed)
      []

      :else
      [{:db/id -1 :entity/key key :initiative/roll parsed}])))

(defmethod transact :initiative/roll-all
  [{:keys [data canvas]}]
  (let [select [{:canvas/initiative [:entity/key :token/flags :initiative/roll]}]
        result (ds/pull data select [:entity/key canvas])
        tokens (:canvas/initiative result)]
    (for [[idx token] (sequence (indexed) tokens)
          :let  [{:keys [entity/key token/flags initiative/roll]} token]
          :when (and (nil? roll) (not (contains? flags :player)))]
      {:db/id idx :entity/key key :initiative/roll (inc (rand-int 20))})))

(defmethod transact :initiative/reset-rolls
  [{:keys [data canvas]}]
  (let [result (ds/pull data [{:canvas/initiative [:entity/key]}] [:entity/key canvas])]
    (->> (for [[idx token] (sequence (indexed) (:canvas/initiative result))
               :let [{key :entity/key} token]]
           [[:db/add idx :entity/key key]
            [:db/retract [:entity/key key] :initiative/roll]])
         (into [] cat))))

(defmethod transact :initiative/change-health
  [{:keys [data]} key f value]
  (let [parsed (.parseFloat js/window value)]
    (if (.isNaN js/Number parsed) []
        (let [{:keys [initiative/health]} (ds/entity data [:entity/key key])]
          [{:db/id -1 :entity/key key :initiative/health (f health parsed)}]))))

(defmethod transact :initiative/leave
  [{:keys [data canvas]}]
  (let [select [{:canvas/initiative [:entity/key]}]
        result (ds/pull data select [:entity/key canvas])
        tokens (:canvas/initiative result)]
    (apply concat
           [[:db/add -1 :entity/key canvas]
            [:db/retract [:entity/key canvas] :canvas/initiative]]
           (for [[idx {key :entity/key}] (sequence (indexed 2) tokens)]
             [[:db/add idx :entity/key key]
              [:db/retract [:entity/key key] :initiative/roll]
              [:db/retract [:entity/key key] :initiative/health]
              [:db/retract [:entity/key key] :initiative/suffix]]))))

(defmethod transact :storage/reset [] [])

(defmethod transact :interface/toggle-shortcuts
  [{:keys [local]} display?]
  [{:db/id -1 :entity/key local :local/shortcuts? display?}])

(defmethod transact :interface/toggle-tooltips
  [{:keys [local]} display?]
  [{:db/id -1 :entity/key local :local/tooltips? display?}])

(defmethod transact :interface/change-panel
  [{:keys [window]} panel]
  [{:db/id -1 :entity/key window :panel/current panel :panel/collapsed? false}])

(defmethod transact :interface/toggle-panel
  [{:keys [data window]}]
  (let [entity (ds/entity data [:entity/key window])]
    [{:db/id -1 :entity/key window :panel/collapsed? (not (:panel/collapsed? entity))}]))

(defmethod transact :stamp/create
  [_ checksum filename width height]
  [{:db/id          -1
    :image/checksum checksum
    :image/filename filename
    :image/width    width
    :image/height   height}
   {:db/id [:db/ident :root] :root/stamps -1}])

(defmethod transact :stamp/remove [_ checksum]
  [[:db/retractEntity [:image/checksum checksum]]])

(defmethod transact :mask/fill
  [{:keys [canvas]}]
  [[:db/add -1 :entity/key canvas]
   [:db/add -1 :mask/filled? true]
   [:db/retract [:entity/key canvas] :canvas/masks]])

(defmethod transact :mask/clear
  [{:keys [canvas]}]
  [[:db/add -1 :entity/key canvas]
   [:db/add -1 :mask/filled? false]
   [:db/retract [:entity/key canvas] :canvas/masks]])

(defmethod transact :mask/create
  [{:keys [canvas]} state vecs]
  [[:db/add -1 :entity/key canvas]
   [:db/add -1 :canvas/masks -2]
   [:db/add -2 :entity/key (squuid)]
   [:db/add -2 :mask/enabled? state]
   [:db/add -2 :mask/vecs vecs]])

(defmethod transact :mask/toggle
  [_ mask state]
  [[:db/add -1 :entity/key mask]
   [:db/add -1 :mask/enabled? state]])

(defmethod transact :mask/remove
  [_ mask]
  [[:db/retractEntity [:entity/key mask]]])

(defmethod transact :session/request
  [{:keys [local]}]
  [{:db/id -1 :db/ident :session :session/host -2}
   {:db/id -2 :entity/key local :session/state :connecting}
   {:db/id -3 :db/ident :root :root/session -1}])

(defmethod transact :session/close
  [{:keys [local]}]
  [{:db/id -1 :entity/key local :session/state :disconnected}
   [:db/retractEntity [:db/ident :session]]])

(defmethod transact :session/disconnected
  [{:keys [local]}]
  [{:db/id -1 :entity/key local :session/state :disconnected}
   [:db/retractEntity [:db/ident :session]]])

(defmethod transact :image/request [] [])

(defmethod transact :image/cache [] [])
