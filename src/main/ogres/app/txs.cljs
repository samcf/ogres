(ns ogres.app.txs
  (:require [datascript.core :as ds :refer [squuid]]
            [clojure.set :refer [union]]
            [clojure.string :refer [trim]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.geom :refer [normalize within?]]))

(def ^:private suffix-max-xf
  (map (fn [[label tokens]] [label (apply max (map :initiative/suffix tokens))])))

(def ^:private zoom-scales
  [0.15 0.30 0.50 0.75 0.90 1 1.25 1.50 2 3 4])

(defn- find-next
  "Finds the element in the given collection which passes the given predicate
   and returns the element that appears after it. Returns nil if no element
   passes the predicate or if the element found is the last in the collection."
  [pred xs]
  (first (next (drop-while (complement pred) xs))))

(defn- indexed
  "Returns a transducer which decorates each element with a decreasing
   negative index suitable for use as temporary ids in a DataScript
   transaction. Optionally receives an offset integer to begin counting and
   a step integer to create space between indexes."
  ([]
   (indexed 1 1))
  ([offset]
   (indexed offset 1))
  ([offset step]
   (map-indexed (fn [idx val] [(-> (* idx step) (+ offset) (* -1)) val]))))

(defn- suffix-token-key
  "Returns a grouping key for the given token that will match other similarly
   identifiable tokens."
  [token]
  (let [{label :token/label
         {checksum :image/checksum} :token/image} token]
    [label checksum]))

(defn- suffixes
  "Returns a map of `{entity key => suffix}` for the given token entities.
   Each suffix represents a unique and stable identity for a token within
   the group of tokens by which it shares a label. Suffixes are intended to
   help decorate tokens that may otherwise be difficult to distinguish
   when they share the same image and label."
  [tokens]
  (let [groups (group-by suffix-token-key tokens)
        offset (into {} suffix-max-xf groups)]
    (loop [tokens tokens index {} result {}]
      (if (seq tokens)
        (let [token (first tokens)
              group (suffix-token-key token)]
          (if (or (= (count (groups group)) 1)
                  (:initiative/suffix token)
                  (contains? (:token/flags token) :player))
            (recur (rest tokens) index result)
            (recur (rest tokens)
                   (update index group inc)
                   (assoc result (:db/key token) (+ (offset group) (index group) 1)))))
        result))))

(defn- round [x n]
  (* (js/Math.round (/ x n)) n))

(defn- to-precision [n p]
  (js/Number (.toFixed (js/Number.parseFloat n) p)))

(defn- constrain [n min max]
  (clojure.core/max (clojure.core/min n max) min))

(defn- mode-allowed? [mode type]
  (not (and (contains? #{:mask :mask-toggle :mask-remove} mode)
            (not= type :host))))

(defn- trans-xf [x y]
  (comp (partition-all 2) (drop 1) (map (fn [[ax ay]] [(+ ax x) (+ ay y)])) cat))

(defn- initiative-order [a b]
  (let [f (juxt :initiative/roll :db/key)]
    (compare (f b) (f a))))

(defmulti transact (fn [{:keys [event]}] event))

(defmethod transact :default [] [])

(defmethod transact :workspace/create
  [{:keys [local]}]
  [[:db/add -1 :db/key (squuid)]
   [:db/add -2 :db/key (squuid)]
   [:db/add [:db/key local] :local/window -1]
   [:db/add [:db/key local] :local/windows -1]
   [:db/add -1 :window/canvas -2]
   [:db/add [:db/ident :root] :root/canvases -2]])

(defmethod transact :workspace/change
  [{:keys [local]} key]
  [[:db/add -1 :db/key local]
   [:db/add -2 :db/key key]
   [:db/add -1 :local/window -2]])

(defmethod transact :workspace/remove
  [{:keys [data local]} key]
  (let [select-w [:db/key {:window/canvas [:db/key]}]
        select-l [:db/key {:local/windows select-w :local/window select-w}]
        select-r [{:root/local select-l} {:root/session [{:session/conns select-l}]}]
        select-o [:db/key {:window/canvas [:db/key {:window/_canvas [:db/key]}]}]

        {{canvas :db/key
          remove :window/_canvas} :window/canvas}
        (ds/pull data select-o [:db/key key])

        {{conns   :session/conns} :root/session
         {window  :local/window
          windows :local/windows} :root/local}
        (ds/pull data select-r [:db/ident :root])]
    (cond
      (= (count windows) 1)
      (into [[:db/retractEntity [:db/key canvas]]
             [:db/add -1 :db/key (squuid)]
             [:db/add -1 :window/canvas -2]
             [:db/add -2 :db/key (squuid)]
             [:db/add [:db/key local] :local/window -1]
             [:db/add [:db/key local] :local/windows -1]]
            (comp cat cat)
            (list (for [{:keys [db/key]} remove]
                    [[:db/retractEntity [:db/key key]]])
                  (for [[idx conn] (sequence (indexed 3 2) conns)
                        :let [tmp (dec idx)]]
                    [[:db/add idx :db/key (:db/key conn)]
                     [:db/add idx :local/windows tmp]
                     [:db/add idx :local/window tmp]
                     [:db/add tmp :db/key (squuid)]
                     [:db/add tmp :window/canvas -2]
                     [:db/add tmp :window/point [0 0]]
                     [:db/add tmp :window/scale 1]])))

      (= key (:db/key window))
      (let [next (->> windows (filter #(not= (:db/key %) (:db/key window))) (first))]
        (into [[:db/retractEntity [:db/key canvas]]
               [:db/add -1 :db/key (:db/key next)]
               [:db/add -2 :db/key local]
               [:db/add -2 :local/window -1]]
              (comp cat cat)
              (list (for [{:keys [db/key]} remove]
                      [[:db/retractEntity [:db/key key]]])
                    (for [[idx conn] (sequence (indexed 3 2) conns)
                          :let [tmp (dec idx)
                                key (->> (:local/windows conn)
                                         (filter #(= (:db/key (:window/canvas %))
                                                     (:db/key (:window/canvas next))))
                                         (first)
                                         (:db/key))]]
                      [[:db/add idx :db/key (:db/key conn)]
                       [:db/add idx :local/windows tmp]
                       [:db/add idx :local/window tmp]
                       [:db/add tmp :db/key (or key (squuid))]
                       [:db/add tmp :window/canvas [:db/key (:db/key (:window/canvas next))]]
                       [:db/add tmp :window/point [0 0]]
                       [:db/add tmp :window/scale 1]]))))

      :else
      (into [[:db/retractEntity [:db/key canvas]]]
            (for [{:keys [db/key]} remove]
              [:db/retractEntity [:db/key key]])))))

(defmethod transact :window/change-label
  [{:keys [window]} label]
  [[:db/add [:db/key window] :window/label label]])

(defmethod transact :window/translate
  [{:keys [window]} x y]
  [[:db/add -1 :db/key window]
   [:db/add -1 :window/point [(round x 1) (round y 1)]]])

(defmethod transact :window/change-mode
  [{:keys [data local window]} mode]
  (let [{curr :window/draw-mode} (ds/entity data [:db/key window])
        {type :local/type}       (ds/entity data [:db/key local])
        allowed? (mode-allowed? mode type)]
    (if allowed?
      [[:db/add -1 :db/key window]
       [:db/add -1 :window/draw-mode mode]
       (if (= mode curr)
         [:db/add -1 :window/draw-mode :select]
         [:db/retract [:db/key window] :window/selected])]
      [])))

(defmethod transact :window/modifier-start
  [{:keys [window]} modifier]
  [[:db/add -1 :db/key window]
   [:db/add -1 :window/modifier modifier]])

(defmethod transact :window/modifier-release
  [{:keys [window]}]
  [[:db/add -1 :db/key window]
   [:db/retract [:db/key window] :window/modifier]])

(defmethod transact :zoom/change
  ([context]
   (transact context 1))
  ([{:keys [data local] :as context} next]
   (let [select    [[:bounds/self :default [0 0 0 0]]]
         result    (ds/pull data select [:db/key local])
         [_ _ w h] (:bounds/self result)]
     (transact context next (/ w 2) (/ h 2))))
  ([{:keys [data window]} next x y]
   (let [select [[:window/scale :default 1] [:window/point :default [0 0]]]
         result (ds/pull data select [:db/key window])
         {prev :window/scale [cx cy] :window/point} result
         fx (/ next prev)
         dx (/ (- (* x fx) x) next)
         dy (/ (- (* y fx) y) next)]
     [[:db/add -1 :db/key window]
      [:db/add -1 :window/point [(round (- cx dx) 1) (round (- cy dy) 1)]]
      [:db/add -1 :window/scale next]])))

(defmethod transact :zoom/delta
  [{:keys [data window] :as context} delta x y]
  (let [result (ds/pull data [[:window/scale :default 1]] [:db/key window])
        next   (-> (:window/scale result)
                   (js/Math.log)
                   (+ delta)
                   (js/Math.exp)
                   (to-precision 2)
                   (constrain 0.15 4))]
    (transact (assoc context :event :zoom/change) next x y)))

(defmethod transact :zoom/in
  [{:keys [data window] :as context}]
  (let [info (ds/pull data [[:window/scale :default 1]] [:db/key window])
        prev (:window/scale info)
        next (reduce (fn [n s] (if (> s prev) (reduced s) n)) prev zoom-scales)]
    (transact (assoc context :event :zoom/change) next)))

(defmethod transact :zoom/out
  [{:keys [data window] :as context}]
  (let [info (ds/pull data [[:window/scale :default 1]] [:db/key window])
        prev (:window/scale info)
        next (reduce (fn [n s] (if (< s prev) (reduced s) n)) prev (reverse zoom-scales))]
    (transact (assoc context :event :zoom/change) next)))

(defmethod transact :zoom/reset [context]
  (transact (assoc context :event :zoom/change) 1))

(defmethod transact :canvas/change-scene
  [{:keys [data canvas]} checksum]
  (let [{:image/keys [width height]} (ds/entity data [:image/checksum checksum])]
    [[:db/add -1 :db/key canvas]
     [:db/add -1 :canvas/image -2]
     [:db/add -2 :image/checksum checksum]
     [:db/add -2 :image/width width]
     [:db/add -2 :image/height height]]))

(defmethod transact :canvas/change-grid-size
  [{:keys [canvas]} size]
  [[:db/add -1 :db/key canvas]
   [:db/add -1 :canvas/grid-size size]])

(defmethod transact :canvas/draw-grid-size
  [{:keys [data window canvas]} _ _ size]
  (let [lookup [:db/key window]
        select [[:window/scale :default 1] {:window/canvas [{:canvas/image [:image/width]}]}]
        result (ds/pull data select lookup)
        {scale :window/scale
         {scene :canvas/image
          {width :image/width} :canvas/image} :window/canvas} result
        size (js/Math.round (/ size scale))]
    (if (nil? scene)
      [[:db/add -1 :db/key canvas]
       [:db/add -1 :canvas/grid-size size]
       [:db/add -2 :db/key window]
       [:db/add -2 :window/draw-mode :select]]
      (let [pattern [size (+ size 1) (- size 1) (+ size 2) (- size 2) (+ size 3) (- size 3) (+ size 4) (- size 4)]
            next    (reduce (fn [_ n] (when (zero? (mod width n)) (reduced n))) pattern)]
        [[:db/add -1 :db/key canvas]
         [:db/add -1 :canvas/grid-size (or next size)]
         [:db/add -2 :db/key window]
         [:db/add -2 :window/draw-mode :select]]))))

(defmethod transact :canvas/toggle-show-grid
  [{:keys [canvas]} value]
  [[:db/add -1 :db/key canvas]
   [:db/add -1 :canvas/show-grid value]])

(defmethod transact :canvas/toggle-snap-grid
  [{:keys [canvas]} value]
  [[:db/add -1 :db/key canvas]
   [:db/add -1 :canvas/snap-grid value]])

(defmethod transact :canvas/toggle-dark-mode
  [{:keys [canvas]} enabled]
  [[:db/add -1 :db/key canvas]
   [:db/add -1 :canvas/dark-mode enabled]])

(defmethod transact :canvas/change-lighting
  [{:keys [canvas]} value]
  [[:db/add -1 :db/key canvas]
   [:db/add -1 :canvas/lighting value]])

(defmethod transact :canvas/change-time-of-day
  [{:keys [canvas]} value]
  [[:db/add -1 :db/key canvas]
   [:db/add -1 :canvas/timeofday value]])

(defmethod transact :element/update
  [_ keys attr value]
  (for [[id key] (sequence (indexed) keys)]
    (assoc {:db/id id :db/key key} attr value)))

(defmethod transact :element/select
  [{:keys [data window]} key replace?]
  (let [lookup [:db/key key]
        entity (ds/entity data lookup)]
    [[:db/add -1 :db/key window]
     (if replace?
       [:db/retract [:db/key window] :window/selected])
     (if (and (not replace?) (:window/_selected entity))
       [:db/retract [:db/key window] :window/selected lookup]
       [:db/add -1 :window/selected lookup])]))

(defmethod transact :element/remove
  [_ keys]
  (for [key keys]
    [:db/retractEntity [:db/key key]]))

(defmethod transact :scene/create
  [_ checksum width height]
  [[:db/add -1 :image/checksum checksum]
   [:db/add -1 :image/width width]
   [:db/add -1 :image/height height]
   [:db/add [:db/ident :root] :root/scenes -1]])

(defmethod transact :scene/remove
  [_ checksum]
  [[:db/retractEntity [:image/checksum checksum]]])

(defmethod transact :scene/remove-all [_]
  [[:db/retract [:db/ident :root] :root/scenes]])

(defmethod transact :token/create
  [{:keys [window canvas]} x y checksum]
  [[:db/add -1 :db/key (squuid)]
   [:db/add -1 :token/point [x y]]
   [:db/add -1 :token/image -4]
   [:db/add -2 :db/key window]
   [:db/add -2 :window/selected -1]
   [:db/add -2 :window/draw-mode :select]
   [:db/add -3 :db/key canvas]
   [:db/add -3 :canvas/tokens -1]
   [:db/add -4 :image/checksum checksum]])

(defmethod transact :token/remove
  [{:keys [data canvas]} keys]
  (let [keys (set keys)
        cnvs (ds/entity data [:db/key canvas])
        curr (->> (:initiative/turn cnvs) :db/key)
        tkns (->> (:canvas/initiative cnvs) (sort initiative-order) (map :db/key))
        tkfn (complement (partial contains? (disj keys curr)))
        next (->> (filter tkfn tkns) (find-next (partial = curr)))
        data {:db/key canvas :initiative/turn {:db/key (or next (first tkns))}}]
    (cond-> (for [key keys] [:db/retractEntity [:db/key key]])
      (contains? keys curr) (conj data))))

(defmethod transact :token/translate
  [_ token x y align?]
  (let [radius (if align? (/ grid-size 2) 1)]
    [[:db/add -1 :db/key token]
     [:db/add -1 :token/point [(round x radius) (round y radius)]]]))

(defmethod transact :token/change-flag
  [{:keys [data]} keys flag add?]
  (let [idents (map (fn [key] [:db/key key]) keys)
        tokens (ds/pull-many data [:db/key :token/flags] idents)]
    (for [[id {:keys [db/key token/flags] :or {flags #{}}}] (sequence (indexed) tokens)]
      {:db/id id :db/key key :token/flags ((if add? conj disj) flags flag)})))

(defmethod transact :token/translate-all
  [{:keys [data]} keys x y align?]
  (let [lookup (map (fn [key] [:db/key key]) keys)
        tokens (ds/pull-many data [:db/key :token/point] lookup)
        radius (if align? (/ grid-size 2) 1)]
    (for [[id {key :db/key [tx ty] :token/point}] (sequence (indexed) tokens)]
      {:db/id id :db/key key :token/point [(round (+ x tx) radius) (round (+ y ty) radius)]})))

(defmethod transact :token/change-label
  [_ keys value]
  (for [[id key] (sequence (indexed) keys)]
    {:db/id id :db/key key :token/label (trim value)}))

(defmethod transact :token/change-size
  [_ keys radius]
  (for [[id key] (sequence (indexed) keys)]
    {:db/id id :db/key key :token/size radius}))

(defmethod transact :token/change-light
  [_ keys radius]
  (for [[id key] (sequence (indexed) keys)]
    {:db/id id :db/key key :token/light radius}))

(defmethod transact :token/change-aura
  [_ keys radius]
  (for [[id key] (sequence (indexed) keys)]
    {:db/id id :db/key key :aura/radius radius}))

(defmethod transact :token/change-stamp
  [_ keys checksum]
  (concat [[:db/add -1 :image/checksum checksum]]
          (for [[id key] (sequence (indexed 2) keys)]
            {:db/id id :db/key key :token/image -1})))

(defmethod transact :token/remove-stamp
  [_ keys]
  (->> (for [[id key] (sequence (indexed) keys)]
         [[:db/add id :db/key key]
          [:db/retract [:db/key key] :token/image]])
       (into [] cat)))

(defmethod transact :shape/create
  [{:keys [window canvas]} kind vecs]
  [[:db/add -1 :db/key (squuid)]
   [:db/add -1 :shape/kind kind]
   [:db/add -1 :shape/vecs vecs]
   [:db/add -2 :db/key canvas]
   [:db/add -2 :canvas/shapes -1]
   [:db/add -3 :db/key window]
   [:db/add -3 :window/draw-mode :select]
   [:db/add -3 :window/selected -1]])

(defmethod transact :shape/remove
  [_ keys]
  (for [key keys]
    [:db/retractEntity [:db/key key]]))

(defmethod transact :shape/translate
  [{:keys [data]} key x y align?]
  (let [{[ax ay] :shape/vecs vecs :shape/vecs} (ds/pull data [:shape/vecs] [:db/key key])
        r (if align? (/ grid-size 2) 1)
        x (round x r)
        y (round y r)]
    [[:db/add -1 :db/key key]
     [:db/add -1 :shape/vecs (into [x y] (trans-xf (- x ax) (- y ay)) vecs)]]))

(defmethod transact :share/initiate [] [])

(defmethod transact :share/toggle
  [{:keys [data local]} open?]
  (let [{:keys [local/type]} (ds/pull data [:local/type] [:db/ident :local])]
    [{:db/id -1
      :db/key local
      :local/sharing? open?
      :local/paused? false
      :local/privileged? (and (= type :host) open?)}]))

(defmethod transact :share/switch
  ([{:keys [data local] :as context}]
   (let [{:keys [local/paused?]} (ds/pull data [:local/paused?] [:db/key local])]
     (transact context (not paused?))))
  ([{:keys [local]} paused?]
   [[:db/add -1 :db/key local]
    [:db/add -1 :local/paused? paused?]]))

(defmethod transact :bounds/change
  [{:keys [data local]} w-type bounds]
  (let [{type :local/type} (ds/pull data [:local/type] [:db/ident :local])]
    [[:db/add -1 :db/key local]
     (if (= w-type type)
       [:db/add -1 :bounds/self bounds])
     [:db/add -1 (keyword :bounds w-type) bounds]]))

(defmethod transact :selection/from-rect
  [{:keys [data window canvas]} vecs]
  (let [select [{:canvas/tokens [:db/key :token/point]}]
        result (ds/pull data select [:db/key canvas])
        bounds (normalize vecs)]
    [{:db/id -1
      :db/key window
      :window/draw-mode :select
      :window/selected
      (for [[idx token] (sequence (indexed 2) (:canvas/tokens result))
            :let  [{[x y] :token/point key :db/key} token]
            :when (within? x y bounds)]
        {:db/id idx :db/key key})}]))

(defmethod transact :selection/clear
  [{:keys [window]}]
  [[:db/add -1 :db/key window]
   [:db/retract [:db/key window] :window/selected]])

(defmethod transact :selection/remove
  [{:keys [data window] :as ctx}]
  (let [select [{:window/selected [:db/key :canvas/_tokens :canvas/_shapes]}]
        result (ds/pull data select [:db/key window])
        groups (group-by (fn [x] (cond (:canvas/_tokens x) :token
                                       (:canvas/_shapes x) :shape))
                         (:window/selected result))]
    (concat (transact (assoc ctx :event :token/remove) (map :db/key (:token groups)))
            (transact (assoc ctx :event :shape/remove) (map :db/key (:shape groups))))))

(defmethod transact :initiative/toggle
  [{:keys [data canvas]} keys adding?]
  (let [tokens (map (fn [key] [:db/key key]) keys)
        select [{:token/image [:image/checksum]}
                [:token/flags :default #{}]
                :db/key
                :token/label
                :initiative/suffix]
        result (ds/pull data [{:canvas/initiative select}] [:db/key canvas])
        change (into #{} (ds/pull-many data select tokens))
        exists (into #{} (:canvas/initiative result))]
    (if adding?
      [{:db/id -1
        :db/key canvas
        :canvas/initiative
        (let [merge (union exists change)
              sffxs (suffixes merge)]
          (for [[idx token] (sequence (indexed 2) merge) :let [key (:db/key token)]]
            (if-let [suffix (sffxs key)]
              {:db/id idx :db/key key :initiative/suffix suffix}
              {:db/id idx :db/key key})))}]
      (apply concat
             [[:db/add -1 :db/key canvas]]
             (for [[idx {key :db/key}] (sequence (indexed 2) change)]
               [[:db/add idx :db/key key]
                [:db/retract [:db/key key] :initiative/suffix]
                [:db/retract [:db/key key] :initiative/roll]
                [:db/retract [:db/key key] :initiative/health]
                [:db/retract [:db/key canvas] :canvas/initiative idx]])))))

(defmethod transact :initiative/next
  [{:keys [data canvas]}]
  (let [{curr :initiative/turn
         trns :initiative/turns
         rnds :initiative/rounds
         tkns :canvas/initiative} (ds/entity data [:db/key canvas])
        tkns (->> tkns (sort initiative-order) (map :db/key))]
    (if (nil? rnds)
      [{:db/key        canvas
        :initiative/turn   {:db/key (first tkns)}
        :initiative/turns  0
        :initiative/rounds 1}]
      (if-let [next (find-next (partial = (:db/key curr)) tkns)]
        [{:db/key       canvas
          :initiative/turn  {:db/key next}
          :initiative/turns (inc trns)}]
        [{:db/key        canvas
          :initiative/turn   {:db/key (first tkns)}
          :initiative/turns  (inc trns)
          :initiative/rounds (inc rnds)}]))))

(defmethod transact :initiative/change-roll
  [_ key roll]
  (let [parsed (.parseFloat js/window roll)]
    (cond
      (or (nil? roll) (= roll ""))
      [[:db/add -1 :db/key key]
       [:db/retract [:db/key key] :initiative/roll]]

      (.isNaN js/Number parsed)
      []

      :else
      [{:db/id -1 :db/key key :initiative/roll parsed}])))

(defmethod transact :initiative/roll-all
  [{:keys [data canvas]}]
  (let [select [{:canvas/initiative [:db/key :token/flags :initiative/roll]}]
        result (ds/pull data select [:db/key canvas])
        tokens (:canvas/initiative result)]
    (for [[idx token] (sequence (indexed) tokens)
          :let  [{:keys [db/key token/flags initiative/roll]} token]
          :when (and (nil? roll) (not (contains? flags :player)))]
      {:db/id idx :db/key key :initiative/roll (inc (rand-int 20))})))

(defmethod transact :initiative/reset
  [{:keys [data canvas]}]
  (let [result (ds/pull data [{:canvas/initiative [:db/key]}] [:db/key canvas])]
    (->> (for [[idx token] (sequence (indexed 2) (:canvas/initiative result))
               :let [{key :db/key} token]]
           [[:db/add idx :db/key key]
            [:db/retract [:db/key key] :initiative/roll]])
         (into [[:db/add -1 :db/key canvas]
                [:db/retract [:db/key canvas] :initiative/turn]
                [:db/retract [:db/key canvas] :initiative/turns]
                [:db/retract [:db/key canvas] :initiative/rounds]] cat))))

(defmethod transact :initiative/change-health
  [{:keys [data]} key f value]
  (let [parsed (.parseFloat js/window value)]
    (if (.isNaN js/Number parsed) []
        (let [{:keys [initiative/health]} (ds/entity data [:db/key key])]
          [{:db/id -1 :db/key key :initiative/health (f health parsed)}]))))

(defmethod transact :initiative/leave
  [{:keys [data canvas]}]
  (let [select [{:canvas/initiative [:db/key]}]
        result (ds/pull data select [:db/key canvas])
        tokens (:canvas/initiative result)]
    (apply concat
           [[:db/add -1 :db/key canvas]
            [:db/retract [:db/key canvas] :canvas/initiative]
            [:db/retract [:db/key canvas] :initiative/turn]
            [:db/retract [:db/key canvas] :initiative/turns]
            [:db/retract [:db/key canvas] :initiative/rounds]]
           (for [[idx {key :db/key}] (sequence (indexed 2) tokens)]
             [[:db/add idx :db/key key]
              [:db/retract [:db/key key] :initiative/roll]
              [:db/retract [:db/key key] :initiative/health]
              [:db/retract [:db/key key] :initiative/suffix]]))))

(defmethod transact :interface/toggle-shortcuts
  [{:keys [local]} display?]
  [{:db/id -1 :db/key local :local/shortcuts? display?}])

(defmethod transact :interface/toggle-tooltips
  [{:keys [local]} display?]
  [{:db/id -1 :db/key local :local/tooltips? display?}])

(defmethod transact :interface/toggle-panel
  [{:keys [local]} panel]
  [{:db/id -1 :db/key local :panel/expanded #{panel}}])

(defmethod transact :stamp/create
  [_ checksum width height scope]
  [{:db/id          -1
    :image/checksum checksum
    :image/width    width
    :image/height   height
    :image/scope    scope}
   {:db/id [:db/ident :root] :root/stamps -1}])

(defmethod transact :stamp/remove [_ checksum]
  [[:db/retractEntity [:image/checksum checksum]]])

(defmethod transact :stamp/remove-all []
  [[:db/retract [:db/ident :root] :root/stamps]])

(defmethod transact :mask/fill
  [{:keys [canvas]}]
  [[:db/add -1 :db/key canvas]
   [:db/add -1 :mask/filled? true]
   [:db/retract [:db/key canvas] :canvas/masks]])

(defmethod transact :mask/clear
  [{:keys [canvas]}]
  [[:db/add -1 :db/key canvas]
   [:db/add -1 :mask/filled? false]
   [:db/retract [:db/key canvas] :canvas/masks]])

(defmethod transact :mask/create
  [{:keys [canvas]} state vecs]
  [[:db/add -1 :db/key canvas]
   [:db/add -1 :canvas/masks -2]
   [:db/add -2 :db/key (squuid)]
   [:db/add -2 :mask/enabled? state]
   [:db/add -2 :mask/vecs vecs]])

(defmethod transact :mask/toggle
  [_ mask state]
  [[:db/add -1 :db/key mask]
   [:db/add -1 :mask/enabled? state]])

(defmethod transact :mask/remove
  [_ mask]
  [[:db/retractEntity [:db/key mask]]])

(defmethod transact :session/request
  [{:keys [local]}]
  [{:db/id -1 :db/ident :session :session/host -2}
   {:db/id -2 :db/key local :session/state :connecting}
   {:db/id -3 :db/ident :root :root/session -1}])

(defmethod transact :session/join
  [{:keys [local]}]
  [{:db/id -1 :db/key local :session/state :connecting}])

(defmethod transact :session/close
  [{:keys [local]}]
  [{:db/id -1 :db/key local :session/state :disconnected}
   [:db/retract [:db/ident :session] :session/host]
   [:db/retract [:db/ident :session] :session/conns]])

(defmethod transact :session/disconnected
  [{:keys [local]}]
  [{:db/id -1 :db/key local :session/state :disconnected}
   [:db/retract [:db/ident :session] :session/host]
   [:db/retract [:db/ident :session] :session/conns]])

(defmethod transact :session/toggle-share-cursors
  [_ enabled]
  [{:db/id -1 :db/ident :session :session/share-cursors enabled}])

(defmethod transact :session/toggle-share-my-cursor
  [{:keys [local]} enabled]
  [{:db/id -1 :db/key local :session/share-cursor enabled}])

(defmethod transact :session/focus
  [{:keys [data]}]
  (let [select-w [:window/canvas [:window/point :default [0 0]] [:window/scale :default 1]]
        select-l [:db/key [:bounds/self :default [0 0 0 0]] {:local/windows [:window/canvas] :local/window select-w}]
        select-s [{:session/host select-l} {:session/conns select-l}]
        result   (ds/pull data select-s [:db/ident :session])
        {{[_ _ hw hh] :bounds/self
          {[hx hy] :window/point} :local/window
          host :local/window} :session/host
         conns :session/conns} result
        scale (:window/scale host)
        mx (- hx (/ hw scale 2))
        my (- hy (/ hh scale 2))]
    (->> (for [[next conn] (sequence (indexed 1 2) conns)
               :let [prev (dec next)
                     exst (->> (:local/windows conn)
                               (filter (fn [conn]
                                         (= (:db/key (:window/canvas conn))
                                            (:db/key (:window/canvas host)))))
                               (first)
                               (:db/key))
                     [_ _ cw ch] (:bounds/self conn)
                     cx (+ mx (/ cw scale 2))
                     cy (+ my (/ ch scale 2))]]
           [[:db/add next :db/key (:db/key conn)]
            [:db/add next :local/window prev]
            [:db/add next :local/windows prev]
            [:db/add prev :db/key        (or exst (squuid))]
            [:db/add prev :window/point  [cx cy]]
            [:db/add prev :window/scale  scale]
            [:db/add prev :window/canvas (:db/id (:window/canvas host))]])
         (into [] cat))))
