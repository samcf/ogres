(ns ogres.app.txs
  (:require [datascript.core :as ds :refer [squuid]]
            [clojure.set :refer [union]]
            [clojure.string :refer [trim]]
            [ogres.app.geom :refer [bounding-box normalize within?]]
            [ogres.app.util :refer [comp-fn with-ns]]))

(def ^:private suffix-max-xf
  (map (fn [[label tokens]] [label (apply max (map :initiative/suffix tokens))])))

(def ^:private zoom-scales
  [0.15 0.30 0.50 0.75 0.90 1 1.25 1.50 2 3 4])

(defn ^:private find-next
  "Finds the element in the given collection which passes the given predicate
   and returns the element that appears after it. Returns nil if no element
   passes the predicate or if the element found is the last in the collection."
  [pred xs]
  (first (next (drop-while (complement pred) xs))))

(defn ^:private indexed
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

(defn ^:private suffix-token-key
  "Returns a grouping key for the given token that will match other similarly
   identifiable tokens."
  [token]
  (let [{label :token/label
         {checksum :image/checksum} :token/image} token]
    [label checksum]))

(defn ^:private suffixes
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

(defn ^:private round [x n]
  (* (js/Math.round (/ x n)) n))

(defn ^:private to-precision [n p]
  (js/Number (.toFixed (js/Number.parseFloat n) p)))

(defn ^:private constrain [n min max]
  (clojure.core/max (clojure.core/min n max) min))

(defn ^:private mode-allowed? [mode type]
  (not (and (contains? #{:mask :mask-toggle :mask-remove} mode)
            (not= type :host))))

(defn ^:private trans-xf [x y]
  (comp (partition-all 2) (drop 1) (map (fn [[ax ay]] [(+ ax x) (+ ay y)])) cat))

(defn ^:private initiative-order [a b]
  (let [f (juxt :initiative/roll :db/key)]
    (compare (f b) (f a))))

(defmulti transact (fn [{:keys [event]}] event))

(defmethod transact :default [] [])

;; -- Local --
(defmethod
  ^{:doc "Change the rendering status of the given local user. This status
          is used to decide what the top-level rendering mode is in order
          to, for example, render an error page when a WebSocket connection
          has failed."}
  transact :local/change-status
  [{:keys [local]} status]
  [[:db/add -1 :db/key local]
   [:db/add -1 :local/status status]])

(defmethod
  ^{:doc "Changes whether or not toolbar tooltips are displayed when the
          local user hovers over them."}
  transact :local/toggle-tooltips
  [{:keys [local]} display?]
  [{:db/id -1 :db/key local :local/tooltips? display?}])

(defmethod
  ^{:doc "Changes the currently expanded panel form to the value given by
          `panel`."}
  transact :local/toggle-panel
  [{:keys [local]} panel]
  [{:db/id -1 :db/key local :panel/expanded #{panel}}])

(defmethod
  ^{:doc "Changes the current keyboard modifier for the local user. This
          modifier is currently only used to determine if the 'Shift' key is
          depressed so that users can draw a selection box across the scene,
          selecting more than one token."}
  transact :local/modifier-start
  [{:keys [local]} modifier]
  [[:db/add -1 :db/key local]
   [:db/add -1 :local/modifier modifier]])

(defmethod
  ^{:doc "Releases the given keyboard modifier for the local user."}
  transact :local/modifier-release
  [{:keys [local]}]
  [[:db/add -1 :db/key local]
   [:db/retract [:db/key local] :local/modifier]])

;; -- Camera --
(defmethod
  ^{:doc "Changes the public label for the current camera."}
  transact :camera/change-label
  [{:keys [camera]} label]
  [[:db/add [:db/key camera] :camera/label label]])

(defmethod
  ^{:doc "Removes the public label for the current camera."}
  transact :camera/remove-label
  [{:keys [camera]}]
  [[:db/retract [:db/key camera] :camera/label]])

(defmethod
  ^{:doc "Translates the current camera to the point given by `x` and `y`."}
  transact :camera/translate
  [{:keys [camera]} x y]
  [[:db/add -1 :db/key camera]
   [:db/add -1 :camera/point [(round x 1) (round y 1)]]])

(defmethod
  ^{:doc "Changes the camera draw mode to the given value. The draw mode is
          used to decide what behavior clicking and dragging on the scene
          will have, such as drawing a shape or determining the distance
          between two points."}
  transact :camera/change-mode
  [{:keys [data local camera]} mode]
  (let [{type :local/type}       (ds/entity data [:db/key local])
        allowed? (mode-allowed? mode type)]
    (if allowed?
      [[:db/add -1 :db/key camera]
       [:db/add -1 :camera/draw-mode mode]]
      [])))

(defmethod
  ^{:arglists '([context] [context value] [context value x y])
    :doc "Changes the zoom value for the current camera by the given value
          `next` and, optionally, a cursor point given by `x` and `y`.
          This method preserves the point of the cursor on the scene,
          adjusting the camera point to ensure that the user feels as if
          they are zooming in or out from their cursor."}
  transact :camera/zoom-change
  ([context]
   (transact context 1))
  ([{:keys [data local] :as context} next]
   (let [select    [[:bounds/self :default [0 0 0 0]]]
         result    (ds/pull data select [:db/key local])
         [_ _ w h] (:bounds/self result)]
     (transact context next (/ w 2) (/ h 2))))
  ([{:keys [data camera]} next x y]
   (let [select [[:camera/scale :default 1] [:camera/point :default [0 0]]]
         result (ds/pull data select [:db/key camera])
         {prev :camera/scale [cx cy] :camera/point} result
         fx (/ next prev)
         dx (/ (- (* x fx) x) next)
         dy (/ (- (* y fx) y) next)]
     [[:db/add -1 :db/key camera]
      [:db/add -1 :camera/point [(round (+ cx dx) 1) (round (+ cy dy) 1)]]
      [:db/add -1 :camera/scale next]])))

(defmethod
  ^{:arglists '([context delta x y])
    :doc "Changes the zoom value for the current camera by offsetting it from
          the given value `delta`. This is useful for zooming with a device
          that uses fine grained updates such as a mousewheel or a trackpad."}
  transact :camera/zoom-delta
  [{:keys [data camera] :as context} delta x y]
  (let [result (ds/pull data [[:camera/scale :default 1]] [:db/key camera])
        next   (-> (:camera/scale result)
                   (js/Math.log)
                   (+ delta)
                   (js/Math.exp)
                   (to-precision 2)
                   (constrain 0.15 4))]
    (transact (assoc context :event :camera/zoom-change) next x y)))

(defmethod
  ^{:doc "Increases the zoom value for the current camera to the next nearest
          zoom level. These fixed zoom levels are determined by an internal
          constant."}
  transact :camera/zoom-in
  [{:keys [data camera] :as context}]
  (let [info (ds/pull data [[:camera/scale :default 1]] [:db/key camera])
        prev (:camera/scale info)
        next (reduce (fn [n s] (if (> s prev) (reduced s) n)) prev zoom-scales)]
    (transact (assoc context :event :camera/zoom-change) next)))

(defmethod
  ^{:doc "Decreases the zoom value for the current camera to the nearest
          previous zoom level. These fixed zoom levels are determined by an
          internal constant."}
  transact :camera/zoom-out
  [{:keys [data camera] :as context}]
  (let [info (ds/pull data [[:camera/scale :default 1]] [:db/key camera])
        prev (:camera/scale info)
        next (reduce (fn [n s] (if (< s prev) (reduced s) n)) prev (reverse zoom-scales))]
    (transact (assoc context :event :camera/zoom-change) next)))

(defmethod
  ^{:doc "Resets the zoom value for the given camera to its default setting of
          100%."}
  transact :camera/zoom-reset [context]
  (transact (assoc context :event :camera/zoom-change) 1))

;; -- Scenes --
(defmethod
  ^{:doc "Creates a new blank scene and corresponding camera for the local user
          then switches them to it."}
  transact :scenes/create
  [{:keys [local]}]
  [[:db/add -1 :db/key (squuid)]
   [:db/add -2 :db/key (squuid)]
   [:db/add [:db/key local] :local/camera -1]
   [:db/add [:db/key local] :local/cameras -1]
   [:db/add -1 :camera/scene -2]
   [:db/add [:db/ident :root] :root/scenes -2]])

(defmethod
  ^{:doc "Switches to the given scene by the given camera identifier."}
  transact :scenes/change
  [{:keys [local]} key]
  [[:db/add -1 :db/key local]
   [:db/add -2 :db/key key]
   [:db/add -1 :local/camera -2]])

(defmethod
  ^{:doc "Removes the scene and corresponding camera for the local user. Also
          removes all scene cameras for any connected users and switches them
          to whichever scene the host is now on."}
  transact :scenes/remove
  [{:keys [data local]} key]
  (let [select-w [:db/key {:camera/scene [:db/key]}]
        select-l [:db/key {:local/cameras select-w :local/camera select-w}]
        select-r [{:root/local select-l} {:root/session [{:session/conns select-l}]}]
        select-o [:db/key {:camera/scene [:db/key {:camera/_scene [:db/key]}]}]

        {{scene :db/key
          remove :camera/_scene} :camera/scene}
        (ds/pull data select-o [:db/key key])

        {{conns   :session/conns} :root/session
         {camera  :local/camera
          cameras :local/cameras} :root/local}
        (ds/pull data select-r [:db/ident :root])]
    (cond
      (= (count cameras) 1)
      (into [[:db/retractEntity [:db/key scene]]
             [:db/add -1 :db/key (squuid)]
             [:db/add -1 :camera/scene -2]
             [:db/add -2 :db/key (squuid)]
             [:db/add [:db/key local] :local/camera -1]
             [:db/add [:db/key local] :local/cameras -1]]
            (comp cat cat)
            (list (for [{:keys [db/key]} remove]
                    [[:db/retractEntity [:db/key key]]])
                  (for [[idx conn] (sequence (indexed 3 2) conns)
                        :let [tmp (dec idx)]]
                    [[:db/add idx :db/key (:db/key conn)]
                     [:db/add idx :local/cameras tmp]
                     [:db/add idx :local/camera tmp]
                     [:db/add tmp :db/key (squuid)]
                     [:db/add tmp :camera/scene -2]
                     [:db/add tmp :camera/point [0 0]]
                     [:db/add tmp :camera/scale 1]])))

      (= key (:db/key camera))
      (let [host-cam (first (filter (comp-fn not= :db/key (:db/key camera)) cameras))
            host-scn (:db/key (:camera/scene host-cam))]
        (into [[:db/retractEntity [:db/key scene]]
               [:db/add -1 :db/key (:db/key host-cam)]
               [:db/add -2 :db/key local]
               [:db/add -2 :local/camera -1]]
              (comp cat cat)
              (list (for [{:keys [db/key]} remove]
                      [[:db/retractEntity [:db/key key]]])
                    (for [[idx conn] (sequence (indexed 3 2) conns)
                          :let [tmp (dec idx)
                                cam (->> (:local/cameras conn)
                                         (filter (comp-fn = (comp :db/key :camera/scene) host-scn))
                                         (first))]]
                      [[:db/add idx :db/key (:db/key conn)]
                       [:db/add idx :local/cameras tmp]
                       [:db/add idx :local/camera tmp]
                       [:db/add tmp :db/key (or (:db/key cam) (squuid))]
                       [:db/add tmp :camera/scene [:db/key host-scn]]
                       [:db/add tmp :camera/point [0 0]]
                       [:db/add tmp :camera/scale 1]]))))

      :else
      (into [[:db/retractEntity [:db/key scene]]]
            (for [{:keys [db/key]} remove]
              [:db/retractEntity [:db/key key]])))))

;; -- Scene Images --
(defmethod
  ^{:doc "Creates a new scene image with the given checksum, width, and height.
          Relates this entity to the root scene collection."}
  transact :scene-images/create
  [_ data]
  [{:db/ident :root :root/scene-images (with-ns data "image")}])

(defmethod
  ^{:doc "Removes the scene image by the given identifying checksum."}
  transact :scene-images/remove
  [_ checksum]
  [[:db/retractEntity [:image/checksum checksum]]])

(defmethod
  ^{:doc "Removes all scene images."}
  transact :scene-images/remove-all [_]
  [[:db/retract [:db/ident :root] :root/scene-images]])

;; -- Scene --
(defmethod
  ^{:doc "Updates the image being used for the current scene by the given
          identifying checksum."}
  transact :scene/change-image
  [{:keys [data scene]} checksum]
  (let [{:image/keys [width height]} (ds/entity data [:image/checksum checksum])]
    [[:db/add -1 :db/key scene]
     [:db/add -1 :scene/image -2]
     [:db/add -2 :image/checksum checksum]
     [:db/add -2 :image/width width]
     [:db/add -2 :image/height height]]))

(defmethod
  ^{:doc "Updates the grid size for the current scene."}
  transact :scene/change-grid-size
  [{:keys [scene]} size]
  [[:db/add -1 :db/key scene]
   [:db/add -1 :scene/grid-size size]])

(defmethod
  ^{:doc "Applies both a grid origin and tile size to the current scene."}
  transact :scene/apply-grid-options
  [{:keys [camera scene]} origin size]
  [[:db/add -1 :db/key camera]
   [:db/add -2 :db/key scene]
   [:db/add -2 :scene/grid-origin origin]
   [:db/add -2 :scene/grid-size size]
   [:db/add -1 :camera/draw-mode :select]])

(defmethod
  ^{:doc "Resets the grid origin to (0, 0)."}
  transact :scene/reset-grid-origin
  [{:keys [camera scene]}]
  [[:db/add [:db/key camera] :camera/draw-mode :select]
   [:db/retract [:db/key scene] :scene/grid-origin]])

(defmethod
  ^{:doc "Retracts the grid size for the current scene, allowing queries to
          revert to their defaults."}
  transact :scene/retract-grid-size
  [{:keys [scene]}]
  [[:db/add -1 :db/key scene]
   [:db/retract [:db/key scene] :scene/grid-size]])

(defmethod
  ^{:doc "Updates whether or not the grid is drawn onto the current scene."}
  transact :scene/toggle-show-grid
  [{:keys [scene]} value]
  [[:db/add -1 :db/key scene]
   [:db/add -1 :scene/show-grid value]])

(defmethod
  ^{:doc "Updates whether or not dark mode is enabled on the current scene."}
  transact :scene/toggle-dark-mode
  [{:keys [scene]} enabled]
  [[:db/add -1 :db/key scene]
   [:db/add -1 :scene/dark-mode enabled]])

(defmethod
  ^{:doc "Updates the lighting option used for the current scene."}
  transact :scene/change-lighting
  [{:keys [scene]} value]
  [[:db/add -1 :db/key scene]
   [:db/add -1 :scene/lighting value]])

(defmethod
  ^{:doc "Updates the time of day option used for the current scene."}
  transact :scene/change-time-of-day
  [{:keys [scene]} value]
  [[:db/add -1 :db/key scene]
   [:db/add -1 :scene/timeofday value]])

(defmethod transact :element/update
  [_ keys attr value]
  (for [[id key] (sequence (indexed) keys)]
    (assoc {:db/id id :db/key key} attr value)))

(defmethod transact :element/select
  [{:keys [data camera]} key replace?]
  (let [lookup [:db/key key]
        entity (ds/entity data lookup)]
    [[:db/add -1 :db/key camera]
     (if replace?
       [:db/retract [:db/key camera] :camera/selected])
     (if (and (not replace?) (:camera/_selected entity))
       [:db/retract [:db/key camera] :camera/selected lookup]
       [:db/add -1 :camera/selected lookup])]))

(defmethod transact :element/remove
  [_ keys]
  (for [key keys]
    [:db/retractEntity [:db/key key]]))

(defmethod transact :token/create
  [{:keys [camera scene]} x y checksum]
  [[:db/add -1 :db/key (squuid)]
   [:db/add -1 :token/point [(round x 1) (round y 1)]]
   [:db/add -1 :token/image -4]
   [:db/add -2 :db/key camera]
   [:db/add -2 :camera/selected -1]
   [:db/add -2 :camera/draw-mode :select]
   [:db/add -3 :db/key scene]
   [:db/add -3 :scene/tokens -1]
   [:db/add -4 :image/checksum checksum]])

(defmethod transact :token/remove
  [{:keys [data scene]} keys]
  (let [keys (set keys)
        cnvs (ds/entity data [:db/key scene])
        curr (->> (:initiative/turn cnvs) :db/key)
        tkns (->> (:scene/initiative cnvs) (sort initiative-order) (map :db/key))
        tkfn (complement (partial contains? (disj keys curr)))
        next (->> (filter tkfn tkns) (find-next (partial = curr)))
        data {:db/key scene :initiative/turn {:db/key (or next (first tkns))}}]
    (cond-> (for [key keys] [:db/retractEntity [:db/key key]])
      (contains? keys curr) (conj data))))

(defmethod transact :token/translate
  [_ token x y]
  [[:db/add -1 :db/key token]
   [:db/add -1 :token/point [(round x 1) (round y 1)]]])

(defmethod transact :token/change-flag
  [{:keys [data]} keys flag add?]
  (let [idents (map (fn [key] [:db/key key]) keys)
        tokens (ds/pull-many data [:db/key :token/flags] idents)]
    (for [[id {:keys [db/key token/flags] :or {flags #{}}}] (sequence (indexed) tokens)]
      {:db/id id :db/key key :token/flags ((if add? conj disj) flags flag)})))

(defmethod transact :token/translate-all
  [{:keys [data]} keys x y]
  (let [lookup (map (fn [key] [:db/key key]) keys)
        tokens (ds/pull-many data [:db/key :token/point] lookup)]
    (for [[id {key :db/key [tx ty] :token/point}] (sequence (indexed) tokens)]
      {:db/id id :db/key key :token/point [(round (+ x tx) 1) (round (+ y ty) 1)]})))

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

(defmethod transact :shape/create
  [{:keys [camera scene]} kind vecs]
  [[:db/add -1 :db/key (squuid)]
   [:db/add -1 :shape/kind kind]
   [:db/add -1 :shape/vecs vecs]
   [:db/add -2 :db/key scene]
   [:db/add -2 :scene/shapes -1]
   [:db/add -3 :db/key camera]
   [:db/add -3 :camera/draw-mode :select]
   [:db/add -3 :camera/selected -1]])

(defmethod transact :shape/remove
  [_ keys]
  (for [key keys]
    [:db/retractEntity [:db/key key]]))

(defmethod transact :shape/translate
  [{:keys [data]} key x y]
  (let [select [:shape/vecs]
        result (ds/pull data select [:db/key key])
        {[ax ay] :shape/vecs
         vecs    :shape/vecs} result
        x (round x 1)
        y (round y 1)]
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
  [{:keys [data local camera scene]} vecs]
  (let [select [{:scene/tokens [:db/key :token/point [:token/flags :default #{}]]}]
        local  (ds/pull data [:local/type] [:db/key local])
        result (ds/pull data select [:db/key scene])
        bounds (normalize vecs)]
    [{:db/id -1
      :db/key camera
      :camera/draw-mode :select
      :camera/selected
      (for [[idx token] (sequence (indexed 2) (:scene/tokens result))
            :let  [{[x y] :token/point flags :token/flags key :db/key} token]
            :when (and (within? x y bounds)
                       (or (= (:local/type local) :host)
                           (not (flags :hidden))))]
        {:db/id idx :db/key key})}]))

(defmethod transact :selection/clear
  [{:keys [camera]}]
  [[:db/add -1 :db/key camera]
   [:db/retract [:db/key camera] :camera/selected]])

(defmethod transact :selection/remove
  [{:keys [data camera] :as ctx}]
  (let [select [{:camera/selected [:db/key :scene/_tokens :scene/_shapes]}]
        result (ds/pull data select [:db/key camera])
        groups (group-by (fn [x] (cond (:scene/_tokens x) :token
                                       (:scene/_shapes x) :shape))
                         (:camera/selected result))]
    (concat (transact (assoc ctx :event :token/remove) (map :db/key (:token groups)))
            (transact (assoc ctx :event :shape/remove) (map :db/key (:shape groups))))))

(defmethod transact :initiative/toggle
  [{:keys [data scene]} keys adding?]
  (let [tokens (map (fn [key] [:db/key key]) keys)
        select [{:token/image [:image/checksum]}
                [:token/flags :default #{}]
                :db/key
                :token/label
                :initiative/suffix]
        result (ds/pull data [{:scene/initiative select}] [:db/key scene])
        change (into #{} (ds/pull-many data select tokens))
        exists (into #{} (:scene/initiative result))]
    (if adding?
      [{:db/id -1
        :db/key scene
        :scene/initiative
        (let [merge (union exists change)
              sffxs (suffixes merge)]
          (for [[idx token] (sequence (indexed 2) merge) :let [key (:db/key token)]]
            (if-let [suffix (sffxs key)]
              {:db/id idx :db/key key :initiative/suffix suffix}
              {:db/id idx :db/key key})))}]
      (apply concat
             [[:db/add -1 :db/key scene]]
             (for [[idx {key :db/key}] (sequence (indexed 2) change)]
               [[:db/add idx :db/key key]
                [:db/retract [:db/key key] :initiative/suffix]
                [:db/retract [:db/key key] :initiative/roll]
                [:db/retract [:db/key key] :initiative/health]
                [:db/retract [:db/key scene] :scene/initiative idx]])))))

(defmethod transact :initiative/next
  [{:keys [data scene]}]
  (let [{curr :initiative/turn
         trns :initiative/turns
         rnds :initiative/rounds
         tkns :scene/initiative} (ds/entity data [:db/key scene])
        tkns (->> tkns (sort initiative-order) (map :db/key))]
    (if (nil? rnds)
      [{:db/key scene
        :initiative/turn {:db/key (first tkns)}
        :initiative/turns 0
        :initiative/rounds 1}]
      (if-let [next (find-next (partial = (:db/key curr)) tkns)]
        [{:db/key scene
          :initiative/turn {:db/key next}
          :initiative/turns (inc trns)}]
        [{:db/key scene
          :initiative/turn {:db/key (first tkns)}
          :initiative/turns (inc trns)
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
  [{:keys [data scene]}]
  (let [select [{:scene/initiative [:db/key :token/flags :initiative/roll]}]
        result (ds/pull data select [:db/key scene])
        tokens (:scene/initiative result)]
    (for [[idx token] (sequence (indexed) tokens)
          :let  [{:keys [db/key token/flags initiative/roll]} token]
          :when (and (nil? roll) (not (contains? flags :player)))]
      {:db/id idx :db/key key :initiative/roll (inc (rand-int 20))})))

(defmethod transact :initiative/reset
  [{:keys [data scene]}]
  (let [result (ds/pull data [{:scene/initiative [:db/key]}] [:db/key scene])]
    (->> (for [[idx token] (sequence (indexed 2) (:scene/initiative result))
               :let [{key :db/key} token]]
           [[:db/add idx :db/key key]
            [:db/retract [:db/key key] :initiative/roll]])
         (into [[:db/add -1 :db/key scene]
                [:db/retract [:db/key scene] :initiative/turn]
                [:db/retract [:db/key scene] :initiative/turns]
                [:db/retract [:db/key scene] :initiative/rounds]] cat))))

(defmethod transact :initiative/change-health
  [{:keys [data]} key f value]
  (let [parsed (.parseFloat js/window value)]
    (if (.isNaN js/Number parsed) []
        (let [{:keys [initiative/health]} (ds/entity data [:db/key key])]
          [{:db/id -1 :db/key key :initiative/health (f health parsed)}]))))

(defmethod transact :initiative/leave
  [{:keys [data scene]}]
  (let [select [{:scene/initiative [:db/key]}]
        result (ds/pull data select [:db/key scene])
        tokens (:scene/initiative result)]
    (apply concat
           [[:db/add -1 :db/key scene]
            [:db/retract [:db/key scene] :scene/initiative]
            [:db/retract [:db/key scene] :initiative/turn]
            [:db/retract [:db/key scene] :initiative/turns]
            [:db/retract [:db/key scene] :initiative/rounds]]
           (for [[idx {key :db/key}] (sequence (indexed 2) tokens)]
             [[:db/add idx :db/key key]
              [:db/retract [:db/key key] :initiative/roll]
              [:db/retract [:db/key key] :initiative/health]
              [:db/retract [:db/key key] :initiative/suffix]]))))

(defmethod transact :tokens/create
  [_ data scope]
  (let [data (assoc data :image/scope scope)]
    [{:db/ident :root :root/token-images (with-ns data "image")}]))

(defmethod
  ^{:doc "Change the scope of the token image by the given checksum to the
          given scope, typically `:public` or `:private`."}
  transact :tokens/change-scope
  [_ checksum scope]
  [[:db/add -1 :image/checksum checksum]
   [:db/add -1 :image/scope scope]])

(defmethod transact :tokens/remove [_ checksum]
  [[:db/retractEntity [:image/checksum checksum]]])

(defmethod transact :tokens/remove-all []
  [[:db/retract [:db/ident :root] :root/token-images]])

(defmethod transact :mask/fill
  [{:keys [scene]}]
  [[:db/add -1 :db/key scene]
   [:db/add -1 :mask/filled? true]
   [:db/retract [:db/key scene] :scene/masks]])

(defmethod transact :mask/clear
  [{:keys [scene]}]
  [[:db/add -1 :db/key scene]
   [:db/add -1 :mask/filled? false]
   [:db/retract [:db/key scene] :scene/masks]])

(defmethod transact :mask/create
  [{:keys [scene]} state vecs]
  [[:db/add -1 :db/key scene]
   [:db/add -1 :scene/masks -2]
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
  [{:db/id -1 :db/key local :local/share-cursor enabled}])

(defmethod transact :session/focus
  [{:keys [data]}]
  (let [select-w [:camera/scene [:camera/point :default [0 0]] [:camera/scale :default 1]]
        select-l [:db/key [:bounds/self :default [0 0 0 0]] {:local/cameras [:camera/scene] :local/camera select-w}]
        select-s [{:session/host select-l} {:session/conns select-l}]
        result   (ds/pull data select-s [:db/ident :session])
        {{[_ _ hw hh] :bounds/self
          {[hx hy] :camera/point} :local/camera
          host :local/camera} :session/host
         conns :session/conns} result
        scale (:camera/scale host)
        mx (+ (/ hw scale 2) hx)
        my (+ (/ hh scale 2) hy)]
    (->> (for [[next conn] (sequence (indexed 1 2) conns)
               :let [prev (dec next)
                     exst (->> (:local/cameras conn)
                               (filter (fn [conn]
                                         (= (:db/key (:camera/scene conn))
                                            (:db/key (:camera/scene host)))))
                               (first)
                               (:db/key))
                     [_ _ cw ch] (:bounds/self conn)
                     cx (- mx (/ cw scale 2))
                     cy (- my (/ ch scale 2))]]
           [[:db/add next :db/key (:db/key conn)]
            [:db/add next :local/camera prev]
            [:db/add next :local/cameras prev]
            [:db/add prev :db/key        (or exst (squuid))]
            [:db/add prev :camera/point  [cx cy]]
            [:db/add prev :camera/scale  scale]
            [:db/add prev :camera/scene (:db/id (:camera/scene host))]])
         (into [] cat))))

;; -- Clipboard --
(defmethod
  ^{:doc "Copy the currently selected tokens to the clipboard. Optionally
          removes them from the current scene if cut? is passed as true.
          The clipboard contains a template for the token data, and not
          references to the tokens themselves since those references
          don't exist after they are pruned from the scene. Only some token
          data is copied; transient state like that related to initiative is
          not preserved."}
  transact :clipboard/copy
  ([context]
   (transact context false))
  ([{:keys [data local camera]} cut?]
   (let [attrs  [:token/label :token/flags :token/light :token/size :aura/radius :token/image :token/point]
         select [{:camera/selected (into attrs [:db/key :scene/_tokens {:token/image [:image/checksum]}])}]
         result (ds/pull data select [:db/key camera])
         tokens (filter (comp-fn contains? identity :scene/_tokens) (:camera/selected result))
         copies (into [] (map (comp-fn select-keys identity attrs)) tokens)]
     (cond-> []
       (seq tokens)
       (into [[:db/add -1 :db/key local]
              [:db/add -1 :local/clipboard copies]])
       (and (seq tokens) cut?)
       (into (for [{key :db/key} tokens]
               [:db/retractEntity [:db/key key]]))))))

(def ^:private clipboard-paste-select
  [{:root/local
    [[:local/clipboard :default []]
     [:bounds/self :default [0 0 0 0]]
     {:local/camera
      [[:camera/scale :default 1]
       [:camera/point :default [0 0]]]}]}
   {:root/token-images [:image/checksum]}])

(defmethod
  ^{:doc "Creates tokens on the current scene from the data stored in the local
          user's clipboard. Attempts to preserve the relative position of
          the tokens when they were copied but in the center of the user's
          viewport. Clipboard data is not pruned after pasting."}
  transact :clipboard/paste
  [{:keys [data camera scene]}]
  (let [result (ds/pull data clipboard-paste-select [:db/ident :root])
        {{clipboard :local/clipboard
          [_ _ sw sh] :bounds/self
          {scale :camera/scale
           [cx cy] :camera/point} :local/camera} :root/local
         images :root/token-images} result
        hashes (into #{} (map :image/checksum) images)
        [ax ay bx by] (apply bounding-box (map :token/point clipboard))
        sx (+ (/ sw scale 2) cx)
        sy (+ (/ sh scale 2) cy)
        ox (/ (- ax bx) 2)
        oy (/ (- ay by) 2)]
    (->> (for [[temp token] (sequence (indexed 3) clipboard)
               :let [[tx ty] (:token/point token)
                     hash    (:image/checksum (:token/image token))
                     data    (merge token {:db/id       temp
                                           :db/key      (squuid)
                                           :token/image [:image/checksum (or (hashes hash) "default")]
                                           :token/point [(+ sx tx ox (- ax)) (+ sy ty oy (- ay))]})]]
           [{:db/id -1 :db/key camera :camera/selected temp}
            {:db/id -2 :db/key scene :scene/tokens data}])
         (into [] cat))))

;; -- Shortcuts --
(defmethod
  ^{:doc "Handles the 'Escape' keyboard shortcut, clearing any token
          selections and changing the mode to `select`."}
  transact :shortcut/escape
  [{:keys [camera]}]
  [[:db/add [:db/key camera] :db/key camera]
   [:db/add [:db/key camera] :camera/draw-mode :select]
   [:db/retract [:db/key camera] :camera/selected]])
