(ns ogres.app.events
  (:require [datascript.core :as ds]
            [clojure.set :refer [union]]
            [clojure.string :refer [trim]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.geom :refer [bounding-box within?]]
            [ogres.app.util :refer [comp-fn round round-grid with-ns]]))

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
                   (assoc result (:db/id token) (+ (offset group) (index group) 1)))))
        result))))

(defn ^:private to-precision [n p]
  (js/Number (.toFixed (js/Number.parseFloat n) p)))

(defn ^:private constrain [n min max]
  (clojure.core/max (clojure.core/min n max) min))

(defn ^:private mode-allowed? [mode type]
  (not (and (contains? #{:mask :mask-toggle :mask-remove :grid} mode)
            (not= type :host))))

(defn ^:private trans-xf [x y]
  (comp (partition-all 2) (drop 1) (map (fn [[ax ay]] [(+ ax x) (+ ay y)])) cat))

(defn ^:private initiative-order [a b]
  (let [f (juxt :initiative/roll :db/id)]
    (compare (f b) (f a))))

(defmulti event-tx-fn (fn [_ event] event))

(defmethod event-tx-fn :default [] [])

;; -- Local --
(defmethod
  ^{:doc "Change the rendering status of the given local user. This status
          is used to decide what the top-level rendering mode is in order
          to, for example, render an error page when a WebSocket connection
          has failed."}
  event-tx-fn :local/change-status
  [_ _ status]
  [{:db/ident :local :local/status status}])

(defmethod
  ^{:doc "Change the selected panel to the keyword given by `panel`."}
  event-tx-fn :local/select-panel
  [_ _ panel]
  [{:db/ident :local :panel/selected panel :panel/expanded true}])

(defmethod
  ^{:doc "Toggle the expanded state of the panel."}
  event-tx-fn :local/toggle-panel
  [data]
  (let [local (ds/entity data [:db/ident :local])]
    [{:db/ident :local :panel/expanded (not (get local :panel/expanded true))}]))

(defmethod
  ^{:doc "Changes the current keyboard modifier for the local user. This
          modifier is currently only used to determine if the 'Shift' key is
          depressed so that users can draw a selection box across the scene,
          selecting more than one token."}
  event-tx-fn :local/modifier-start
  [_ _ modifier]
  [{:db/ident :local :local/modifier modifier}])

(defmethod
  ^{:doc "Releases the given keyboard modifier for the local user."}
  event-tx-fn :local/modifier-release
  []
  [[:db/retract [:db/ident :local] :local/modifier]])

;; -- Camera --
(defn ^:private assoc-camera
  [data & kvs]
  (let [local (ds/entity data [:db/ident :local])]
    [(apply assoc {:db/id (:db/id (:local/camera local))} kvs)]))

(defmethod
  ^{:doc "Changes the public label for the current camera."}
  event-tx-fn :camera/change-label
  [_ _ label]
  [[:db.fn/call assoc-camera :camera/label label]])

(defmethod
  ^{:doc "Removes the public label for the current camera."}
  event-tx-fn :camera/remove-label
  [data]
  (let [local (ds/entity data [:db/ident :local])]
    [[:db/retract (:db/id (:local/camera local)) :camera/label]]))

(defmethod
  ^{:doc "Translates the current camera to the point given by `x` and `y`."}
  event-tx-fn :camera/translate
  [_ _ x y]
  [[:db.fn/call assoc-camera :camera/point [(round x) (round y)]]])

(defmethod
  ^{:doc "Changes the camera draw mode to the given value. The draw mode is
          used to decide what behavior clicking and dragging on the scene
          will have, such as drawing a shape or determining the distance
          between two points."}
  event-tx-fn :camera/change-mode
  [data _ mode]
  (let [local (ds/entity data [:db/ident :local])]
    (if (mode-allowed? mode (:local/type local))
      [{:db/id (:db/id (:local/camera local)) :camera/draw-mode mode}]
      [])))

(defmethod
  ^{:doc "Changes the zoom value for the current camera by the given value
          `next` and, optionally, a cursor point given by `x` and `y`.
          This method preserves the point of the cursor on the scene,
          adjusting the camera point to ensure that the user feels as if
          they are zooming in or out from their cursor."}
  event-tx-fn :camera/zoom-change
  ([data event & args]
   (let [local (ds/entity data [:db/ident :local])]
     (case (count args)
       0 [[:db.fn/call event-tx-fn event 1]]
       1 (let [[scale] args
               [_ _ w h] (or (:bounds/self local) [0 0 0 0])]
           [[:db.fn/call event-tx-fn event scale (/ w 2) (/ h 2)]])
       (let [[scale x y] args
             camera  (:local/camera local)
             [cx cy] (or (:camera/point camera) [0 0])
             prev    (or (:camera/scale camera) 1)
             fx      (/ scale prev)
             dx      (/ (- (* x fx) x) scale)
             dy      (/ (- (* y fx) y) scale)]
         [{:db/id (:db/id camera)
           :camera/point [(round (+ cx dx)) (round (+ cy dy))]
           :camera/scale scale}])))))

(defmethod
  ^{:doc "Changes the zoom value for the current camera by offsetting it from
          the given value `delta`. This is useful for zooming with a device
          that uses fine grained updates such as a mousewheel or a trackpad."}
  event-tx-fn :camera/zoom-delta
  [data _ delta x y]
  (let [local  (ds/entity data [:db/ident :local])
        scale  (-> (:camera/scale (:local/camera local)) (or 1) (js/Math.log)
                   (+ delta) (js/Math.exp) (to-precision 2) (constrain 0.15 4))]
    [[:db.fn/call event-tx-fn :camera/zoom-change scale x y]]))

(defmethod
  ^{:doc "Increases the zoom value for the current camera to the next nearest
          zoom level. These fixed zoom levels are determined by an internal
          constant."}
  event-tx-fn :camera/zoom-in
  [data]
  (let [local  (ds/entity data [:db/ident :local])
        camera (:local/camera local)
        prev   (or (:camera/scale camera) 1)
        next   (reduce (fn [n s] (if (> s prev) (reduced s) n)) prev zoom-scales)]
    [[:db.fn/call event-tx-fn :camera/zoom-change next]]))

(defmethod
  ^{:doc "Decreases the zoom value for the current camera to the nearest
          previous zoom level. These fixed zoom levels are determined by an
          internal constant."}
  event-tx-fn :camera/zoom-out
  [data]
  (let [local  (ds/entity data [:db/ident :local])
        camera (:local/camera local)
        prev   (or (:camera/scale camera) 1)
        next   (reduce (fn [n s] (if (< s prev) (reduced s) n)) prev (reverse zoom-scales))]
    [[:db.fn/call event-tx-fn :camera/zoom-change next]]))

(defmethod
  ^{:doc "Resets the zoom value for the given camera to its default setting of
          100%."}
  event-tx-fn :camera/zoom-reset
  []
  [[:db.fn/call event-tx-fn :camera/zoom-change 1]])

;; -- Scenes --
(defmethod
  ^{:doc "Creates a new blank scene and corresponding camera for the local user
          then switches them to it."}
  event-tx-fn :scenes/create
  []
  [{:db/ident    :root
    :root/scenes {:db/id -1 :db/empty true}
    :root/local
    {:db/ident :local
     :local/camera -2
     :local/cameras {:db/id -2 :camera/scene -1}}}])

(defmethod
  ^{:doc "Switches to the given scene by the given camera identifier."}
  event-tx-fn :scenes/change
  [_ _ id]
  [{:db/ident :local :local/camera id}])

(defmethod
  ^{:doc "Removes the scene and corresponding camera for the local user. Also
          removes all scene cameras for any connected users and switches them
          to whichever scene the host is now on."}
  event-tx-fn :scenes/remove
  [data _ id]
  (let [camera  (ds/entity data id)
        local   (ds/entity data [:db/ident :local])
        session (ds/entity data [:db/ident :session])]
    (cond
      (= (count (:local/cameras local)) 1)
      (into [[:db/retractEntity (:db/id camera)]
             [:db/retractEntity (:db/id (:camera/scene camera))]
             {:db/ident :root
              :root/scenes {:db/id -2 :db/empty true}
              :root/local
              {:db/ident :local
               :local/camera -1
               :local/cameras {:db/id -1 :camera/scene -2}}}]
            (for [[idx conn] (sequence (indexed 3 2) (:session/conns session))]
              {:db/id (:db/id conn)
               :local/camera idx
               :local/cameras
               {:db/id idx
                :camera/scene -2
                :camera/point [0 0]
                :camera/scale 1}}))
      (= id (:db/id (:local/camera local)))
      (let [host-cam (first (filter (comp-fn not= :db/id id) (:local/cameras local)))
            host-scn (:db/id (:camera/scene host-cam))]
        (into [[:db/retractEntity (:db/id camera)]
               [:db/retractEntity (:db/id (:camera/scene camera))]
               {:db/ident :local :local/camera {:db/id (:db/id host-cam)}}]
              (for [[tmp conn] (sequence (indexed) (:session/conns session))
                    :let [cam (->> (:local/cameras conn)
                                   (filter (comp-fn = (comp :db/id :camera/scene) host-scn))
                                   (first))
                          idx (or (:db/id cam) tmp)]]
                {:db/id (:db/id conn)
                 :local/camera idx
                 :local/cameras
                 {:db/id idx
                  :camera/scene host-scn
                  :camera/point [0 0]
                  :camera/scale 1}})))
      :else
      [[:db/retractEntity (:db/id camera)]
       [:db/retractEntity (:db/id (:camera/scene camera))]])))

;; -- Scene Images --
(defmethod
  ^{:doc "Creates a new scene image with the given checksum, width, and height.
          Relates this entity to the root scene collection."}
  event-tx-fn :scene-images/create
  [_ _ image-data]
  (let [keys [:name :size :checksum :width :height]]
    [{:db/ident :root :root/scene-images (with-ns (select-keys image-data keys) "image")}]))

(defmethod
  ^{:doc "Removes the scene image by the given identifying checksum."}
  event-tx-fn :scene-images/remove
  [_ _ checksum]
  [[:db/retractEntity [:image/checksum checksum]]])

(defmethod
  ^{:doc "Removes all scene images."}
  event-tx-fn :scene-images/remove-all
  []
  [[:db/retract [:db/ident :root] :root/scene-images]])

;; -- Scene --
(defn ^:private assoc-scene
  [data & kvs]
  (let [local (ds/entity data [:db/ident :local])
        scene (:db/id (:camera/scene (:local/camera local)))]
    [(apply assoc {:db/id scene} kvs)]))

(defmethod
  ^{:doc "Updates the image being used for the current scene by the given
          identifying checksum."}
  event-tx-fn :scene/change-image
  [_ _ checksum]
  [[:db.fn/call assoc-scene :scene/image {:image/checksum checksum}]])

(defmethod
  ^{:doc "Updates the grid size for the current scene."}
  event-tx-fn :scene/change-grid-size
  [_ _ size]
  [[:db.fn/call assoc-scene :scene/grid-size size]])

(defmethod
  ^{:doc "Applies both a grid origin and tile size to the current scene."}
  event-tx-fn :scene/apply-grid-options
  [data _ origin size]
  (let [local (ds/entity data [:db/ident :local])]
    [{:db/id (:db/id (:local/camera local))
      :camera/draw-mode :select
      :camera/scene
      {:db/id (:db/id (:camera/scene (:local/camera local)))
       :scene/grid-size size
       :scene/grid-origin origin}}]))

(defmethod
  ^{:doc "Resets the grid origin to (0, 0)."}
  event-tx-fn :scene/reset-grid-origin
  [data]
  (let [local (ds/entity data [:db/ident :local])
        scene (:db/id (:camera/scene (:local/camera local)))]
    [[:db.fn/call assoc-camera :camera/draw-mode :select]
     [:db/retract scene :scene/grid-origin]]))

(defmethod
  ^{:doc "Retracts the grid size for the current scene, allowing queries to
          revert to their defaults."}
  event-tx-fn :scene/retract-grid-size
  [data]
  (let [local (ds/entity data [:db/ident :local])
        scene (:db/id (:camera/scene (:local/camera local)))]
    [[:db/retract scene :scene/grid-size]]))

(defmethod
  ^{:doc "Updates whether or not the grid is drawn onto the current scene."}
  event-tx-fn :scene/toggle-show-grid
  [_ _ value]
  [[:db.fn/call assoc-scene :scene/show-grid value]])

(defmethod
  ^{:doc "Updates whether or not dark mode is enabled on the current scene."}
  event-tx-fn :scene/toggle-dark-mode
  [_ _ enabled]
  [[:db.fn/call assoc-scene :scene/dark-mode enabled]])

(defmethod
  ^{:doc "Updates whether or not align to grid is enabled on the current scene."}
  event-tx-fn :scene/toggle-grid-align
  [_ _ enabled]
  [[:db.fn/call assoc-scene :scene/grid-align enabled]])

(defmethod
  ^{:doc "Updates the lighting option used for the current scene."}
  event-tx-fn :scene/change-lighting
  [_ _ value]
  [[:db.fn/call assoc-scene :scene/lighting value]])

(defmethod event-tx-fn :element/update
  [_ _ idxs attr value]
  (for [id idxs]
    (assoc {:db/id id} attr value)))

(defmethod event-tx-fn :element/select
  ([_ event id]
   [[:db.fn/call event-tx-fn event id false]])
  ([data _ id shift?]
   (let [local    (ds/entity data [:db/ident :local])
         entity   (ds/entity data id)
         camera   (:db/id (:local/camera local))
         selected (contains? (:camera/selected (:local/camera local)) entity)]
     [[:db/retract [:db/ident :local] :local/dragging]
      (if (not shift?)
        [:db/retract camera :camera/selected])
      (if (and shift? selected)
        [:db/retract camera :camera/selected id]
        {:db/id camera :camera/selected {:db/id id}})])))

(defmethod event-tx-fn :element/remove
  [_ _ idxs]
  (for [id idxs]
    [:db/retractEntity id]))

(defmethod
  ^{:doc "Creates a new token on the current scene at the screen coordinates
          given by `sx` and `sy`. These coordinates are converted to the
          scene coordinate space."}
  event-tx-fn :token/create
  [data _ sx sy checksum]
  (let [local (ds/entity data [:db/ident :local])
        {{[cx cy] :camera/point
          scale :camera/scale
          {align? :scene/grid-align
           [ox oy] :scene/grid-origin}
          :camera/scene} :local/camera} local
        tx (+ (/ sx (or scale 1)) (or cx 0))
        ty (+ (/ sy (or scale 1)) (or cy 0))]
    [{:db/id -1
      :token/point
      (if align?
        [(round-grid tx (/ grid-size 2) (mod ox grid-size))
         (round-grid ty (/ grid-size 2) (mod oy grid-size))]
        [(round tx) (round ty)])
      :token/image (if (some? checksum) {:image/checksum checksum} {})
      :local/camera
      {:db/id (:db/id (:local/camera local))
       :camera/selected -1
       :camera/draw-mode :select
       :camera/scene
       {:db/id (:db/id (:camera/scene (:local/camera local)))
        :scene/tokens -1}}}]))

(defmethod event-tx-fn :token/remove
  [data _ idxs]
  (let [local (ds/entity data [:db/ident :local])
        scene (:camera/scene (:local/camera local))
        idxs (set idxs)
        curr (->> (:initiative/turn scene) :db/id)
        tkns (->> (:scene/initiative scene) (sort initiative-order) (map :db/id))
        tkfn (complement (partial contains? (disj idxs curr)))
        next (->> (filter tkfn tkns) (find-next (partial = curr)))
        data {:db/id (:db/id scene) :initiative/turn (if-let [id (or next (first tkns))] {:db/id id} {})}]
    (cond-> (for [id idxs] [:db/retractEntity id])
      (contains? idxs curr) (conj data))))

(defmethod event-tx-fn :token/translate
  [_ _ id dx dy]
  [[:db.fn/call event-tx-fn :token/translate-all [id] dx dy]])

(defmethod event-tx-fn :token/translate-all
  [data _ idxs dx dy]
  (let [tokens (ds/pull-many data [:db/id :token/point [:token/size :default 5]] idxs)
        local  (ds/entity data [:db/ident :local])
        {{{[ox oy] :scene/grid-origin align? :scene/grid-align}
          :camera/scene} :local/camera} local]
    (into [[:db/retract [:db/ident :local] :local/dragging]]
          (for [{id :db/id size :token/size [tx ty] :token/point} tokens]
            {:db/id id :token/point
             (if align?
               (let [rd (* (/ size 5) (/ grid-size 2))]
                 [(round-grid (+ tx dx) rd (mod ox grid-size))
                  (round-grid (+ ty dy) rd (mod oy grid-size))])
               [(+ dx tx) (+ dy ty)])}))))

(defmethod event-tx-fn :token/change-flag
  [data _ idxs flag add?]
  (let [tokens (ds/pull-many data [:db/id :token/flags] idxs)]
    (for [{:keys [db/id token/flags] :or {flags #{}}} tokens]
      {:db/id id :token/flags ((if add? conj disj) flags flag)})))

(defmethod event-tx-fn :token/change-label
  [_ _ idxs value]
  (for [id idxs]
    {:db/id id :token/label (trim value)}))

(defmethod event-tx-fn :token/change-size
  [_ _ idxs radius]
  (for [id idxs]
    {:db/id id :token/size radius}))

(defmethod event-tx-fn :token/change-light
  [_ _ idxs radius]
  (for [id idxs]
    {:db/id id :token/light radius}))

(defmethod event-tx-fn :token/change-aura
  [_ _ idxs radius]
  (for [id idxs]
    {:db/id id :aura/radius radius}))

(defmethod event-tx-fn :shape/create
  [_ _ kind vecs]
  [{:db/id -1 :shape/kind kind :shape/vecs vecs}
   [:db.fn/call assoc-camera :camera/draw-mode :select :camera/selected -1]
   [:db.fn/call assoc-scene :scene/shapes -1]])

(defmethod event-tx-fn :shape/remove
  [_ _ idxs]
  (for [id idxs]
    [:db/retractEntity id]))

(defmethod event-tx-fn :shape/translate
  [data _ id dx dy]
  (let [result (ds/pull data [:shape/vecs] id)
        {[ax ay] :shape/vecs
         vecs    :shape/vecs} result
        x (round (+ ax dx))
        y (round (+ ay dy))]
    [[:db/retract [:db/ident :local] :local/dragging]
     {:db/id id :shape/vecs (into [x y] (trans-xf (- x ax) (- y ay)) vecs)}]))

(defmethod event-tx-fn :share/initiate [] [])

(defmethod event-tx-fn :share/toggle
  [data _ open?]
  (let [local (ds/entity data [:db/ident :local])]
    [{:db/ident          :local
      :local/sharing?    open?
      :local/privileged? (and (= (:local/type local) :host) open?)}]))

(defmethod event-tx-fn :bounds/change
  [data _ w-type bounds]
  (let [local (ds/entity data [:db/ident :local])]
    [[:db/add -1 :db/ident :local]
     (if (= w-type (:local/type local))
       [:db/add -1 :bounds/self bounds])
     [:db/add -1 (keyword :bounds w-type) bounds]]))

(defmethod event-tx-fn :selection/from-rect
  [data _ [ax ay bx by]]
  (let [root  (ds/entity data [:db/ident :root])
        local (:root/local root)
        bound (bounding-box [ax ay] [bx by])
        owned (into #{} (comp (mapcat :local/dragging) (map :db/id))
                    (:session/conns (:root/session root)))]
    [{:db/id (:db/id (:local/camera local))
      :camera/draw-mode :select
      :camera/selected
      (for [token (:scene/tokens (:camera/scene (:local/camera local)))
            :let  [{id :db/id [x y] :token/point flags :token/flags} token]
            :when (and (within? x y bound)
                       (not (owned id))
                       (or (= (:local/type local) :host)
                           (not ((or flags #{}) :hidden))))]
        {:db/id id})}]))

(defmethod event-tx-fn :selection/clear
  [data]
  (let [local (ds/entity data [:db/ident :local])]
    [[:db/retract (:db/id (:local/camera local)) :camera/selected]]))

(defmethod event-tx-fn :selection/remove
  [data]
  (let [local (ds/entity data [:db/ident :local])
        type  (->> (:camera/selected (:local/camera local))
                   (group-by (fn [x] (cond (:scene/_tokens x) :token (:scene/_shapes x) :shape)))
                   (first))]
    (case (first type)
      :token [[:db.fn/call event-tx-fn :token/remove (map :db/id (val type))]]
      :shape [[:db.fn/call event-tx-fn :shape/remove (map :db/id (val type))]]
      [])))

(defmethod event-tx-fn :initiative/toggle
  [data _ idxs adding?]
  (let [local  (ds/entity data [:db/ident :local])
        scene  (:db/id (:camera/scene (:local/camera local)))
        select [:db/id {:token/image [:image/checksum]} [:token/flags :default #{}] :initiative/suffix :token/label]
        result (ds/pull data [{:scene/initiative select}] scene)
        change (into #{} (ds/pull-many data select idxs))
        exists (into #{} (:scene/initiative result))]
    (if adding?
      [{:db/id scene
        :scene/initiative
        (let [merge (union exists change)
              sffxs (suffixes merge)]
          (for [token merge :let [id (:db/id token)]]
            (if-let [suffix (sffxs id)]
              {:db/id id :initiative/suffix suffix}
              {:db/id id})))}]
      (into [] cat
            (for [{id :db/id} change]
              [[:db/retract id :initiative/suffix]
               [:db/retract id :initiative/roll]
               [:db/retract id :initiative/health]
               [:db/retract scene :scene/initiative id]])))))

(defmethod event-tx-fn :initiative/next
  [data]
  (let [local (ds/entity data [:db/ident :local])
        scene (:camera/scene (:local/camera local))
        {curr :initiative/turn
         trns :initiative/turns
         rnds :initiative/rounds
         tkns :scene/initiative} scene
        tkns (map :db/id (sort initiative-order tkns))]
    (if (nil? rnds)
      [{:db/id (:db/id scene)
        :initiative/turn {:db/id (first tkns)}
        :initiative/turns 0
        :initiative/rounds 1}]
      (if-let [next (find-next (partial = (:db/id curr)) tkns)]
        [{:db/id (:db/id scene)
          :initiative/turn next
          :initiative/turns (inc trns)}]
        [{:db/id (:db/id scene)
          :initiative/turn (first tkns)
          :initiative/turns (inc trns)
          :initiative/rounds (inc rnds)}]))))

(defmethod event-tx-fn :initiative/change-roll
  [_ _ id roll]
  (let [parsed (.parseFloat js/window roll)]
    (cond
      (or (nil? roll) (= roll ""))
      [[:db/add id]
       [:db/retract id :initiative/roll]]

      (.isNaN js/Number parsed)
      []

      :else
      [{:db/id id :initiative/roll parsed}])))

(defmethod event-tx-fn :initiative/roll-all
  [data]
  (let [local  (ds/entity data [:db/ident :local])
        scene  (:camera/scene (:local/camera local))]
    (for [token (:scene/initiative scene)
          :let  [{:keys [db/id token/flags initiative/roll]} token]
          :when (and (nil? roll) (not (contains? flags :player)))]
      {:db/id id :initiative/roll (inc (rand-int 20))})))

(defmethod event-tx-fn :initiative/reset
  [data]
  (let [local (ds/entity data [:db/ident :local])
        scene (:camera/scene (:local/camera local))]
    (->> (for [token (:scene/initiative scene)
               :let [{id :db/id} token]]
           [[:db/retract id :initiative/roll]])
         (into [[:db/retract (:db/id scene) :initiative/turn]
                [:db/retract (:db/id scene) :initiative/turns]
                [:db/retract (:db/id scene) :initiative/rounds]] cat))))

(defmethod event-tx-fn :initiative/change-health
  [data _ id f value]
  (let [parsed (.parseFloat js/window value)]
    (if (.isNaN js/Number parsed) []
        (let [{:keys [initiative/health]} (ds/entity data id)]
          [{:db/id id :initiative/health (f health parsed)}]))))

(defmethod event-tx-fn :initiative/leave
  [data]
  (let [local (ds/entity data [:db/ident :local])
        scene (:camera/scene (:local/camera local))]
    (apply concat
           [[:db/retract (:db/id scene) :scene/initiative]
            [:db/retract (:db/id scene) :initiative/turn]
            [:db/retract (:db/id scene) :initiative/turns]
            [:db/retract (:db/id scene) :initiative/rounds]]
           (for [{id :db/id} (:scene/initiative scene)]
             [[:db/retract id :initiative/roll]
              [:db/retract id :initiative/health]
              [:db/retract id :initiative/suffix]]))))

(defmethod event-tx-fn :tokens/create
  [_ _ image-data scope]
  (let [keys [:name :size :checksum :width :height]
        data (-> image-data (select-keys keys) (assoc :scope scope))]
    [{:db/ident :root :root/token-images (with-ns data "image")}]))

(defmethod
  ^{:doc "Change the scope of the token image by the given checksum to the
          given scope, typically `:public` or `:private`."}
  event-tx-fn :tokens/change-scope
  [_ _ checksum scope]
  [[:db/add -1 :image/checksum checksum]
   [:db/add -1 :image/scope scope]])

(defmethod event-tx-fn :tokens/remove
  [_ _ checksum]
  [[:db/retractEntity [:image/checksum checksum]]])

(defmethod event-tx-fn :tokens/remove-all
  []
  [[:db/retract [:db/ident :root] :root/token-images]])

;; --- Masks ---
(defmethod
  ^{:doc "Sets the current scene to be entirely masked by default. This is
          useful when the scene image is composed of many rooms and mostly
          dead space between them, such as a dungeon, and it is more efficient
          to 'carve out' the scene instead of filling it in."}
  event-tx-fn :scene/mask
  []
  [[:db.fn/call assoc-scene :scene/masked true]])

(defmethod
  ^{:doc "Sets the current scene to not be entirely masked by default. This
          is the default behavior."}
  event-tx-fn :scene/reveal
  []
  [[:db.fn/call assoc-scene :scene/masked false]])

(defmethod
  ^{:doc "Creates a new mask object for the current scene, accepting its
          current state (hide or reveal) and its polygon points as a flat
          vector of x, y pairs."}
  event-tx-fn :mask/create
  [_ _ state vecs]
  [[:db.fn/call assoc-scene :scene/masks
    {:mask/enabled? state :mask/vecs vecs}]])

(defmethod
  ^{:doc "Toggles the state of the given mask to be either hiding or revealing
          its contents."}
  event-tx-fn :mask/toggle
  [_ _ id state]
  [{:db/id id :mask/enabled? state}])

(defmethod
  ^{:doc "Removes the given mask object."}
  event-tx-fn :mask/remove
  [_ _ id]
  [[:db/retractEntity id]])

;; -- Session --
(defmethod
  ^{:doc "Attempts to start a new online session through the server. This
          transaction only updates the connection status of the local user
          and expands the session panel form."}
  event-tx-fn :session/request
  []
  [{:db/ident :root :root/session
    {:db/ident :session :session/host
     {:db/ident :local :session/state :connecting :panel/selected :session}}}])

(defmethod
  ^{:doc "Attempts to join an existing online session through the server. This
          transaction only updates the connection status of the local user."}
  event-tx-fn :session/join
  []
  [{:db/ident :local :session/state :connecting}])

(defmethod
  ^{:doc "Destroys the existing online session, pruning it and all player
          user state."}
  event-tx-fn :session/close
  []
  [{:db/ident :local :session/state :disconnected}
   [:db/retract [:db/ident :session] :session/host]
   [:db/retract [:db/ident :session] :session/conns]])

(defmethod
  ^{:doc "Updates the connection status for the local user to `disconnected`.
          This is typically done in response to an unexpected closure of the
          server connection."}
  event-tx-fn :session/disconnected
  []
  [{:db/ident :local :session/state :disconnected}
   [:db/retract [:db/ident :session] :session/host]
   [:db/retract [:db/ident :session] :session/conns]])

(defmethod
  ^{:doc "Toggles whether or not live cursors are displayed for everyone
          in the online session."}
  event-tx-fn :session/toggle-share-cursors
  [_ _ enabled]
  [{:db/ident :session :session/share-cursors enabled}])

(defmethod
  ^{:doc "Toggles whether or not the cursor of the local user is displayed
          to other players in the session."}
  event-tx-fn :session/toggle-share-my-cursor
  [_ _ enabled]
  [{:db/ident :local :local/share-cursor enabled}])

(defmethod
  ^{:doc "Updates the current scene, camera position, and zoom level of all
          players in the online session to match the host. This is useful to
          bring a new scene or encounter to attention."}
  event-tx-fn :session/focus
  [data]
  (let [select-w [:camera/scene [:camera/point :default [0 0]] [:camera/scale :default 1]]
        select-l [:db/id [:bounds/self :default [0 0 0 0]] {:local/cameras [:camera/scene] :local/camera select-w}]
        select-s [{:session/host select-l} {:session/conns select-l}]
        result   (ds/pull data select-s [:db/ident :session])
        {{[_ _ hw hh] :bounds/self
          {[hx hy] :camera/point} :local/camera
          host :local/camera} :session/host
         conns :session/conns} result
        scale (:camera/scale host)
        mx (+ (/ hw scale 2) hx)
        my (+ (/ hh scale 2) hy)]
    (->> (for [[next conn] (sequence (indexed) conns)
               :let [exst (->> (:local/cameras conn)
                               (filter (fn [conn]
                                         (= (:db/id (:camera/scene conn))
                                            (:db/id (:camera/scene host)))))
                               (first)
                               (:db/id))
                     prev (or exst next)
                     [_ _ cw ch] (:bounds/self conn)
                     cx (- mx (/ cw scale 2))
                     cy (- my (/ ch scale 2))]]
           [{:db/id (:db/id conn) :local/camera prev :local/cameras prev}
            {:db/id prev
             :camera/point [cx cy]
             :camera/scale scale
             :camera/scene (:db/id (:camera/scene host))}])
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
  event-tx-fn :clipboard/copy
  ([_ event]
   [[:db.fn/call event-tx-fn event false]])
  ([data _ cut?]
   (let [attrs  [:db/id :token/label :token/flags :token/light :token/size :aura/radius :token/image :token/point]
         select [{:local/camera [{:camera/selected (into attrs [:scene/_tokens {:token/image [:image/checksum]}])}]}]
         result (ds/pull data select [:db/ident :local])
         tokens (filter (comp-fn contains? identity :scene/_tokens) (:camera/selected (:local/camera result)))
         copies (into [] (map (comp-fn select-keys identity attrs)) tokens)]
     (cond-> []
       (seq tokens)
       (into [{:db/ident :local :local/clipboard copies}])
       (and (seq tokens) cut?)
       (into (for [{id :db/id} tokens]
               [:db/retractEntity id]))))))

(def ^:private clipboard-paste-select
  [{:root/local
    [[:local/clipboard :default []]
     [:bounds/self :default [0 0 0 0]]
     {:local/camera
      [:db/id
       [:camera/scale :default 1]
       [:camera/point :default [0 0]]
       {:camera/scene
        [:db/id
         [:scene/grid-origin :default [0 0]]
         [:scene/grid-align :default false]]}]}]}
   {:root/token-images [:image/checksum]}])

(defmethod
  ^{:doc "Creates tokens on the current scene from the data stored in the local
          user's clipboard. Attempts to preserve the relative position of
          the tokens when they were copied but in the center of the user's
          viewport. Clipboard data is not pruned after pasting."}
  event-tx-fn :clipboard/paste
  [data]
  (let [result (ds/pull data clipboard-paste-select [:db/ident :root])
        {{clipboard :local/clipboard
          [_ _ sw sh] :bounds/self
          {camera :db/id
           scale :camera/scale
           [cx cy] :camera/point
           {scene :db/id
            align? :scene/grid-align
            [gx gy] :scene/grid-origin} :camera/scene} :local/camera} :root/local
         images :root/token-images} result
        hashes (into #{} (map :image/checksum) images)
        [ax ay bx by] (apply bounding-box (map :token/point clipboard))
        sx (+ (/ sw scale 2) cx)
        sy (+ (/ sh scale 2) cy)
        ox (/ (- ax bx) 2)
        oy (/ (- ay by) 2)]
    (for [[idx token] (sequence (indexed) clipboard)
          :let [[tx ty] (:token/point token)
                cs (:image/checksum (:token/image token))
                sz (:token/size token)
                rd (* (/ (or sz 5) 5) (/ grid-size 2))
                ax (+ sx tx ox (- ax))
                ay (+ sy ty oy (- ay))
                dt (merge token
                          {:db/id idx
                           :token/image (if (some? cs) [:image/checksum (hashes cs)] {})
                           :token/point
                           (if align?
                             [(round-grid ax rd (mod gx grid-size))
                              (round-grid ay rd (mod gy grid-size))]
                             [ax ay])})]]
      {:db/id camera
       :camera/scene {:db/id scene :scene/tokens dt}
       :camera/selected idx})))

;; -- Shortcuts --
(defmethod
  ^{:doc "Handles the 'Escape' keyboard shortcut, clearing any token
          selections and changing the mode to `select`."}
  event-tx-fn :shortcut/escape
  [data]
  (let [local (ds/entity data [:db/ident :local])
        id   (:db/id (:local/camera local))]
    [{:db/id id :camera/draw-mode :select}
     [:db/retract id :camera/selected]]))

;; -- Dragging --
(defmethod
  ^{:doc "User has started dragging one or more scene objects."}
  event-tx-fn :drag/start
  [_ _ ids]
  [{:db/ident :local :local/dragging ids}])

(defmethod
  ^{:doc "User has ended all dragging."}
  event-tx-fn :drag/end
  []
  [[:db/retract [:db/ident :local] :local/dragging]])
