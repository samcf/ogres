(ns ogres.app.events
  (:require [datascript.core :as ds]
            [clojure.set :refer [union difference]]
            [clojure.string :refer [trim]]
            [ogres.app.const :refer [grid-size half-size]]
            [ogres.app.geom :as geom]
            [ogres.app.matrix :as matrix]
            [ogres.app.segment :as seg]
            [ogres.app.vec :as vec :refer [Vec2]]))

(def ^:private suffix-max-xf
  (map (fn [[label tokens]] [label (apply max (map :initiative/suffix tokens))])))

(def ^:private zoom-scales
  [0.15 0.30 0.50 0.75 0.90 1 1.25 1.50 2 3 4])

(defn ^:private linear [dx dy rx ry]
  (fn [n] (+ (* (/ (- n dx) (- dy dx)) (- ry rx)) rx)))

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
  (let [{label :token/label {hash :image/hash} :token/image} token]
    [label hash]))

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
  (not (and (contains? #{:mask :mask-toggle :mask-remove :grid :note} mode)
            (not= type :host))))

(defn ^:private initiative-order [a b]
  (let [f (juxt :initiative/roll :db/id)]
    (compare (f b) (f a))))

(defn ^:private random-rolls
  "Returns a lazy infinite sequence of random integers in the domain
   of [start, end]. Each group of integers in the domain are guaranteed
   to be unique among each other."
  [start end]
  (sequence (mapcat shuffle) (repeat (range start (inc end)))))

(defn ^:private roll-token? [token]
  (let [{:keys [initiative/roll token/flags]} token]
    (and (not (contains? flags :player))
         (not (number? roll)))))

(defmulti event-tx-fn (fn [_ event] event))

(defmethod event-tx-fn :default [] [])

;; -- Local --
(defmethod
  ^{:doc "Change the selected panel to the keyword given by `panel`."}
  event-tx-fn :user/select-panel
  [_ _ panel]
  [{:db/ident :user :panel/selected panel :panel/expanded true}])

(defmethod
  ^{:doc "Toggle the expanded state of the panel."}
  event-tx-fn :user/toggle-panel
  [data]
  (let [user (ds/entity data [:db/ident :user])]
    [{:db/ident :user :panel/expanded (not (get user :panel/expanded true))}]))

(defmethod
  ^{:doc "Changes the character label for the given user."}
  event-tx-fn :user/change-label
  ([_ _ value]
   [{:db/ident :user :user/label value}])
  ([_ _ uuid value]
   [{:user/uuid uuid :user/label value}]))

(defmethod
  ^{:doc "Changes the character description for the given user."}
  event-tx-fn :user/change-description
  ([_ _ value]
   [{:db/ident :user :user/description value}])
  ([_ _ uuid value]
   [{:user/uuid uuid :user/description value}]))

(defmethod
  ^{:doc "Changes the character image for the given user."}
  event-tx-fn :user/change-image
  ([_ _ hash]
   [{:db/ident :user :user/image [:image/hash hash]}])
  ([_ _ uuid hash]
   [{:user/uuid uuid :user/image [:image/hash hash]}]))

(defmethod
  ^{:doc "Changes the character label and description for the given user."}
  event-tx-fn :user/change-details
  ([_ _ label description]
   [{:db/ident :user :user/label label :user/description description}])
  ([_ _ uuid label description]
   [{:user/uuid uuid :user/label label :user/description description}]))

;; -- Camera --
(defn ^:private assoc-camera
  [data & kvs]
  (let [user (ds/entity data [:db/ident :user])]
    [(apply assoc {:db/id (:db/id (:user/camera user))} kvs)]))

(defmethod
  ^{:doc "Changes the public label for the current camera."}
  event-tx-fn :camera/change-label
  [_ _ label]
  [[:db.fn/call assoc-camera :camera/label label]])

(defmethod
  ^{:doc "Removes the public label for the current camera."}
  event-tx-fn :camera/remove-label
  [data]
  (let [user (ds/entity data [:db/ident :user])]
    [[:db/retract (:db/id (:user/camera user)) :camera/label]]))

(defmethod
  ^{:doc "Translate the current camera by the offset given by dx and dy."}
  event-tx-fn :camera/translate
  [data _ delta]
  (let [{{id :db/id point :camera/point scale :camera/scale} :user/camera}
        (ds/entity data [:db/ident :user])]
    [{:db/id id :camera/point (vec/add (or point vec/zero) (vec/div delta (or scale 1)))}]))

(defmethod
  ^{:doc "Changes the camera draw mode to the given value. The draw mode is
          used to decide what behavior clicking and dragging on the scene
          will have, such as drawing a shape or determining the distance
          between two points."}
  event-tx-fn :camera/change-mode
  [data _ mode]
  (let [user (ds/entity data [:db/ident :user])]
    (if (mode-allowed? mode (:user/type user))
      [{:db/id (:db/id (:user/camera user)) :camera/draw-mode mode}]
      [])))

(defmethod
  ^{:doc "Changes the zoom value for the current camera by the given value
          `next` and, optionally, a cursor point given by `x` and `y`.
          This method preserves the point of the cursor on the scene,
          adjusting the camera point to ensure that the user feels as if
          they are zooming in or out from their cursor."}
  event-tx-fn :camera/zoom-change
  ([data event & args]
   (let [user (ds/entity data [:db/ident :user])]
     (case (count args)
       0 [[:db.fn/call event-tx-fn event 1]]
       1 (let [[scale] args
               bounds (or (:user/bounds user) seg/zero)]
           [[:db.fn/call event-tx-fn event scale (seg/midpoint bounds)]])
       (let [[next-scale point] args
             {{id :db/id scale :camera/scale camera :camera/point} :user/camera} user]
         [{:db/id id
           :camera/scale next-scale
           :camera/point
           (-> (vec/mul point (/ next-scale (or scale 1)))
               (vec/sub point)
               (vec/div next-scale)
               (vec/add camera))}])))))

(defmethod
  ^{:doc "Changes the zoom value for the current camera by offsetting it from
          the given value `delta`. This is useful for zooming with a device
          that uses fine grained updates such as a mousewheel or a trackpad."}
  event-tx-fn :camera/zoom-delta
  [data _ mx my delta trackpad?]
  (let [user (ds/entity data [:db/ident :user])
        bound (or (:user/bounds user) seg/zero)
        scale (linear -400 400 -0.50 0.50)
        delta (if trackpad? (scale (* -1 8 delta)) (scale (* -1 2 delta)))
        point (vec/sub (Vec2. mx my) (.-a bound))
        scale (-> (:camera/scale (:user/camera user)) (or 1)
                  (js/Math.log) (+ delta) (js/Math.exp)
                  (to-precision 2) (constrain 0.15 4))]
    [[:db.fn/call event-tx-fn :camera/zoom-change scale point]]))

(defmethod
  ^{:doc "Increases the zoom value for the current camera to the next nearest
          zoom level. These fixed zoom levels are determined by an internal
          constant."}
  event-tx-fn :camera/zoom-in
  [data]
  (let [user   (ds/entity data [:db/ident :user])
        camera (:user/camera user)
        prev   (or (:camera/scale camera) 1)
        next   (reduce (fn [n s] (if (> s prev) (reduced s) n)) prev zoom-scales)]
    [[:db.fn/call event-tx-fn :camera/zoom-change next]]))

(defmethod
  ^{:doc "Decreases the zoom value for the current camera to the nearest
          previous zoom level. These fixed zoom levels are determined by an
          internal constant."}
  event-tx-fn :camera/zoom-out
  [data]
  (let [user   (ds/entity data [:db/ident :user])
        camera (:user/camera user)
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
  [[:db/add -1 :db/ident :root]
   [:db/add -1 :root/scenes -2]
   [:db/add -2 :db/empty true]
   [:db/add -1 :root/user -3]
   [:db/add -3 :db/ident :user]
   [:db/add -3 :user/camera -4]
   [:db/add -3 :user/cameras -4]
   [:db/add -4 :camera/scene -2]
   [:db/add -4 :camera/point vec/zero]])

(defmethod
  ^{:doc "Switches to the given scene by the given camera identifier."}
  event-tx-fn :scenes/change
  [_ _ id]
  [{:db/ident :user :user/camera id}])

(defmethod
  ^{:doc "Removes the scene and corresponding camera for the local user. Also
          removes all scene cameras for any connected users and switches them
          to whichever scene the host is now on."}
  event-tx-fn :scenes/remove
  [data _ camera-id]
  (let [root (ds/entity data [:db/ident :root])
        user (ds/entity data [:db/ident :user])
        prev-cam (ds/entity data camera-id)
        prev-scn (:db/id (:camera/scene prev-cam))]
    (conj
     (if (= (:db/id (:user/camera user)) (:db/id prev-cam))
       (if-let [next-scn (:db/id (first (remove (comp #{prev-scn} :db/id) (:root/scenes root))))]
         (if-let [next-cam (:db/id (first (filter (comp #{next-scn} :db/id :camera/scene) (:user/cameras user))))]
           [[:db/add (:db/id user) :user/camera next-cam]]
           [[:db/add (:db/id user) :user/camera -1]
            [:db/add (:db/id user) :user/cameras -1]
            [:db/add -1 :camera/scene next-scn]
            [:db/add -1 :camera/point vec/zero]])
         [[:db/add (:db/id root) :root/scenes -2]
          [:db/add (:db/id user) :user/camera -1]
          [:db/add (:db/id user) :user/cameras -1]
          [:db/add -1 :camera/scene -2]
          [:db/add -1 :camera/point vec/zero]
          [:db/add -2 :db/empty true]]) [])
     [:db.fn/call event-tx-fn :scenes/sync-with-user prev-scn]
     [:db/retractEntity prev-scn]
     [:db/retractEntity camera-id])))

(defmethod
  ^{:doc "Find all players that are currently viewing the given scene and
          move them to the scene being viewd by the current user."}
  event-tx-fn
  :scenes/sync-with-user
  [data _ prev-scn]
  (let [next-scn (-> (ds/entity data [:db/ident :user]) :user/camera :camera/scene :db/id)]
    (->> (for [conn (:session/conns (ds/entity data [:db/ident :session]))
               :let [curr-scn (-> conn :user/camera :camera/scene :db/id)]
               :when (= curr-scn prev-scn)
               :let [curr-cam (first (filter (comp #{curr-scn} :db/id :camera/scene) (:user/cameras conn)))]]
           (if-let [next-cam (first (filter (comp #{next-scn} :db/id :camera/scene) (:user/cameras conn)))]
             [[:db/retractEntity (:db/id curr-cam)]
              [:db/add (:db/id conn) :user/camera (:db/id next-cam)]]
             [[:db/retractEntity (:db/id curr-cam)]
              [:db/add (:db/id conn) :user/cameras -3]
              [:db/add (:db/id conn) :user/camera -3]
              [:db/add -3 :camera/scene next-scn]
              [:db/add -3 :camera/point vec/zero]]))
         (apply concat))))

;; -- Scene Images --
(defmethod event-tx-fn :scene-images/create-many
  [_ _ images]
  (into [{:db/ident :root
          :root/scene-images
          (for [[{:keys [hash name size width height]} _] images]
            {:image/hash hash
             :image/name name
             :image/size size
             :image/width width
             :image/height height})}] cat
        (for [[image thumbnail] images]
          (if (= (:hash image) (:hash thumbnail))
            [{:image/hash (:hash image) :image/thumbnail [:image/hash (:hash image)]}]
            [{:image/hash (:hash thumbnail)
              :image/name (:name thumbnail)
              :image/size (:size thumbnail)
              :image/width (:width thumbnail)
              :image/height (:height thumbnail)}
             {:image/hash (:hash image) :image/thumbnail [:image/hash (:hash thumbnail)]}]))))

(defmethod
  ^{:doc "Removes the scene image by the given identifying hash."}
  event-tx-fn :scene-images/remove
  [_ _ image thumb]
  (if (= image thumb)
    [[:db/retractEntity [:image/hash image]]]
    [[:db/retractEntity [:image/hash image]]
     [:db/retractEntity [:image/hash thumb]]]))

;; -- Scene --
(defn ^:private assoc-scene
  [data & kvs]
  (let [user (ds/entity data [:db/ident :user])
        scene (:db/id (:camera/scene (:user/camera user)))]
    [(apply assoc {:db/id scene} kvs)]))

(defmethod
  ^{:doc "Updates the image being used for the current scene by the given
          identifying hash."}
  event-tx-fn :scene/change-image
  [_ _ hash]
  [[:db.fn/call assoc-scene :scene/image {:image/hash hash}]])

(defmethod
  ^{:doc "Updates the grid size for the current scene."}
  event-tx-fn :scene/change-grid-size
  [_ _ size]
  [[:db.fn/call assoc-scene :scene/grid-size size]])

(defmethod
  ^{:doc "Applies both a grid origin and tile size to the current scene."}
  event-tx-fn :scene/apply-grid-options
  [data _ origin size]
  (let [{{camera-id :db/id point :camera/point
          {scene-id :db/id prev-origin :scene/grid-origin}
          :camera/scene} :user/camera}
        (ds/entity data [:db/ident :user])]
    [{:db/id camera-id
      :camera/draw-mode :select
      :camera/point (vec/sub (vec/add point (or prev-origin vec/zero)) origin)
      :camera/scene
      {:db/id scene-id
       :scene/grid-size size
       :scene/grid-origin origin}}]))

(defmethod
  ^{:doc "Resets the grid origin to (0, 0)."}
  event-tx-fn :scene/reset-grid-origin
  [data]
  (let [user (ds/entity data [:db/ident :user])
        scene (:db/id (:camera/scene (:user/camera user)))]
    [[:db.fn/call assoc-camera :camera/draw-mode :select]
     [:db/retract scene :scene/grid-origin]]))

(defmethod
  ^{:doc "Retracts the grid size for the current scene, allowing queries to
          revert to their defaults."}
  event-tx-fn :scene/retract-grid-size
  [data]
  (let [user (ds/entity data [:db/ident :user])
        scene (:db/id (:camera/scene (:user/camera user)))]
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
  ^{:doc "Updates whether or not object outlines are drawn on the current scene."}
  event-tx-fn :scene/toggle-object-outlines
  [_ _ enabled]
  [[:db.fn/call assoc-scene :scene/show-object-outlines enabled]])

(defmethod
  ^{:doc "Updates the lighting option used for the current scene."}
  event-tx-fn :scene/change-lighting
  [_ _ value]
  [[:db.fn/call assoc-scene :scene/lighting value]])

;; --- Objects ---
(defmethod event-tx-fn :objects/translate
  ^{:doc "Translates the object given by id by the given delta."}
  [_ _ id delta]
  [[:db.fn/call event-tx-fn :objects/translate-many #{id} delta]])

(def ^:private translate-many-select
  [:db/id
   :object/type
   :object/point
   :shape/points
   :token/size])

(defmethod event-tx-fn :objects/translate-many
  ^{:doc "Translates the objects given by idxs by the given delta,
          possibly aligning them to the grid if the appropriate scene
          option is enabled."}
  [data _ idxs delta]
  (let [result (ds/entity data [:db/ident :user])
        align? (-> result :user/camera :camera/scene :scene/grid-align)]
    (into [[:db/retract [:db/ident :user] :user/dragging]]
          (for [entity (ds/pull-many data translate-many-select idxs)
                :let [{id :db/id point :object/point} entity]]
            (cond
              (and align? (= (:object/type entity) :token/token))
              (let [bounds (vec/rnd (vec/add (geom/object-bounding-rect entity) delta) grid-size)]
                {:db/id id :object/point (seg/midpoint bounds)})
              (and align? (not= (:object/type entity) :note/note))
              (let [round (geom/object-alignment entity)]
                {:db/id id :object/point (vec/rnd (vec/add point delta) round)})
              :else
              {:db/id id :object/point (vec/add point delta)})))))

(defmethod event-tx-fn :objects/translate-selected
  ^{:doc "Translate the currently selected objects by the given delta."}
  [data _ delta]
  (let [select [{:user/camera [{:camera/selected [:db/id :object/point]}]}]
        result (ds/pull data select [:db/ident :user])
        {{selected :camera/selected} :user/camera} result]
    [[:db.fn/call event-tx-fn :objects/translate-many (into #{} (map :db/id) selected) delta]]))

(defmethod event-tx-fn :objects/select
  ^{:doc "Joins or removes the object given by id to the current selection,
          alternating behavior based on the boolean modify."}
  [data _ id modify]
  (let [object (ds/entity data id)
        entity (ds/entity data [:db/ident :user])
        {{camera :db/id selected :camera/selected} :user/camera} entity]
    [[:db/retract [:db/ident :user] :user/dragging]
     (if (not modify)
       [:db/retract camera :camera/selected])
     (if (and modify (contains? selected object))
       [:db/retract camera :camera/selected id]
       {:db/id camera :camera/selected {:db/id id}})]))

(defmethod
  ^{:doc "Hide or reveal the given object."}
  event-tx-fn :objects/toggle-hidden
  [data _ id]
  (let [entity (ds/entity data id)]
    [[:db/add id :object/hidden (not (:object/hidden entity))]]))

(defmethod
  ^{:doc "Hides or reveals the currently selected objects."}
  event-tx-fn :objects/toggle-hidden-selected
  [data _]
  (let [user (ds/entity data [:db/ident :user])
        selected (:camera/selected (:user/camera user))]
    (for [{id :db/id} selected]
      [:db/add id :object/hidden (not (every? :object/hidden selected))])))

(defmethod
  ^{:doc "Locks or unlocks the currently selected objects."}
  event-tx-fn :objects/toggle-locked-selected
  [data _]
  (let [user (ds/entity data [:db/ident :user])
        selected (:camera/selected (:user/camera user))]
    (for [{id :db/id} selected]
      [:db/add id :object/locked (not (every? :object/locked selected))])))

(defmethod
  ^{:doc "Resets all transformations for the currently selected objects."}
  event-tx-fn :objects/reset-transform-selected
  [data _]
  (let [user (ds/entity data [:db/ident :user])]
    (into
     [] cat
     (for [{id :db/id} (:camera/selected (:user/camera user))]
       [[:db/retract id :object/scale]
        [:db/retract id :object/rotation]]))))

(defmethod
  ^{:doc "Change the scaling of the given object."}
  event-tx-fn :object/change-scale
  [_ _ id scale]
  [[:db/add id :object/scale scale]])

(defmethod
  ^{:doc "Change the rotation of the given object."}
  event-tx-fn :object/change-rotation
  [_ _ id rotation]
  [[:db/add id :object/rotation rotation]])

(defmethod
  ^{:doc "Removes the objects given by idxs."}
  event-tx-fn :objects/remove
  [_ _ idxs]
  (for [id idxs]
    [:db/retractEntity id]))

(defmethod
  ^{:doc "Removes all currently currently selected objects."}
  event-tx-fn :objects/remove-selected
  [data _]
  (let [user (ds/entity data [:db/ident :user])]
    (for [{id :db/id} (:camera/selected (:user/camera user))]
      [:db/retractEntity id])))

(defmethod
  ^{:doc "Updates the attribute for the objects given by idxs to the
          given value."}
  event-tx-fn :objects/update
  [_ _ idxs attr value]
  (for [id idxs]
    (assoc {:db/id id} attr value)))

;; --- Tokens ---
(defmethod
  ^{:doc "Creates a new token on the current scene at the screen coordinates
          given by `sx` and `sy`. These coordinates are converted to the
          scene coordinate space."}
  event-tx-fn :token/create
  [data _ point hash]
  (let [{{camera-id :db/id
          shift :camera/point
          scale :camera/scale
          {scene-id :db/id
           align? :scene/grid-align} :camera/scene} :user/camera}
        (ds/entity data [:db/ident :user])
        next (vec/add (vec/div point (or scale 1)) shift)]
    [(cond-> {:db/id -1 :object/type :token/token}
       (some? hash) (assoc :token/image {:image/hash hash})
       (not align?) (assoc :object/point next)
       align?       (assoc :object/point
                           (-> (vec/shift next (- half-size))
                               (vec/rnd grid-size)
                               (vec/shift half-size))))
     {:db/id camera-id
      :camera/selected -1
      :camera/draw-mode :select
      :camera/scene
      {:db/id scene-id
       :scene/tokens -1}}]))

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
    {:db/id id :token/aura-radius radius}))

(defmethod event-tx-fn :token/change-dead
  [_ _ idxs add?]
  [[:db.fn/call event-tx-fn :token/change-flag idxs :dead add?]
   (if add?
     [:db.fn/call event-tx-fn :initiative/toggle idxs false])])

(defmethod event-tx-fn :shape/create
  [_ _ type [src & points]]
  [{:db/id -1
    :object/type (keyword :shape type)
    :object/point src
    :shape/points (into [] (map (fn [vrt] (vec/sub vrt src))) points)}
   [:db.fn/call assoc-camera :camera/draw-mode :select :camera/selected -1]
   [:db.fn/call assoc-scene :scene/shapes -1]])

(defmethod event-tx-fn :user/change-bounds
  [_ _ bounds]
  [[:db/add -1 :db/ident :user]
   [:db/add -1 :user/bounds bounds]])

(defmethod event-tx-fn :selection/from-rect
  [data _ rect]
  (let [result (ds/entity data [:db/ident :root])
        {{{{tokens :scene/tokens
            shapes :scene/shapes
            notes  :scene/notes
            props  :scene/props} :camera/scene
           camera :db/id} :user/camera
          type :user/type} :root/user
         {conns :session/conns} :root/session} result
        bounds (geom/bounding-rect (seq rect))
        occupied (into #{} (comp (mapcat :user/dragging) (map :db/id)) conns)]
    [{:db/id camera
      :camera/draw-mode :select
      :camera/selected
      (for [entity (concat shapes tokens notes props)
            :let   [{id :db/id} entity]
            :let   [object (geom/object-bounding-rect entity)]
            :when  (and (not (occupied id))
                        (not (and (= type :conn) (:object/hidden entity)))
                        (not (and (= type :conn) (= (:object/type entity) :note/note)))
                        (not (and (= type :conn) (= (:object/type entity) :prop/prop)))
                        (geom/rect-intersects-rect object bounds))]
        {:db/id id})}]))

(defmethod event-tx-fn :selection/clear
  [data]
  (let [user (ds/entity data [:db/ident :user])]
    [[:db/retract (:db/id (:user/camera user)) :camera/selected]]))

(defmethod event-tx-fn :selection/remove
  [data]
  (let [user (ds/entity data [:db/ident :user])]
    (for [entity (:camera/selected (:user/camera user))]
      [:db/retractEntity (:db/id entity)])))

(defmethod event-tx-fn :initiative/toggle
  [data _ idxs adding?]
  (let [user   (ds/entity data [:db/ident :user])
        scene  (:db/id (:camera/scene (:user/camera user)))
        select [:db/id {:token/image [:image/hash]} [:token/flags :default #{}] :initiative/suffix :token/label]
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
               [:db/retract scene :scene/initiative id]
               [:db/retract scene :initiative/played id]])))))

(defmethod event-tx-fn :initiative/next
  [data]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))
        {rounds :initiative/rounds
         tokens :scene/initiative
         played :initiative/played} scene]
    (if (some? rounds)
      (let [[next] (sort initiative-order (difference tokens played))]
        (if (some? next)
          [{:db/id (:db/id scene)
            :initiative/turn (:db/id next)
            :initiative/played (:db/id next)}]
          [[:db/retract (:db/id scene) :initiative/played]
           [:db/retract (:db/id scene) :initiative/turn]
           {:db/id (:db/id scene) :initiative/rounds (inc rounds)}]))
      [{:db/id (:db/id scene) :initiative/rounds 1}])))

(defmethod event-tx-fn :initiative/mark
  [data _ id]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))]
    [{:db/id (:db/id scene)
      :initiative/turn {:db/id id}
      :initiative/played {:db/id id}
      :initiative/rounds (max (:initiative/rounds scene) 1)}]))

(defmethod event-tx-fn :initiative/unmark
  [data _ id]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))]
    [[:db/retract (:db/id scene) :initiative/played id]
     (if (= (:db/id (:initiative/turn scene)) id)
       [:db/retract (:db/id scene) :initiative/turn])]))

(defmethod event-tx-fn :initiative/change-roll
  [_ _ id roll]
  (let [parsed (.parseFloat js/window roll)]
    (cond
      (or (nil? roll) (= roll ""))
      [[:db/retract id :initiative/roll]]

      (.isNaN js/Number parsed)
      []

      :else
      [{:db/id id :initiative/roll parsed}])))

(defmethod event-tx-fn :initiative/roll-all
  [data]
  (let [user (ds/entity data [:db/ident :user])
        {{{tokens :scene/initiative} :camera/scene} :user/camera} user
        idxs (sequence (comp (filter roll-token?) (map :db/id)) tokens)]
    (for [[id roll] (zipmap idxs (random-rolls 1 20))]
      {:db/id id :initiative/roll roll})))

(defmethod event-tx-fn :initiative/change-health
  [data _ id f value]
  (let [parsed (.parseFloat js/window value)]
    (if (.isNaN js/Number parsed) []
        (let [{:keys [initiative/health]} (ds/entity data id)]
          [{:db/id id :initiative/health (f health parsed)}]))))

(defmethod event-tx-fn :initiative/leave
  [data]
  (let [user (ds/entity data [:db/ident :user])
        scene (:camera/scene (:user/camera user))]
    (apply concat
           [[:db/retract (:db/id scene) :scene/initiative]
            [:db/retract (:db/id scene) :initiative/turn]
            [:db/retract (:db/id scene) :initiative/played]
            [:db/retract (:db/id scene) :initiative/rounds]]
           (for [{id :db/id} (:scene/initiative scene)]
             [[:db/retract id :initiative/roll]
              [:db/retract id :initiative/health]
              [:db/retract id :initiative/suffix]]))))

;; --- Token Images ---
(defmethod event-tx-fn :token-images/create-many
  [_ _ images scope]
  (into [{:db/ident :root
          :root/token-images
          (for [[{:keys [hash name size width height]} _] images]
            {:image/hash hash
             :image/name name
             :image/size size
             :image/scope scope
             :image/width width
             :image/height height})}] cat
        (for [[image thumbnail] images]
          (if (= (:hash image) (:hash thumbnail))
            [{:image/hash (:hash image) :image/thumbnail [:image/hash (:hash image)]}]
            [{:image/hash (:hash thumbnail)
              :image/name (:name thumbnail)
              :image/size (:size thumbnail)
              :image/width (:width thumbnail)
              :image/height (:height thumbnail)}
             {:image/hash (:hash image) :image/thumbnail [:image/hash (:hash thumbnail)]}]))))

(defmethod
  ^{:doc "Change the scope of the token image by the given hash to the
          given scope, typically `:public` or `:private`."}
  event-tx-fn :token-images/change-scope
  [_ _ hash scope]
  [[:db/add -1 :image/hash hash]
   [:db/add -1 :image/scope scope]])

(defmethod event-tx-fn :token-images/remove
  [_ _ image thumb]
  (if (= image thumb)
    [[:db/retractEntity [:image/hash image]]]
    [[:db/retractEntity [:image/hash image]]
     [:db/retractEntity [:image/hash thumb]]]))

(defmethod event-tx-fn :token-images/remove-all
  []
  [[:db/retract [:db/ident :root] :root/token-images]])

(defmethod event-tx-fn :token-images/change-thumbnail
  [_ _ hash thumb rect]
  [{:image/hash hash
    :image/thumbnail-rect rect
    :image/thumbnail
    {:image/hash (:hash thumb)
     :image/size (.-size (:data thumb))
     :image/width (:width thumb)
     :image/height (:height thumb)}}])

(defmethod
  ^{:doc ""}
  event-tx-fn
  :token-images/change-default-label
  [_ _ hash label]
  [[:db/add [:image/hash hash] :token-image/default-label label]])

(defmethod
  ^{:doc ""}
  event-tx-fn
  :token-images/change-url
  [_ _ hash url]
  [[:db/add [:image/hash hash] :token-image/url url]])

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
  [_ _ vecs]
  [[:db.fn/call assoc-scene :scene/masks {:mask/vecs vecs}]])

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
     {:db/ident :user :session/status :connecting :panel/selected :lobby}}}])

(defmethod
  ^{:doc "Attempts to join an existing online session through the server. This
          transaction only updates the connection status of the local user."}
  event-tx-fn :session/join
  []
  [{:db/ident :user :session/status :connecting}])

(defmethod
  ^{:doc "Destroys the existing online session, pruning it and all player
          user state."}
  event-tx-fn :session/close
  []
  [{:db/ident :user :session/status :disconnected}
   [:db/retract [:db/ident :session] :session/host]
   [:db/retract [:db/ident :session] :session/conns]])

(defmethod
  ^{:doc "Toggles whether or not live cursors are displayed for everyone
          in the online session."}
  event-tx-fn :session/toggle-share-cursors
  [_ _ enabled]
  [{:db/ident :session :session/share-cursors enabled}])

(defmethod event-tx-fn :session/change-status
  ^{:doc "Updates the user's session status in response to changes to
          the WebSocket's ready state."}
  [_ _ status]
  (let [statuses {0 :connecting 1 :connected 2 :disconnecting 3 :disconnected}]
    [{:db/ident :user :session/status (statuses status)}]))

(defmethod
  ^{:doc "Toggles whether or not the cursor of the local user is displayed
          to other players in the session."}
  event-tx-fn :session/toggle-share-my-cursor
  [_ _ enabled]
  [{:db/ident :user :user/share-cursor enabled}])

(defmethod
  ^{:doc "Updates the current scene, camera position, and zoom level of all
          players in the online session to match the host. This is useful to
          bring a new scene or encounter to attention."}
  event-tx-fn :session/focus
  [data]
  (let [select-w [:camera/scene [:camera/point :default vec/zero] [:camera/scale :default 1]]
        select-l [:db/id [:user/bounds :default seg/zero]
                  {:user/cameras [:camera/scene] :user/camera select-w}]
        select-s [{:session/host select-l} {:session/conns select-l}]
        result   (ds/pull data select-s [:db/ident :session])
        {{bounds :user/bounds
          {point :camera/point} :user/camera
          host :user/camera} :session/host
         conns :session/conns} result
        scale (:camera/scale host)
        center (vec/add point (vec/div (seg/midpoint bounds) scale))]
    (->> (for [[next conn] (sequence (indexed) conns)
               :let [prev (->> (:user/cameras conn)
                               (filter (fn [conn]
                                         (= (:db/id (:camera/scene conn))
                                            (:db/id (:camera/scene host))))) (first) (:db/id))
                     next  (or prev next)
                     point (vec/sub center (vec/div (seg/midpoint (:user/bounds conn)) scale))]]
           [{:db/id (:db/id conn) :user/camera next :user/cameras next}
            {:db/id next
             :camera/point point
             :camera/scale scale
             :camera/scene (:db/id (:camera/scene host))}])
         (into [] cat))))

;; -- Clipboard --
(def ^:private clipboard-copy-attrs
  [:object/point
   :object/type
   :object/hidden
   :object/locked
   :object/scale
   :object/rotation
   :note/icon
   :note/label
   :note/description
   :shape/points
   :shape/color
   :shape/pattern
   :token/label
   :token/flags
   :token/light
   :token/size
   :token/aura-radius
   :token/image
   :prop/image])

(def ^:private clipboard-copy-select
  [{:user/camera
    [{:camera/selected
      (into clipboard-copy-attrs
            [:db/id
             {:prop/image [:image/hash]}
             {:token/image [:image/hash]}])}]}])

(defmethod
  ^{:doc "Copy the currently selected objects to the clipboard. Optionally
          removes them from the current scene if cut? is passed as true.
          The clipboard contains a template for the object data and not
          references to the objects themselves since those references
          don't exist after they are pruned from the scene. Only some object
          data is copied; transient state is not preserved."}
  event-tx-fn :clipboard/copy
  ([_ event]
   [[:db.fn/call event-tx-fn event false]])
  ([data _ cut?]
   (let [result (ds/pull data clipboard-copy-select [:db/ident :user])
         copied (:camera/selected (:user/camera result))
         copies (into [] (map #(select-keys % clipboard-copy-attrs)) copied)]
     (cond-> []
       (seq copies) (conj {:db/ident :user :user/clipboard copies})
       cut?         (into (for [{id :db/id} copied]
                            [:db/retractEntity id]))))))

(def ^:private clipboard-paste-select
  [{:root/user
    [[:user/clipboard :default []]
     [:user/bounds :default seg/zero]
     {:user/camera
      [:db/id
       [:camera/scale :default 1]
       [:camera/point :default vec/zero]
       {:camera/scene
        [:db/id
         [:scene/grid-align :default false]]}]}]}
   {:root/token-images [:image/hash]}
   {:root/props-images [:image/hash]}])

(defmethod
  ^{:doc "Creates objects on the current scene from the data stored in the
          local user's clipboard. Attempts to preserve the relative position
          of the objects when they were copied but in the center of the user's
          viewport. Clipboard data is not pruned after pasting."}
  event-tx-fn :clipboard/paste
  [data]
  (let [result (ds/pull data clipboard-paste-select [:db/ident :root])
        {{clipboard :user/clipboard
          screen :user/bounds
          {camera :db/id
           scale :camera/scale
           point :camera/point
           {scene :db/id align? :scene/grid-align}
           :camera/scene} :user/camera} :root/user
         token-images :root/token-images
         props-images :root/props-images} result
        props-hashes (into #{} (map :image/hash) props-images)
        token-hashes (into #{} (map :image/hash) token-images)
        pastable-xf
        (filter
         (fn [data]
           (or (not= (:object/type data) :prop/prop)
               (props-hashes (:image/hash (:prop/image data))))))
        bound (transduce (mapcat geom/object-bounding-rect) geom/bounding-rect-rf clipboard)
        delta (vec/sub
               (vec/add point (vec/div (seg/midpoint screen) scale))
               (seg/midpoint bound))]
    (for [[idx copy] (sequence (comp pastable-xf (indexed)) clipboard)
          :let [{point :object/point
                 {hash-prop :image/hash} :prop/image
                 {hash-token :image/hash} :token/image} copy
                type (keyword (namespace (:object/type copy)))
                data (cond-> (assoc copy :db/id idx :object/point (vec/add point delta))
                       align?
                       (assoc :object/point (vec/rnd (vec/add point delta) grid-size))
                       (and (= type :prop) (props-hashes hash-prop))
                       (assoc :prop/image [:image/hash (props-hashes hash-prop)])
                       (and (= type :token) (token-hashes hash-token))
                       (assoc :token/image [:image/hash (token-hashes hash-token)])
                       (and (= type :token) align?)
                       (assoc :object/point
                              (let [bounds (geom/object-bounding-rect copy)
                                    aligns (vec/rnd (vec/add bounds delta) grid-size)]
                                (seg/midpoint aligns))))]]
      {:db/id camera
       :camera/selected idx
       :camera/scene
       (cond-> {:db/id scene}
         (= type :note)  (assoc :scene/notes data)
         (= type :prop)  (assoc :scene/props data)
         (= type :shape) (assoc :scene/shapes data)
         (= type :token) (assoc :scene/tokens data))})))

;; -- Shortcuts --
(defmethod
  ^{:doc "Handles the 'Escape' keyboard shortcut, clearing any token
          selections and changing the mode to `select`."}
  event-tx-fn :shortcut/escape
  [data]
  (let [user (ds/entity data [:db/ident :user])
        id   (:db/id (:user/camera user))]
    [{:db/id id :camera/draw-mode :select}
     [:db/retract id :camera/selected]]))

;; -- Dragging --
(defmethod
  ^{:doc "User has started dragging a single object."}
  event-tx-fn :drag/start
  [_ _ id]
  [{:db/ident :user :user/dragging id}])

(defmethod
  ^{:doc "User has started dragging all selected objects."}
  event-tx-fn :drag/start-selected
  [data _]
  (let [result (ds/entity data [:db/ident :user])
        {{selected :camera/selected} :user/camera} result]
    [{:db/ident :user :user/dragging (map :db/id selected)}]))

(defmethod
  ^{:doc "User has ended all dragging."}
  event-tx-fn :drag/end
  []
  [[:db/retract [:db/ident :user] :user/dragging]])

;; -- Notes --
(defmethod
  ^{:doc "Create a new note object at the given point."}
  event-tx-fn :note/create
  [data _ point]
  (let [user (ds/entity data [:db/ident :user])
        {bounds :user/bounds
         {camera :db/id
          {scene :db/id} :camera/scene
          shift :camera/point
          scale :camera/scale} :user/camera} user]
    [{:db/id -1
      :object/type :note/note
      :object/point
      (-> (vec/sub point (.-a bounds))
          (vec/div (or scale 1))
          (vec/add shift)
          (vec/shift -16)
          (vec/rnd))
      :object/hidden true
      :object/locked false}
     {:db/id scene :scene/notes -1}
     {:db/id camera :camera/selected -1 :camera/draw-mode :select}]))

(defmethod
  ^{:doc "Change the selected icon for the given note. icon-name is a
          string that corresponds to a unique icon name."}
  event-tx-fn :note/change-icon
  [_ _ id icon-name]
  [[:db/add id :note/icon icon-name]])

(defmethod
  ^{:doc "Change the label for the given note."}
  event-tx-fn :note/change-label
  [_ _ id value]
  [[:db/add id :note/label value]])

(defmethod
  ^{:doc "Change the description for the given note."}
  event-tx-fn :note/change-description
  [_ _ id value]
  [[:db/add id :note/description value]])

(defmethod
  ^{:doc "Change both the label and description for the given note
          and close the form for editing."}
  event-tx-fn :note/change-details
  [data _ id label desc]
  (let [user (ds/entity data [:db/ident :user])]
    [[:db/add id :note/label label]
     [:db/add id :note/description desc]
     [:db/retract (:db/id (:user/camera user)) :camera/selected id]]))

;; --- Props Images ---

(defmethod
  ^{:doc "Creates state representations of images used as props which
          include metadata like image dimensions, filename, and their
          thumbnails. Uniquely identified by their SHA-1 hash digest."}
  event-tx-fn :props-images/create-many
  [_ _ images]
  (into
   [{:db/ident :root
     :root/props-images
     (for [[{:keys [hash name size width height]} _] images]
       {:image/hash hash
        :image/name name
        :image/size size
        :image/width width
        :image/height height})}] cat
   (for [[image thumbnail] images]
     (if (= (:hash image) (:hash thumbnail))
       [{:image/hash (:hash image) :image/thumbnail [:image/hash (:hash image)]}]
       [{:image/hash (:hash thumbnail)
         :image/name (:name thumbnail)
         :image/size (:size thumbnail)
         :image/width (:width thumbnail)
         :image/height (:height thumbnail)}
        {:image/hash (:hash image) :image/thumbnail [:image/hash (:hash thumbnail)]}]))))

(defmethod
  ^{:doc "Removes all prop images as well as any props from all scenes."}
  event-tx-fn :props-images/remove-all
  [data _ _]
  (let [user (ds/entity data [:db/ident :user])
        xfrm (comp (map :camera/scene) (mapcat :scene/props) (map :db/id))]
    (into [[:db/retract [:db/ident :root] :root/props-images]]
          (for [id (sequence xfrm (:user/cameras user))]
            [:db/retractEntity id]))))

(defmethod
  ^{:doc "Removes a prop image as well as any instances of that image
          in all scenes."}
  event-tx-fn :props-images/remove
  [data _ hash]
  (let [user (ds/entity data [:db/ident :user])
        xfrm (comp
              (mapcat (comp :scene/props :camera/scene))
              (filter (comp #{hash} :image/hash :prop/image)))]
    (into [[:db/retractEntity [:image/hash hash]]]
          (for [entity (sequence xfrm (:user/cameras user))]
            [:db/retractEntity (:db/id entity)]))))

;; --- Props ---

(defmethod
  ^{:doc "Creates a new prop image in the current scene at the given
          screen-space point."}
  event-tx-fn :props/create
  [data _ point hash]
  (let [{{bounds :user/bounds
          {camera-point :camera/point
           camera-scale :camera/scale
           camera-id :db/id
           {scene-id :db/id}
           :camera/scene}
          :user/camera} :root/user}
        (ds/entity data [:db/ident :root])
        {width :image/width
         height :image/height}
        (ds/entity data [:image/hash hash])
        xform
        (-> (matrix/translate matrix/identity camera-point)
            (matrix/translate (/ width -2) (/ height -2))
            (matrix/scale (/ (or camera-scale 1)))
            (matrix/translate (vec/mul (.-a bounds) -1)))]
    (if (geom/point-within-rect? point bounds)
      [[:db/add -1 :object/point (xform point)]
       [:db/add -1 :object/type :prop/prop]
       [:db/add -1 :prop/image [:image/hash hash]]
       [:db/add camera-id :camera/selected -1]
       [:db/add scene-id :scene/props -1]]
      [])))
