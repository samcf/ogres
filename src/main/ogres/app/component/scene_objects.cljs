(ns ogres.app.component.scene-objects
  (:require [clojure.string :refer [join]]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.scene-context-menu :refer [context-menu]]
            [ogres.app.component.scene-pattern :refer [pattern]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [ogres.app.matrix :as matrix]
            [ogres.app.modifiers :as modifiers]
            [ogres.app.util :as util]
            [ogres.app.vec :as vec :refer [Vec2 Segment]]
            [react-transition-group :refer [TransitionGroup CSSTransition]]
            [uix.core :as uix :refer [defui $]]
            [uix.dom :as dom]
            ["@dnd-kit/core"
             :refer [useDndMonitor useDraggable DndContext]
             :rename {DndContext dnd-context
                      useDndMonitor use-dnd-monitor
                      useDraggable use-draggable}]))

(def ^:private note-icons
  ["journal-bookmark-fill" "dice-5" "door-open" "geo-alt" "fire" "skull" "question-circle"])

(defn ^:private resize-cursor [deg]
  (cond
    (> deg 345) "ew-resize"
    (> deg 285) "nesw-resize"
    (> deg 255) "ns-resize"
    (> deg 195) "nwse-resize"
    (> deg 165) "ew-resize"
    (> deg 105) "nesw-resize"
    (> deg 75)  "ns-resize"
    (> deg 15)  "nwse-resize"
    (> deg 0)   "ew-resize"))

(defn ^:private stop-propagation
  "Defines an event handler that ceases event propagation."
  [event]
  (.stopPropagation event))

(defn ^:private compare-objects
  "Defines a comparator function for shapes."
  [a b]
  (let [{point-a :object/point} a
        {point-b :object/point} b]
    (compare
     [(.-x point-a) (.-y point-a)]
     [(.-x point-b) (.-y point-b)])))

(defn ^:private compare-tokens
  "Defines a comparator function for tokens."
  [a b]
  (let [{size-a :token/size point-a :object/point} a
        {size-b :token/size point-b :object/point} b]
    (compare
     [size-b (.-x point-a) (.-y point-a)]
     [size-a (.-x point-b) (.-y point-b)])))

(def ^{:private true
       :doc "Defines a transducer which expects a collection of user
             entities, returning a sequence of key-value pairs whose
             keys are the IDs of the objects currently being dragged
             and whose values are the user that is dragging that object."}
  user-drag-xf
  (comp (filter (comp nil? :db/ident))
        (filter (comp seq :user/dragging))
        (mapcat (fn [user]
                  (map (juxt :db/id (constantly user))
                       (:user/dragging user))))))

(defn ^:private tokens-xf
  "Defines a transducer which expects a collection of token entities
   and returns only the elements suitable for rendering given the
   user type."
  [user]
  (filter
   (fn [token]
     (or (= user :host) (not (:object/hidden token))))))

(defn ^:private use-cursor-point
  "Defines a React state hook which returns a point [Ax Ay] of the
   given user's current cursor position, if available."
  [uuid point]
  (let [[cursor set-cursor] (uix/use-state nil)]
    (uix/use-effect
     (fn []
       (if (nil? uuid)
         (set-cursor nil))) [uuid])
    (hooks/use-subscribe :cursor/moved
      (uix/use-callback
       (fn [id cx cy]
         (if (= id uuid)
           (set-cursor
            (fn [[_ _ dx dy]]
              (let [rx (- cx (.-x point)) ry (- cy (.-y point))]
                (if (nil? dx)
                  [(- rx rx) (- ry ry) rx ry]
                  [(- cx (.-x point) dx) (- cy (.-y point) dy) dx dy])))))) [uuid point]))
    cursor))

(defui ^:private drag-remote-fn
  "Renders the given children as a function with the user's current
   cursor position as its only argument in the form of [Ax Ay]."
  [{:keys [children user point]}]
  (let [point (use-cursor-point user point)]
    (if (nil? point)
      (children nil)
      (children (Vec2. (point 0) (point 1))))))

(defui ^:private drag-local-fn
  "Renders the given children as a function with an object of drag
   parameters passed as its only argument.
   https://docs.dndkit.com/api-documentation/draggable/usedraggable"
  [{:keys [children id disabled]
    :or   {disabled false}}]
  (let [options (use-draggable #js {"id" id "disabled" (boolean disabled)})]
    (children options)))

(defui ^:private shape-circle [props]
  (let [{{[dst] :shape/points} :entity} props]
    ($ :circle.scene-shape-fill
      {:r (vec/dist-cheb dst)})))

(defui ^:private shape-rect [props]
  (let [{{[dst] :shape/points} :entity} props]
    ($ :path.scene-shape-fill
      {:d (join " " [\M 0 0 \H (.-x dst) \V (.-y dst) \H 0 \Z])})))

(defui ^:private shape-line [props]
  (let [{{[dst] :shape/points} :entity} props]
    ($ :polygon.scene-shape-fill
      {:points (->> (Segment. vec/zero dst) (geom/line-points) (mapcat seq) (join " "))})))

(defui ^:private shape-cone [props]
  (let [{{[dst] :shape/points} :entity} props]
    ($ :polygon.scene-shape-fill
      {:points (->> (Segment. vec/zero dst) (geom/cone-points) (mapcat seq) (join " "))})))

(defui ^:private shape-poly [props]
  (let [{{points :shape/points} :entity} props]
    ($ :polygon.scene-shape-fill
      {:points (join " " (into [0 0] (mapcat seq) points))})))

(defui ^:private shape [props]
  (case (:object/type (:entity props))
    :shape/circle ($ shape-circle props)
    :shape/rect   ($ shape-rect props)
    :shape/line   ($ shape-line props)
    :shape/cone   ($ shape-cone props)
    :shape/poly   ($ shape-poly props)
    nil))

(defui ^:private object-shape [props]
  (let [{{id :db/id shape-pattern :shape/pattern :as entity} :entity} props]
    ($ :g.scene-shape {:data-color (:shape/color entity)}
      ($ :defs.scene-shape-defs
        ($ pattern {:id (str "shape-pattern-" id) :name shape-pattern}))
      (if (not= (:object/type entity) :shape/rect)
        (let [bounds (geom/object-bounding-rect entity)]
          ($ :rect.scene-shape-bounds
            {:width (vec/width bounds)
             :height (vec/height bounds)
             :transform (vec/sub (.-a bounds) (:object/point entity))})))
      ($ :g.scene-shape-path
        {:fill (str "url(#shape-pattern-" id ")")
         :fill-opacity (if (= shape-pattern :solid) 0.40 0.80)}
        ($ shape props)))))

(defui ^:private object-token [props]
  ($ :use
    {:href (str "#token" (:db/id (:entity props)))}))

(defui ^:private object-note [props]
  (let [dispatch  (hooks/use-dispatch)
        entity    (:entity props)
        id        (:db/id entity)
        hidden    (:object/hidden entity)
        camera    (first (:camera/_selected entity))
        selected  (into #{} (map :db/id) (:camera/selected camera))
        selected? (and (contains? camera :user/_camera) (= selected #{id}))]
    ($ :foreignObject.scene-object-note
      {:x -8 :y -8 :width 362 :height (if selected? 334 58) :data-selected selected?}
      ($ :.scene-note {:data-hidden hidden}
        ($ :.scene-note-header
          ($ :.scene-note-anchor
            ($ icon {:name (:note/icon entity) :size 26}))
          ($ :.scene-note-nav
            ($ :.scene-note-navinner
              ($ :.scene-note-label
                (let [label (:note/label entity)]
                  (if (and (some? label) (not= label ""))
                    label "Unlabeled note")))
              ($ :.scene-note-control
                {:on-pointer-down stop-propagation
                 :on-click (fn [] (dispatch :objects/toggle-hidden id))}
                ($ icon {:name (if hidden "eye-slash-fill" "eye-fill") :size 22}))
              ($ :.scene-note-control
                {:on-pointer-down stop-propagation
                 :on-click (fn [] (dispatch :objects/remove [id]))}
                ($ icon {:name "trash3-fill" :size 22})))))
        (if selected?
          ($ :.scene-note-body {:on-pointer-down stop-propagation}
            ($ :ul.scene-note-icons
              (for [icon-name note-icons]
                ($ :li {:key icon-name}
                  ($ :label
                    ($ :input
                      {:type "radio"
                       :name "note-icon"
                       :value icon-name
                       :checked (= (:note/icon entity) icon-name)
                       :on-change
                       (fn [event]
                         (dispatch :note/change-icon (:db/id entity) (.. event -target -value)))})
                    ($ icon {:name icon-name})))))
            ($ :form.scene-note-form
              {:on-blur
               (fn [event]
                 (let [name  (.. event -target -name)
                       value (.. event -target -value)]
                   (cond (and (= name "label") (not= (:note/label entity) value))
                         (dispatch :note/change-label id value)
                         (and (= name "description") (not= (:note/description entity) value))
                         (dispatch :note/change-description id value))))
               :on-submit
               (fn [event]
                 (.preventDefault event)
                 (let [input (.. event -target -elements)
                       label (.. input -label -value)
                       descr (.. input -description -value)]
                   (dispatch :note/change-details id label descr)))}
              ($ :fieldset.fieldset
                ($ :legend "Label")
                ($ :input.text.text-ghost
                  {:type "text"
                   :name "label"
                   :auto-complete "off"
                   :default-value (:note/label entity)
                   :placeholder "Spider's Ballroom"}))
              ($ :fieldset.fieldset
                ($ :legend "Description")
                ($ :textarea
                  {:name "description"
                   :auto-complete "off"
                   :default-value (:note/description entity)}))
              ($ :input {:type "submit" :hidden true}))))))))

(defui ^:private ^:memo object-prop-scale
  [{:keys [point size angle]}]
  (let [option #js {"id" (str "resize/" point) "data" #js {"type" "resize" "point" point}}
        resize (use-draggable option)
        handle (and (.-listeners resize) (.-onPointerDown (.-listeners resize)))
        cursor (resize-cursor angle)]
    ($ :<>
      (if (.-isDragging resize)
        (dom/create-portal
         ($ :.cursor-region
           {:style {:cursor cursor}}) js/document.body))
      ($ :rect.scene-prop-anchor
        {:data-dragging (.-isDragging resize)
         :on-pointer-down handle
         :style {:cursor cursor}
         :x (- (.-x point) (/ size 2))
         :y (- (.-y point) (/ size 2))
         :width size
         :height size}))))

(defui ^:private ^:memo object-prop-rotate
  [{:keys [point size]}]
  (let [option #js {"id" "rotate" "data" #js {"type" "rotate" "point" point}}
        rotate (use-draggable option)
        handle (and (.-listeners rotate) (.-onPointerDown (.-listeners rotate)))
        cursor (if (.-isDragging rotate) "grabbing" "grab")]
    ($ :<>
      (if (.-isDragging rotate)
        (dom/create-portal
         ($ :.cursor-region
           {:style {:cursor "grabbing"}}) js/document.body))
      ($ :circle.scene-prop-anchor
        {:data-dragging (.-isDragging rotate)
         :on-pointer-down handle
         :style {:cursor cursor}
         :cx (.-x point)
         :cy (.-y point)
         :r size}))))

(defui ^:private object-prop-edit [props]
  (let [{{id :db/id
          object-scale :object/scale
          object-rotation :object/rotation
          {width :image/width
           height :image/height} :prop/image
          [{zoom :camera/scale}] :camera/_selected} :entity
         transform :transform} props
        [scale set-scale] (uix/use-state object-scale)
        [rotation set-rotation] (uix/use-state object-rotation)
        dispatch (hooks/use-dispatch)
        bounds (Segment. vec/zero (Vec2. width height))
        center (vec/midpoint bounds)
        get-scale
        (fn [^js/Object event]
          (let [data (.. event -active -data -current)
                dx (.-x (.-delta event))
                dy (.-y (.-delta event))]
            (-> (vec/shift (transform (.-point data)) dx dy)
                (vec/dist center)
                (/ (vec/dist center)))))
        get-rotation
        (fn [^js/Object event]
          (let [data (.. event -active -data -current)
                dx (.-x (.-delta event))
                dy (.-y (.-delta event))
                dg (-> (vec/shift (transform (.-point data)) dx dy)
                       (vec/sub center)
                       (vec/angle)
                       (+ 90))
                rd (util/round dg 45)]
            (if (< (abs (- dg rd)) 5) rd dg)))]
    (uix/use-effect
     (fn []
       (set-scale object-scale)
       (set-rotation object-rotation))
     [object-scale object-rotation])
    (use-dnd-monitor
     #js {"onDragMove"
          (fn [event]
            (case (.. event -active -data -current -type)
              "resize" (set-scale (get-scale event))
              "rotate" (set-rotation (get-rotation event))))
          "onDragEnd"
          (fn [event]
            (case (.. event -active -data -current -type)
              "resize" (dispatch :object/change-scale id (get-scale event))
              "rotate" (dispatch :object/change-rotation id (get-rotation event))))})
    ($ :g.scene-prop
      {:style
       {:transform
        (-> (matrix/translate matrix/identity center)
            (matrix/scale scale)
            (matrix/rotate rotation)
            (matrix/translate (vec/mul center -1)))}}
      (:children props)
      (for [point (geom/rect-points bounds)]
        ($ object-prop-scale
          {:key point
           :point point
           :size (/ 8 scale zoom)
           :angle (vec/angle (vec/sub (transform point) center))}))
      ($ object-prop-rotate
        {:point (Vec2. (.-x center) (/ 26 scale zoom -1))
         :size (/ 5 scale zoom)}))))

(defui ^:private object-prop [props]
  (let [{{id :db/id
          hidden :object/hidden
          locked :object/locked
          {hash :image/hash
           width :image/width
           height :image/height} :prop/image
          [{selected :camera/selected
            [{user :root/_user}] :user/_camera
            zoom :camera/scale}] :camera/_selected} :entity} props
        url-image (hooks/use-image hash)
        mod-scale (uix/use-memo (fn [] (modifiers/scale-fn zoom)) [zoom])
        transform (geom/object-transform (:entity props))
        selected (into #{} (map :db/id) selected)]
    (if (and (some? user) (not locked) (= #{id} selected))
      ($ dnd-context
        #js {"modifiers" #js [mod-scale modifiers/trunc]}
        ($ object-prop-edit
          (assoc props :transform transform)
          ($ :image.scene-prop-image
            {:data-hidden hidden
             :width width
             :height height
             :href url-image})
          ($ :rect.scene-prop-bounds
            {:width width :height height})))
      ($ :g.scene-prop {:style {:transform transform}}
        ($ :image.scene-prop-image
          {:data-hidden hidden
           :width width
           :height height
           :href url-image})
        ($ :rect.scene-prop-bounds
          {:width width :height height})))))

(defui ^:private object [props]
  (case (:object/type (:entity props))
    :token/token ($ object-token props)
    :note/note ($ object-note props)
    :prop/prop ($ object-prop props)
    ($ object-shape props)))

(defn ^:private use-drag-listener []
  (let [dispatch (hooks/use-dispatch)]
    (use-dnd-monitor
     #js {"onDragStart"
          (uix/use-callback
           (fn [data]
             (let [id (.. data -active -id)]
               (if (= id "selected")
                 (dispatch :drag/start-selected)
                 (dispatch :drag/start id)))) [dispatch])
          "onDragCancel"
          (uix/use-callback
           (fn []
             (dispatch :drag/end)) [dispatch])
          "onDragEnd"
          (uix/use-callback
           (fn [data]
             (let [event (.-activatorEvent data)
                   ident (.. data -active -id)
                   delta (Vec2. (.. data -delta -x) (.. data -delta -y))
                   shift (.-shiftKey event)]
               (if (= delta vec/zero)
                 (if (= ident "selected")
                   (let [id (.. event -target (closest "[data-id]") -dataset -id)]
                     (dispatch :objects/select (js/Number id) shift))
                   (dispatch :objects/select ident shift))
                 (if (= ident "selected")
                   (dispatch :objects/translate-selected delta)
                   (dispatch :objects/translate ident delta))))) [dispatch])})))

(defui ^:private object-hint [props]
  (let [{{point :object/point type :object/type :as entity} :entity
         portal :portal
         delta :delta
         is-outline :is-outline
         is-aligned :is-aligned} props
        is-aligning (and is-aligned (not= delta vec/zero))]
    ($ :<>
      (if (and (= type :token/token) is-aligning)
        (let [rect (vec/rnd (vec/add (geom/object-bounding-rect entity) delta) grid-size)]
          (dom/create-portal
           ($ :rect.scene-object-align
             {:width (vec/width rect)
              :height (vec/height rect)
              :transform (.-a rect)}) portal)))
      (if (= (namespace type) "shape")
        (let [align-to (geom/object-alignment entity)
              aligned (vec/rnd (vec/add point delta) (if is-aligning align-to 1))]
          (dom/create-portal
           ($ :<>
             (if is-outline
               (let [path (geom/object-tile-path entity (vec/sub aligned point))]
                 (if (seq path)
                   ($ :polygon.scene-object-tiles
                     {:points (join " " (mapcat seq path))}))))
             (if is-aligning
               ($ :g.scene-object-ghost
                 {:transform aligned}
                 ($ :circle.scene-object-anchor {:r 3})
                 ($ :circle.scene-object-anchor-ring {:r 5})
                 ($ shape {:entity entity})))) portal))))))

(def ^:private query
  [{:root/user
    [:user/type
     [:user/bounds :default vec/zero-segment]
     {:user/camera
      [:db/id
       :camera/selected
       [:camera/scale :default 1]
       [:camera/point :default vec/zero]
       {:camera/scene
        [[:scene/grid-align :default false]
         [:scene/show-object-outlines :default true]
         {:scene/tokens
          [:db/id
           [:object/type :default :token/token]
           [:object/point :default vec/zero]
           [:object/hidden :default false]
           [:token/label :default ""]
           [:token/flags :default #{}]
           [:token/size :default 5]
           [:token/light :default 15]
           [:token/aura-radius :default 0]
           {:token/image [:image/hash]}
           {:scene/_initiative [:db/id :initiative/turn]}]}
         {:scene/shapes
          [:db/id
           [:object/type :default :shape/circle]
           [:object/point :default vec/zero]
           [:object/locked :default false]
           [:shape/points :default [vec/zero]]
           [:shape/color :default "red"]
           [:shape/pattern :default :solid]]}
         {:scene/props
          [:db/id
           [:object/type :default :prop/prop]
           [:object/point :default vec/zero]
           [:object/scale :default 1]
           [:object/rotation :default 0]
           [:object/hidden :default false]
           [:object/locked :default false]
           {:prop/image
            [:image/hash
             [:image/width :default 0]
             [:image/height :default 0]]}
           {:camera/_selected
            [[:camera/scale :default 1]
             :camera/selected
             {:user/_camera [:root/_user]}]}]}
         {:scene/notes
          [:db/id
           [:object/type :default :note/note]
           [:object/point :default vec/zero]
           [:object/hidden :default true]
           [:object/locked :default false]
           [:note/icon :default "journal-bookmark-fill"]
           [:note/description :default ""]
           :note/label
           {:camera/_selected
            [:camera/selected
             {:user/_camera
              [{:root/_user
                [[:user/type :default :host]]}]}]}]}]}]}]
    :root/session
    [{:session/conns
      [:db/ident :user/uuid :user/color :user/dragging]}]}])

(defui objects []
  (let [dispatch (hooks/use-dispatch)
        [_ set-ready] (uix/use-state false)
        result (hooks/use-query query [:db/ident :root])
        {{bounds :user/bounds
          user-type :user/type
          {point :camera/point
           scale :camera/scale
           selected :camera/selected
           {outline? :scene/show-object-outlines
            align? :scene/grid-align
            shapes :scene/shapes
            tokens :scene/tokens
            props :scene/props
            notes :scene/notes}
           :camera/scene}
          :user/camera} :root/user
         {conns :session/conns} :root/session} result
        portal (uix/use-ref)
        screen (Segment. point (vec/add point (vec/div (.-b (vec/rebase bounds)) scale)))
        selected (into #{} (map :db/id) selected)
        dragging (into {} user-drag-xf conns)
        bound-xf
        (comp (filter (comp selected :db/id))
              (map geom/object-bounding-rect)
              (mapcat seq))
        entities
        (into []
              (filter
               (fn [entity]
                 (or (not (:object/hidden entity)) (= user-type :host))))
              (concat
               (sort compare-objects props)
               (sort compare-objects shapes)
               (sort compare-objects notes)
               (sort compare-tokens (sequence (tokens-xf user-type) tokens))))]

    ;; automatically re-render once the portal ref is initialized.
    (uix/use-effect
     (fn [] (set-ready true)) [])

    (use-drag-listener)
    ($ :g.scene-objects {}
      ($ :g.scene-objects-portal
        {:ref portal :tab-index -1})
      ($ TransitionGroup {:component nil}
        (for [entity entities
              :let [{id :db/id point :object/point} entity
                    lock (or (:object/locked entity)
                             (contains? dragging id)
                             (and (= user-type :conn)
                                  (or (= (:object/type entity) :note/note)
                                      (= (:object/type entity) :prop/prop))))
                    node (uix/create-ref)
                    user (dragging id)
                    rect (geom/object-bounding-rect entity)
                    seen (geom/rect-intersects-rect rect screen)]]
          ($ CSSTransition {:key id :nodeRef node :timeout 256}
            ($ :g.scene-object-transition {:ref node}
              (if (not (selected id))
                ($ drag-remote-fn {:user (:user/uuid user) :point point}
                  (fn [remote]
                    ($ drag-local-fn {:id id :disabled lock}
                      (fn [^js/Object drag]
                        (let [drag-fn (and (.-listeners drag) (.-onPointerDown (.-listeners drag)))
                              drag-x (and (.-transform drag) (.-x (.-transform drag)))
                              drag-y (and (.-transform drag) (.-y (.-transform drag)))
                              local (Vec2. (or drag-x 0) (or drag-y 0))
                              delta (or remote local)]
                          ($ :g.scene-object
                            {:ref (.-setNodeRef drag)
                             :transform (vec/add point delta)
                             :tab-index (if (and (not lock) seen) 0 -1)
                             :on-pointer-down drag-fn
                             :on-double-click
                             (fn []
                               (if (and (not= user-type :conn) lock)
                                 (dispatch :objects/select (:db/id entity))))
                             :data-drag-remote (some? user)
                             :data-drag-local (.-isDragging drag)
                             :data-locked (boolean lock)
                             :data-color (:user/color user)
                             :data-type (name (keyword (namespace (:object/type entity))))
                             :data-id id}
                            ($ object {:entity entity})
                            (if-let [portal (deref portal)]
                              ($ object-hint
                                {:entity entity
                                 :portal portal
                                 :delta delta
                                 :is-outline outline?
                                 :is-aligned align?})))))))))))))
      ($ hooks/use-portal {:name :selected}
        (let [select (filter (comp selected :db/id) entities)
              bounds (transduce bound-xf geom/bounding-rect-rf entities)
              locked (or (some dragging selected)
                         (and (= (count selected) 1) (:object/locked (first select)))
                         (and (= user-type :conn)
                              (some
                               (fn [entity]
                                 (or (= (:object/type entity) :note/note)
                                     (= (:object/type entity) :prop/prop))) select)))]
          ($ drag-local-fn {:id "selected" :disabled locked}
            (fn [^js/Object drag]
              (let [drag-fn (and (.-listeners drag) (.-onPointerDown (.-listeners drag)))
                    drag-x (and (.-transform drag) (.-x (.-transform drag)))
                    drag-y (and (.-transform drag) (.-y (.-transform drag)))
                    local (Vec2. (or drag-x 0) (or drag-y 0))]
                ($ :g.scene-objects.scene-objects-selected
                  {:ref (.-setNodeRef drag)
                   :transform local
                   :on-pointer-down drag-fn
                   :data-drag-local (.-isDragging drag)
                   :data-locked locked}
                  (if (> (count selected) 1)
                    ($ :rect.scene-objects-bounds
                      {:width (vec/width bounds)
                       :height (vec/height bounds)
                       :transform (.-a bounds)}))
                  ($ TransitionGroup {:component nil}
                    (for [entity entities
                          :let [{id :db/id point :object/point} entity
                                node (uix/create-ref)
                                user (dragging id)]]
                      ($ CSSTransition {:key id :nodeRef node :timeout 256}
                        ($ :g.scene-object-transition {:ref node}
                          (if (selected id)
                            ($ drag-remote-fn {:user (:user/uuid user) :point point}
                              (fn [remote]
                                (let [delta (or remote local)]
                                  ($ :g.scene-object
                                    {:transform (vec/add point (or remote vec/zero))
                                     :data-drag-remote (some? user)
                                     :data-drag-local (.-isDragging drag)
                                     :data-color (:user/color user)
                                     :data-id id}
                                    ($ object {:entity entity})
                                    (if-let [portal (deref portal)]
                                      ($ object-hint
                                        {:entity entity
                                         :portal portal
                                         :delta delta
                                         :is-outline outline?
                                         :is-aligned align?})))))))))))
                  (if (seq select)
                    (let [sz 400
                          xf (matrix/scale matrix/identity scale)
                          bn (xf bounds)]
                      ($ :foreignObject.context-menu-object
                        {:x (.-x (vec/shift (vec/midpoint bn) (/ sz -2)))
                         :y (.-y (.-b bn))
                         :width sz
                         :height sz
                         :transform (matrix/inverse xf)
                         :data-type (namespace (:object/type (first select)))}
                        ($ context-menu
                          {:data select
                           :user user-type})))))))))))))
