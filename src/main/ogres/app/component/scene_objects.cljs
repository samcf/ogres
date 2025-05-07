(ns ogres.app.component.scene-objects
  (:require [clojure.string :refer [join]]
            [goog.object :refer [getValueByKeys]]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.scene-context-menu :refer [context-menu]]
            [ogres.app.component.scene-pattern :refer [pattern]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [ogres.app.vec :as vec :refer [Vec2 Segment]]
            [react-transition-group :refer [TransitionGroup CSSTransition]]
            [uix.core :as uix :refer [defui $]]
            [uix.dom :as dom]
            ["@dnd-kit/core"
             :refer [useDndMonitor useDraggable]
             :rename {useDndMonitor use-dnd-monitor
                      useDraggable use-draggable}]))

(def ^:private note-icons
  ["journal-bookmark-fill" "dice-5" "door-open" "geo-alt" "fire" "skull" "question-circle"])

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

(defn ^:private objects-xf [user]
  (filter (fn [entity] (or (= user :host) (not (:object/hidden entity))))))

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
              ($ :.scene-note-label (:note/label entity))
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

(defui ^:private object [props]
  (case (keyword (namespace (:object/type (:entity props))))
    :shape ($ object-shape props)
    :token ($ object-token props)
    :note  ($ object-note props)))

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
           [:token/size :default 5]
           [:token/light :default 15]
           [:token/aura-radius :default 0]
           {:token/image [:image/hash]}
           {:scene/_initiative [:db/id :initiative/turn]}]}
         {:scene/shapes
          [:db/id
           [:object/type :default :shape/circle]
           [:object/point :default vec/zero]
           [:shape/points :default [vec/zero]]
           [:shape/color :default "red"]
           [:shape/pattern :default :solid]]}
         {:scene/notes
          [:db/id
           [:object/type :default :note/note]
           [:object/point :default vec/zero]
           [:object/hidden :default true]
           [:object/locked :default true]
           [:note/icon :default "journal-bookmark-fill"]
           [:note/label :default ""]
           [:note/description :default ""]
           {:camera/_selected
            [:camera/selected
             {:user/_camera
              [{:root/_user
                [[:user/type :default :host]]}]}]}]}]}]}]
    :root/session
    [{:session/conns
      [:db/ident :user/uuid :user/color :user/dragging]}]}])

(defui objects []
  (let [[_ set-ready] (uix/use-state false)
        result (hooks/use-query query [:db/ident :root])
        {{bounds :user/bounds
          type :user/type
          {point :camera/point
           scale :camera/scale
           selected :camera/selected
           {outline? :scene/show-object-outlines
            align? :scene/grid-align
            shapes :scene/shapes
            tokens :scene/tokens
            notes :scene/notes}
           :camera/scene}
          :user/camera} :root/user
         {conns :session/conns} :root/session} result
        portal (uix/use-ref)
        screen (Segment. point (vec/add point (vec/div (.-b (vec/rebase bounds)) scale)))
        notes  (sort compare-objects notes)
        shapes (sort compare-objects shapes)
        tokens (sort compare-tokens (sequence (tokens-xf type) tokens))
        entities (into [] (objects-xf type) (into tokens (into shapes notes)))
        selected (into #{} (map :db/id) selected)
        dragging (into {} user-drag-xf conns)
        bound-xf (comp (filter (comp selected :db/id))
                       (map geom/object-bounding-rect)
                       (mapcat seq))]

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
                    locked? (and (= type :conn) (:object/locked entity))
                    node (uix/create-ref)
                    user (dragging id)
                    rect (geom/object-bounding-rect entity)
                    seen (geom/rect-intersects-rect rect screen)
                    type (keyword (namespace (:object/type entity)))]]
          ($ CSSTransition {:key id :nodeRef node :timeout 256}
            ($ :g.scene-object-transition {:ref node}
              (if (not (selected id))
                ($ drag-remote-fn {:user (:user/uuid user) :point point}
                  (fn [remote]
                    ($ drag-local-fn {:id id :disabled (or user locked?)}
                      (fn [drag]
                        (let [drag-fn (getValueByKeys drag "listeners" "onPointerDown")
                              drag-x (getValueByKeys drag "transform" "x")
                              drag-y (getValueByKeys drag "transform" "y")
                              local (Vec2. (or drag-x 0) (or drag-y 0))
                              delta (or remote local)]
                          ($ :g.scene-object
                            {:ref (.-setNodeRef drag)
                             :transform (vec/add point delta)
                             :tab-index (if (and (not locked?) seen) 0 -1)
                             :on-pointer-down (or drag-fn stop-propagation)
                             :data-drag-remote (some? user)
                             :data-drag-local (.-isDragging drag)
                             :data-color (:user/color user)
                             :data-type (name type)
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
        (let [bounds (transduce bound-xf geom/bounding-rect-rf entities)]
          ($ drag-local-fn {:id "selected" :disabled (some dragging selected)}
            (fn [drag]
              (let [drag-fn (getValueByKeys drag "listeners" "onPointerDown")
                    drag-x (getValueByKeys drag "transform" "x")
                    drag-y (getValueByKeys drag "transform" "y")
                    local (Vec2. (or drag-x 0) (or drag-y 0))]
                ($ :g.scene-objects.scene-objects-selected
                  {:ref (.-setNodeRef drag)
                   :transform local
                   :on-pointer-down (or drag-fn stop-propagation)
                   :data-drag-local (.-isDragging drag)}
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
                  (let [sz 400
                        tx (-> (+ (.-x (.-a bounds)) (.-x (.-b bounds))) (* scale) (- sz) (/ 2) int)
                        ty (-> (+ (.-y (.-b bounds)) 24) (* scale) (- 24) int)
                        tf (str "scale(" (/ scale) ")")]
                    ($ :foreignObject.context-menu-object
                      {:x tx :y ty :width sz :height sz :transform tf}
                      ($ context-menu
                        {:data (sequence (filter (comp selected :db/id)) entities)
                         :type type}))))))))))))
