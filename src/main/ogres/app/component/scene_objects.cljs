(ns ogres.app.component.scene-objects
  (:require [clojure.string :refer [join]]
            [goog.object :refer [getValueByKeys]]
            [ogres.app.component :refer [icon]]
            [ogres.app.component.scene-context-menu :refer [context-menu]]
            [ogres.app.component.scene-pattern :refer [pattern]]
            [ogres.app.const :refer [half-size]]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :as hooks]
            [react-transition-group :refer [TransitionGroup CSSTransition]]
            [uix.core :as uix :refer [defui $]]
            [uix.dom :as dom]
            ["@dnd-kit/core"
             :refer [useDndMonitor useDraggable]
             :rename {useDndMonitor use-dnd-monitor
                      useDraggable use-draggable}]))

(def ^:private rf-points->poly
  (completing into (fn [xs] (join " " xs))))

(def ^:private note-icons
  ["journal-bookmark-fill" "dice-5" "door-open" "geo-alt" "fire" "skull" "question-circle"])

(defn ^:private stop-propagation
  "Defines an event handler that ceases event propagation."
  [event]
  (.stopPropagation event))

(defn ^:private compare-objects
  "Defines a comparator function for shapes."
  [a b]
  (let [{[ax ay] :object/point} a
        {[bx by] :object/point} b]
    (compare [ax ay] [bx by])))

(defn ^:private compare-tokens
  "Defines a comparator function for tokens."
  [a b]
  (let [{size-a :token/size [ax ay] :object/point} a
        {size-b :token/size [bx by] :object/point} b]
    (compare [size-b ax ay] [size-a bx by])))

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
     (or (= user :host)
         (not (contains? (:token/flags token) :hidden))))))

(defn ^:private objects-xf [user]
  (filter (fn [entity] (or (= user :host) (not (:object/hidden entity))))))

(defn ^:private use-cursor-point
  "Defines a React state hook which returns a point [Ax Ay] of the
   given user's current cursor position, if available."
  [uuid ox oy]
  (let [[point set-point] (uix/use-state nil)]
    (uix/use-effect
     (fn []
       (if (nil? uuid)
         (set-point nil))) [uuid])
    (hooks/use-subscribe :cursor/moved
      (uix/use-callback
       (fn [id cx cy]
         (if (= id uuid)
           (set-point
            (fn [[_ _ dx dy]]
              (let [rx (- cx ox) ry (- cy oy)]
                (if (nil? dx)
                  [(- rx rx) (- ry ry) rx ry]
                  [(- cx ox dx) (- cy oy dy) dx dy])))))) [uuid ox oy])) point))

(defui ^:private drag-remote-fn
  "Renders the given children as a function with the user's current
   cursor position as its only argument in the form of [Ax Ay]."
  [{:keys [children user x y]}]
  (let [point (use-cursor-point user x y)]
    (children point)))

(defui ^:private drag-local-fn
  "Renders the given children as a function with an object of drag
   parameters passed as its only argument.
   https://docs.dndkit.com/api-documentation/draggable/usedraggable"
  [{:keys [children id disabled]
    :or   {disabled false}}]
  (let [options (use-draggable #js {"id" id "disabled" (boolean disabled)})]
    (children options)))

(defui ^:private shape-circle [props]
  (let [{[ax ay] :shape/points} (:entity props)
        radius (geom/chebyshev-distance 0 0 ax ay)]
    ($ :circle.scene-shape-fill
      {:cx 0
       :cy 0
       :r radius
       :fill (str "url(#shape-pattern-" (:db/id (:entity props)) ")")})))

(defui ^:private shape-rect [props]
  (let [{[ax ay] :shape/points} (:entity props)]
    ($ :path.scene-shape-fill
      {:d (join " " [\M 0 0 \H ax \V ay \H 0 \Z])
       :fill (str "url(#shape-pattern-" (:db/id (:entity props)) ")")})))

(defui ^:private shape-line [props]
  (let [{[ax ay] :shape/points} (:entity props)]
    ($ :polygon.scene-shape-fill
      {:points (join " " (geom/line-points 0 0 ax ay half-size))
       :fill (str "url(#shape-pattern-" (:db/id (:entity props)) ")")})))

(defui ^:private shape-cone [props]
  (let [{[bx by] :shape/points} (:entity props)]
    ($ :polygon.scene-shape-fill
      {:points (join " " (geom/cone-points 0 0 bx by))
       :fill (str "url(#shape-pattern-" (:db/id (:entity props)) ")")})))

(defui ^:private shape-poly [props]
  (let [{points :shape/points} (:entity props)]
    ($ :polygon.scene-shape-fill
      {:points (join " " (into [0 0] points))
       :fill (str "url(#shape-pattern-" (:db/id (:entity props)) ")")})))

(defui ^:private shape [props]
  (case (keyword (name (:object/type (:entity props))))
    :circle ($ shape-circle props)
    :rect   ($ shape-rect props)
    :line   ($ shape-line props)
    :cone   ($ shape-cone props)
    :poly   ($ shape-poly props)))

(defui ^:private object-shape [props]
  (let [entity (:entity props)
        {id :db/id
         color :shape/color
         pattern-name :shape/pattern
         [ax ay] :object/point} entity
        [bx by cx cy] (geom/object-bounding-rect entity)]
    ($ :g.scene-shape {:data-color color}
      ($ :defs.scene-shape-defs
        ($ pattern {:id (str "shape-pattern-" id) :name pattern-name}))
      ($ :rect.scene-shape-bounds
        {:x (- bx ax 6)
         :y (- by ay 6)
         :rx 3
         :ry 3
         :width (+ (- cx bx) (* 6 2))
         :height (+ (- cy by) (* 6 2))})
      ($ :g.scene-shape-path
        {:fill-opacity (if (= pattern-name :solid) 0.40 0.80)}
        ($ shape props)))))

(defui ^:private object-token [props]
  (let [{id :db/id} (:entity props)]
    ($ :<>
      ($ :use {:href (str "#token" id)})
      (if-let [[ax ay bx by] (:aligned-to props)]
        (if-let [portal (deref (:portal props))]
          (dom/create-portal
           ($ :g.scene-object-align
             ($ :rect
               {:x  ax :y (+ ay 1)
                :rx 3  :ry 3
                :width (- bx ax 1)
                :height (- by ay 1)})) portal))))))

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
                   id (.. data -active -id)
                   dx (.. data -delta -x)
                   dy (.. data -delta -y)
                   sh (.-shiftKey event)]
               (if (and (= dx 0) (= dy 0))
                 (if (= id "selected")
                   (let [id (.. event -target (closest "[data-id]") -dataset -id)]
                     (dispatch :objects/select (js/Number id) sh))
                   (dispatch :objects/select id sh))
                 (if (= id "selected")
                   (dispatch :objects/translate-selected dx dy)
                   (dispatch :objects/translate id dx dy))))) [dispatch])})))

(def ^:private query
  [{:root/user
    [:user/type
     [:bounds/self :default [0 0 0 0]]
     {:user/camera
      [:db/id
       :camera/selected
       [:camera/scale :default 1]
       [:camera/point :default [0 0]]
       {:camera/scene
        [[:scene/grid-align :default false]
         [:scene/show-object-outlines :default true]
         {:scene/tokens
          [:db/id
           [:object/type :default :token/token]
           [:object/point :default [0 0]]
           [:token/flags :default #{}]
           [:token/label :default ""]
           [:token/size :default 5]
           [:token/light :default 15]
           [:token/aura-radius :default 0]
           {:token/image [:image/hash]}
           {:scene/_initiative [:db/id :initiative/turn]}]}
         {:scene/shapes
          [:db/id
           [:object/type :default :shape/circle]
           [:object/point :default [0 0]]
           [:shape/points :default [0 0]]
           [:shape/color :default "red"]
           [:shape/pattern :default :solid]]}
         {:scene/notes
          [:db/id
           [:object/type :default :note/note]
           [:object/point :default [0 0]]
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
        {{[_ _ bw bh] :bounds/self
          type :user/type
          {[cx cy]  :camera/point
           scale    :camera/scale
           selected :camera/selected
           {outlines :scene/show-object-outlines
            align? :scene/grid-align
            shapes :scene/shapes
            tokens :scene/tokens
            notes :scene/notes}
           :camera/scene}
          :user/camera} :root/user
         {conns :session/conns} :root/session} result
        portal (uix/use-ref)
        bounds [cx cy (+ (/ bw scale) cx) (+ (/ bh scale) cy)]
        notes  (sort compare-objects notes)
        shapes (sort compare-objects shapes)
        tokens (sort compare-tokens (sequence (tokens-xf type) tokens))
        entities (into [] (objects-xf type) (into tokens (into shapes notes)))
        selected (into #{} (map :db/id) selected)
        dragging (into {} user-drag-xf conns)
        boundsxf (comp (filter (comp selected :db/id)) (mapcat geom/object-bounding-rect))]

    ;; automatically re-render once the portal ref is initialized.
    (uix/use-effect
     (fn [] (set-ready true)) [])

    (use-drag-listener)
    ($ :g.scene-objects {}
      ($ :g.scene-objects-portal
        {:ref portal :tab-index -1})
      ($ TransitionGroup {:component nil}
        (for [entity entities
              :let [{id :db/id [ax ay] :object/point} entity
                    lock (and (= type :conn) (:object/locked entity))
                    node (uix/create-ref)
                    user (dragging id)
                    rect (geom/object-bounding-rect entity)
                    seen (geom/rect-intersects-rect rect bounds)]]
          ($ CSSTransition {:key id :nodeRef node :timeout 256}
            ($ :g.scene-object-transition {:ref node}
              (if (not (selected id))
                ($ drag-remote-fn {:user (:user/uuid user) :x ax :y ay}
                  (fn [[rx ry]]
                    ($ drag-local-fn {:id id :disabled (or user lock)}
                      (fn [drag]
                        (let [handler (getValueByKeys drag "listeners" "onPointerDown")
                              dx (or (getValueByKeys drag "transform" "x") 0)
                              dy (or (getValueByKeys drag "transform" "y") 0)
                              sx (or rx dx 0)
                              sy (or ry dy 0)
                              to (if (and align? (.-isDragging drag) (or (not= dx 0) (not= dy 0)))
                                   (into [] (geom/alignment-xf dx dy) rect))]
                          ($ :g.scene-object
                            {:ref (.-setNodeRef drag)
                             :transform (str "translate(" (+ ax sx) ", " (+ ay sy) ")")
                             :tab-index (if (and (not lock) seen) 0 -1)
                             :on-pointer-down (or handler stop-propagation)
                             :data-drag-remote (some? user)
                             :data-drag-local (.-isDragging drag)
                             :data-color (:user/color user)
                             :data-type (namespace (:object/type entity))
                             :data-id id}
                            (if outlines
                              (let [path (geom/object-tile-path entity sx sy)]
                                (if (and (seq path) (some? (deref portal)))
                                  (dom/create-portal
                                   ($ :polygon.scene-object-tiles
                                     {:points (transduce (partition-all 2) rf-points->poly path)})
                                   (deref portal)))))
                            ($ object
                              {:aligned-to to
                               :entity entity
                               :portal portal}))))))))))))
      ($ hooks/use-portal {:name :selected}
        (let [[ax ay bx by] (geom/bounding-rect (sequence boundsxf entities))]
          ($ drag-local-fn {:id "selected" :disabled (some dragging selected)}
            (fn [drag]
              (let [handler (getValueByKeys drag "listeners" "onPointerDown")
                    dx (or (getValueByKeys drag "transform" "x") 0)
                    dy (or (getValueByKeys drag "transform" "y") 0)]
                ($ :g.scene-objects.scene-objects-selected
                  {:ref (.-setNodeRef drag)
                   :transform (str "translate(" dx ", " dy ")")
                   :on-pointer-down (or handler stop-propagation)
                   :data-drag-local (.-isDragging drag)}
                  (if (> (count selected) 1)
                    ($ :rect.scene-objects-bounds
                      {:x (- ax 6) :y (- ay 6)
                       :width  (+ (- bx ax) (* 6 2))
                       :height (+ (- by ay) (* 6 2))
                       :rx 3 :ry 3}))
                  ($ TransitionGroup {:component nil}
                    (for [entity entities
                          :let [{id :db/id [ax ay] :object/point} entity
                                node (uix/create-ref)
                                user (dragging id)
                                rect (geom/object-bounding-rect entity)
                                rect (if (and align? (.-isDragging drag) (or (not= dx 0) (not= dy 0)))
                                       (into [] (geom/alignment-xf dx dy) rect))]]
                      ($ CSSTransition {:key id :nodeRef node :timeout 256}
                        ($ :g.scene-object-transition {:ref node}
                          (if (selected id)
                            ($ drag-remote-fn {:user (:user/uuid user) :x ax :y ay}
                              (fn [[rx ry]]
                                ($ :g.scene-object
                                  {:transform (str "translate(" (+ ax rx) ", " (+ ay ry) ")")
                                   :data-drag-remote (some? user)
                                   :data-drag-local (.-isDragging drag)
                                   :data-color (:user/color user)
                                   :data-id id}
                                  (if outlines
                                    (let [path (geom/object-tile-path entity (or rx dx 0) (or ry dy 0))]
                                      (if (and (seq path) (some? (deref portal)))
                                        (dom/create-portal
                                         ($ :polygon.scene-object-tiles
                                           {:points (transduce (partition-all 2) rf-points->poly path)})
                                         (deref portal)))))
                                  ($ object
                                    {:aligned-to rect
                                     :entity entity
                                     :portal portal})))))))))
                  (let [sz 400
                        tx (-> (+ ax bx) (* scale) (- sz) (/ 2) int)
                        ty (-> (+ by 24) (* scale) (- 24) int)
                        tf (str "scale(" (/ scale) ")")]
                    ($ :foreignObject.context-menu-object
                      {:x tx :y ty :width sz :height sz :transform tf}
                      ($ context-menu
                        {:data (sequence (filter (comp selected :db/id)) entities)
                         :type type}))))))))))))
