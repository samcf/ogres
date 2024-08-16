(ns ogres.app.component.scene-objects
  (:require [clojure.string :refer [join]]
            [goog.object :refer [getValueByKeys]]
            [ogres.app.component.scene-context-menu :refer [context-menu]]
            [ogres.app.component.scene-pattern :refer [pattern]]
            [ogres.app.const :refer [grid-size]]
            [ogres.app.geom :as geom]
            [ogres.app.hooks :refer [use-subscribe use-dispatch use-portal use-query]]
            [ogres.app.util :refer [round]]
            [react-transition-group :refer [TransitionGroup CSSTransition]]
            [uix.core :as uix :refer [defui $ create-ref use-callback use-effect use-state]]
            [uix.dom :as dom]
            ["@dnd-kit/core"
             :refer [useDndMonitor useDraggable]
             :rename {useDndMonitor use-dnd-monitor
                      useDraggable use-draggable}]))

(defn ^:private stop-propagation
  "Defines an event handler that ceases event propagation."
  [event]
  (.stopPropagation event))

(defn ^:private shapes-comparator
  "Defines a comparator function for shapes."
  [a b]
  (let [{[ax ay] :object/point} a
        {[bx by] :object/point} b]
    (compare [ax ay] [bx by])))

(defn ^:private tokens-comparator
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
  (comp (filter (comp seq :user/dragging))
        (mapcat (fn [user]
                  (map (juxt :db/id (constantly user))
                       (:user/dragging user))))))

(defn ^:private alignment-xf
  "Returns a transducer which expects a collection of points in the
   form of [Ax Ay Bx By ...] and aligns those points to the nearest
   grid intersection given a drag delta (dx dy) and the current
   grid offset given as (ox oy)."
  [dx dy ox oy]
  (comp (partition-all 2)
        (mapcat
         (fn [[x y]]
           [(+ (round (- (+ x dx) ox) grid-size) ox)
            (+ (round (- (+ y dy) oy) grid-size) oy)]))))

(defn ^:private tokens-xf
  "Defines a transducer which expects a collection of token entities
   and returns only the elements suitable for rendering given the
   user type."
  [user]
  (filter
   (fn [token]
     (or (= user :host)
         (not (contains? (:token/flags token) :hidden))))))

(defn ^:private use-cursor-point
  "Defines a React state hook which returns a point [Ax Ay] of the
   given user's current cursor position, if available."
  [uuid ox oy]
  (let [[point set-point] (use-state nil)]
    (use-effect
     (fn []
       (if (nil? uuid)
         (set-point nil))) [uuid])
    (use-subscribe :cursor/moved {:disabled (nil? uuid)}
      (use-callback
       (fn [{[id cx cy] :args}]
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

(defui ^:private shape-circle
  [{:keys [data]}]
  (let [{[ax ay] :shape/points} data]
    ($ :circle
      {:cx 0 :cy 0 :r (geom/chebyshev-distance 0 0 ax ay)})))

(defui ^:private shape-rect
  [{:keys [data]}]
  (let [{[ax ay] :shape/points} data]
    ($ :path
      {:d (join " " [\M 0 0 \H ax \V ay \H 0 \Z])})))

(defui ^:private shape-line
  [{:keys [data]}]
  (let [{[ax ay] :shape/points} data]
    ($ :line
      {:x1 0 :y1 0 :x2 ax :y2 ay :stroke-width 16 :stroke-linecap "round"})))

(defui ^:private shape-cone
  [{:keys [data]}]
  (let [{[bx by] :shape/points} data]
    ($ :polygon
      {:points (join " " (geom/cone-points 0 0 bx by))})))

(defui ^:private shape-poly
  [{:keys [data]}]
  (let [{points :shape/points} data]
    ($ :polygon
      {:points (join " " (into [0 0] points))})))

(defui ^:private shape [props]
  (case (keyword (name (:object/type (:data props))))
    :circle ($ shape-circle props)
    :rect   ($ shape-rect props)
    :line   ($ shape-line props)
    :cone   ($ shape-cone props)
    :poly   ($ shape-poly props)))

(defui ^:private object-shape [{:keys [entity]}]
  (let [{id :db/id
         color :shape/color
         pattern-name :shape/pattern
         opacity :shape/opacity
         [ax ay] :object/point} entity
        [bx by cx cy] (geom/object-bounding-rect entity)]
    ($ :g.scene-shape
      ($ :defs.scene-shape-defs
        ($ pattern
          {:id (str "shape-pattern-" id)
           :name pattern-name
           :color color}))
      ($ :rect.scene-shape-bounds
        {:x (- bx ax 6)
         :y (- by ay 6)
         :rx 3
         :ry 3
         :width (+ (- cx bx) (* 6 2))
         :height (+ (- cy by) (* 6 2))})
      ($ :g.scene-shape-path {:stroke color :fill (str "url(#shape-pattern-" id ")") :fill-opacity opacity}
        ($ shape {:data entity})))))

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

(defui ^:private object [props]
  (case (keyword (namespace (:object/type (:entity props))))
    :shape ($ object-shape props)
    :token ($ object-token props)))

(defn ^:private use-drag-listener []
  (let [dispatch (use-dispatch)]
    (use-dnd-monitor
     #js {"onDragStart"
          (use-callback
           (fn [data]
             (let [id (.. data -active -id)]
               (if (= id "selected")
                 (dispatch :drag/start-selected)
                 (dispatch :drag/start id)))) [dispatch])
          "onDragCancel"
          (use-callback
           (fn []
             (dispatch :drag/end)) [dispatch])
          "onDragEnd"
          (use-callback
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
        [[:scene/grid-origin :default [0 0]]
         [:scene/grid-align :default false]
         {:scene/tokens
          [:db/id
           [:object/type :default :token/token]
           [:object/point :default [0 0]]
           [:token/flags :default #{}]
           [:token/label :default ""]
           [:token/size :default 5]
           [:token/light :default 15]
           [:aura/radius :default 0]
           {:token/image [:image/checksum]}
           {:scene/_initiative [:db/id :initiative/turn]}]}
         {:scene/shapes
          [:db/id
           [:object/type :default :shape/circle]
           [:object/point :default [0 0]]
           [:shape/points :default [0 0]]
           [:shape/color :default "#f44336"]
           [:shape/opacity :default 0.25]
           [:shape/pattern :default :solid]]}]}]}]
    :root/session
    [{:session/conns
      [:user/uuid :user/color :user/dragging]}]}])

(defui objects []
  (let [result (use-query query [:db/ident :root])
        {{[_ _ bw bh] :bounds/self
          type :user/type
          {[cx cy]  :camera/point
           scale    :camera/scale
           selected :camera/selected
           {[ox oy] :scene/grid-origin
            align? :scene/grid-align
            tokens :scene/tokens
            shapes :scene/shapes}
           :camera/scene}
          :user/camera} :root/user
         {conns :session/conns} :root/session} result
        portal (uix/use-ref)
        bounds [cx cy (+ (/ bw scale) cx) (+ (/ bh scale) cy)]
        shapes (sort shapes-comparator shapes)
        tokens (sort tokens-comparator (sequence (tokens-xf type) tokens))
        entities (concat shapes tokens)
        selected (into #{} (map :db/id) selected)
        dragging (into {} user-drag-xf conns)]
    (use-drag-listener)
    ($ :g.scene-objects {}
      ($ :g.scene-objects-portal
        {:ref portal :tab-index -1})
      ($ TransitionGroup {:component nil}
        (for [entity entities
              :let [{id :db/id [ax ay] :object/point} entity
                    node (create-ref)
                    user (dragging id)
                    rect (geom/object-bounding-rect entity)
                    seen (geom/rect-intersects-rect rect bounds)]]
          ($ CSSTransition {:key id :nodeRef node :timeout 256}
            ($ :g.scene-object-transition {:ref node}
              (if (not (selected id))
                ($ drag-remote-fn {:user (:user/uuid user) :x ax :y ay}
                  (fn [[rx ry]]
                    ($ drag-local-fn {:id id :disabled (some? user)}
                      (fn [drag]
                        (let [handler (getValueByKeys drag "listeners" "onPointerDown")
                              dx (or (getValueByKeys drag "transform" "x") 0)
                              dy (or (getValueByKeys drag "transform" "y") 0)
                              tx (+ ax (or rx dx 0))
                              ty (+ ay (or ry dy 0))
                              to (if (and align? (.-isDragging drag) (or (not= dx 0) (not= dy 0)))
                                   (into [] (alignment-xf dx dy ox oy) rect))]
                          ($ :g.scene-object
                            {:ref (.-setNodeRef drag)
                             :transform (str "translate(" tx ", " ty ")")
                             :tab-index (if seen 0 -1)
                             :on-pointer-down (or handler stop-propagation)
                             :data-drag-remote (some? user)
                             :data-drag-local (.-isDragging drag)
                             :data-color (:user/color user)
                             :data-id id}
                            ($ object
                              {:entity entity
                               :portal portal
                               :aligned-to to}))))))))))))
      ($ use-portal {:name :selected}
        (let [[ax ay bx by] (-> (comp (filter (comp selected :db/id))
                                      (mapcat geom/object-bounding-rect))
                                (sequence entities)
                                (geom/bounding-rect))]
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
                                node (create-ref)
                                user (dragging id)
                                rect (geom/object-bounding-rect entity)
                                rect (if (and align? (.-isDragging drag) (or (not= dx 0) (not= dy 0)))
                                       (into [] (alignment-xf dx dy ox oy) rect))]]
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
                                  ($ object
                                    {:entity entity
                                     :portal portal
                                     :aligned-to rect})))))))))
                  (let [sz 400
                        tx (-> (+ ax bx) (* scale) (- sz) (/ 2) int)
                        ty (-> (+ by 24) (* scale) (- 24) int)
                        tf (str "scale(" (/ scale) ")")]
                    ($ :foreignObject.context-menu-object
                      {:x tx :y ty :width sz :height sz :transform tf}
                      ($ context-menu
                        {:data (sequence (filter (comp selected :db/id)) entities)
                         :type type}))))))))))))
