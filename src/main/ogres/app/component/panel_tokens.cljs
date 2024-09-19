(ns ogres.app.component.panel-tokens
  (:require [goog.object :as object :refer [getValueByKeys]]
            [ogres.app.component :refer [icon image pagination modal-fullscreen]]
            [ogres.app.hooks :refer [use-publish use-dispatch use-image use-image-uploader use-query use-shortcut]]
            [ogres.app.util :refer [separate comp-fn]]
            [uix.core :as uix :refer [defui $ use-callback use-ref use-state use-effect use-memo]]
            [uix.dom :refer [create-portal]]
            ["@dnd-kit/core"
             :refer  [DndContext DragOverlay useDndMonitor useDraggable useDroppable]
             :rename {DndContext    dnd-context
                      DragOverlay   drag-overlay
                      useDndMonitor use-dnd-monitor
                      useDraggable  use-draggable
                      useDroppable  use-droppable}]))

(def ^:private query-footer
  [{:root/user [:user/type]}
   {:root/token-images
    [:image/hash
     {:image/thumbnail
      [:image/hash]}]}])

(def ^:private query-form
  [{:root/user [:user/type]}
   {:root/token-images
    [:image/hash
     :image/name
     :image/scope
     {:image/thumbnail
      [:image/hash]}]}])

(def ^:private query-bounds
  [[:bounds/self :default [0 0 0 0]]])

(defui ^:private draggable
  [{:keys [id children]}]
  (let [opt (use-draggable #js {"id" id "data" #js {"image" "default"}})]
    (children {:options opt})))

(defui ^:private token
  [{:keys [id hash children]}]
  (let [url (use-image hash)
        opt (use-draggable #js {"id" id "data" #js {"image" hash}})]
    (children {:url url :options opt})))

(defui ^:private overlay []
  (let [[active set-active] (use-state nil)]
    (use-dnd-monitor
     #js {"onDragStart" (fn [event] (set-active (.. event -active -data -current -image)))
          "onDragEnd"   (fn [_]     (set-active nil))})
    (create-portal
     ($ drag-overlay {:drop-animation nil}
       (if (= active "default")
         ($ :.token-gallery-item
           {:data-type "default"}
           ($ icon {:name "dnd"}))
         (if (some? active)
           ($ image {:hash active}
             (fn [url]
               ($ :.token-gallery-item {:data-type "image" :style {:background-image (str "url(" url ")")}}))))))
     (.querySelector js/document "#root"))))

(defui ^:private gallery [props]
  (let [option (use-droppable #js {"id" "trash"})]
    ($ :<>
      (for [[idx data] (sequence (map-indexed vector) (:data props))]
        (cond (map? data)
              (let [hash (:image/hash data)
                    thmb (:image/hash (:image/thumbnail data))]
                ($ token {:key hash :id hash :hash thmb}
                  (fn [{:keys [url ^js/object options]}]
                    ($ :button.token-gallery-item
                      {:ref (.-setNodeRef options)
                       :data-type "image"
                       :style {:background-image (str "url(" url ")")}
                       :on-key-down (.. options -listeners -onKeyDown)
                       :on-pointer-down (.. options -listeners -onPointerDown)
                       :aria-label (:image/name data)
                       :aria-pressed (getValueByKeys options "attributes" "aria-pressed")
                       :aria-disabled (getValueByKeys options "attributes" "aria-disabled")
                       :aria-describedby (getValueByKeys options "attributes" "aria-describedby")
                       :aria-roledescription (getValueByKeys options "attributes" "aria-roledescription")}))))
              (= data :default)
              ($ draggable {:key idx :id "default"}
                (fn [{:keys [^js/object options]}]
                  ($ :button.token-gallery-item
                    {:ref (.-setNodeRef options)
                     :data-type "default"
                     :on-key-down (.. options -listeners -onKeyDown)
                     :on-pointer-down (.. options -listeners -onPointerDown)
                     :aria-label "default"
                     :aria-pressed (getValueByKeys options "attributes" "aria-pressed")
                     :aria-disabled (getValueByKeys options "attributes" "aria-disabled")
                     :aria-describedby (getValueByKeys options "attributes" "aria-describedby")
                     :aria-roledescription (getValueByKeys options "attributes" "aria-roledescription")}
                    ($ icon {:name "dnd"}))))
              (= data :placeholder)
              ($ :.token-gallery-item {:key idx :data-type "placeholder"})))
      ($ :.token-gallery-item
        {:ref (.-setNodeRef option) :data-type "trash"}
        ($ icon {:name "trash3-fill" :size 26})))))

(defui ^:private paginated
  [{:keys [name data limit]
    :or   {data [] limit 10}}]
  (let [[page set-page] (use-state 1)
        limit (dec limit)
        pages (-> (count data) (/ limit) (js/Math.ceil))
        start (-> (min page pages) (dec) (* limit) (max 0))
        stop  (-> (+ start limit) (min (count data)))
        data  (->> (repeat :placeholder)
                   (concat (subvec data start stop))
                   (take limit))]
    ($ :<>
      ($ gallery {:data data})
      (if (> pages 1)
        ($ pagination
          {:name  name
           :pages (max pages 1)
           :value (max (min pages page) 1)
           :on-change set-page})))))

(defui tokens []
  (let [result (use-query query-form [:db/ident :root])
        {data :root/token-images
         {type :user/type} :root/user} result
        [pub prv] (separate (comp-fn = :image/scope :public) data)
        data-pub  (into [:default] (reverse pub))
        data-prv  (vec (reverse prv))
        drop-pub  (use-droppable #js {"id" "scope-pub"})
        drop-prv  (use-droppable #js {"id" "scope-prv"})]
    ($ :.form-tokens
      (if (= type :host)
        ($ :<>
          ($ :header ($ :h2 "Tokens"))
          ($ :fieldset.fieldset.token-gallery
            {:ref (.-setNodeRef drop-pub) :data-type "host" :data-scope "public"}
            ($ :legend "Public")
            ($ paginated {:name "tokens-public" :data data-pub :limit 10}))
          ($ :fieldset.fieldset.token-gallery
            {:ref (.-setNodeRef drop-prv) :data-type "host" :data-scope "private"}
            ($ :legend "Private")
            ($ paginated {:name "tokens-private" :data data-prv :limit 20}))
          ($ :.form-notice
            "Upload images from your computer and pull them onto the scene as
             tokens. Moving tokens to the public section will make them available
             for other players in an online game."))
        ($ :<>
          ($ :header ($ :h2 "Tokens"))
          ($ :fieldset.fieldset.token-gallery
            {:ref (.-setNodeRef drop-pub) :data-type "conn" :data-scope "public"}
            ($ :legend "Tokens")
            ($ paginated {:name "tokens-public" :data data-pub :limit 30})))))))

(defui form []
  (let [dispatch (use-dispatch)
        results  (use-query query-bounds [:db/ident :user])
        {[bx by bw bh] :bounds/self} results
        on-create
        (use-callback
         (fn [hash element delta]
           (let [rect (.getBoundingClientRect element)
                 tw (.-width rect)
                 th (.-height rect)
                 tx (.-x rect)
                 ty (.-y rect)
                 dx (.-x delta)
                 dy (.-y delta)
                 mx (- (+ tx dx (/ tw 2)) bx)
                 my (- (+ ty dy (/ th 2)) by)]
             (if (and (<= bx mx (+ bx bw)) (<= by my (+ by bh)))
               (dispatch :token/create mx my (if (not= hash "default") hash)))))
         [dispatch bx by bw bh])]
    ($ dnd-context
      #js {"onDragEnd"
           (use-callback
            (fn [event]
              (let [drag (getValueByKeys event #js ["active" "id"])
                    drop (getValueByKeys event #js ["over" "id"])
                    nail (getValueByKeys event #js ["active" "data" "current" "image"])]
                (if (and (some? drop) (not= drag "default"))
                  (case drop
                    "scope-pub" (dispatch :token-images/change-scope drag :public)
                    "scope-prv" (dispatch :token-images/change-scope drag :private)
                    "trash" (dispatch :token-images/remove drag nail))
                  (let [target (.. event -activatorEvent -target)
                        delta  (.-delta event)]
                    (on-create drag target delta)))))
            [dispatch on-create])}
      ($ tokens)
      ($ overlay))))

(defn ^:private object-fit-scale
  "Returns the amount the image given by its natural dimensions {Aw Ah} has
   been scaled to fit into the container given by {Bw Bh} as a number in
   the domain (0, 1]; 1 being that the image dimensions have not been
   scaled at all."
  [aw ah bw bh]
  (min (/ bh ah) (/ bw aw) 1))

(defn ^:private default-region [x y]
  (if (<= x y)
    [0 0 x x]
    [(/ (- x y) 2) 0
     (/ (+ x y) 2) y]))

(defn ^:private dnd-scale-fn
  "Returns a modifier function which scales the transform delta to
   the given ratio."
  [scale]
  (fn [data]
    (let [dx (.. data -transform -x)
          dy (.. data -transform -y)]
      (js/Object.assign
       #js {} (.-transform data)
       #js {"x" (/ dx scale)
            "y" (/ dy scale)}))))

(defn ^:private dnd-integer-fn
  "Returns a new transform delta whose terms are coerced to integers."
  [data]
  (let [dx (.. data -transform -x)
        dy (.. data -transform -y)]
    (js/Object.assign
     #js {} (.-transform data)
     #js {"x" (int dx)
          "y" (int dy)})))

(defn ^:private dnd-clamp-fn
  "Returns a modifier function which clamps the transform delta to
   the dimensions of the image given as {Wd Ht}."
  [[ax ay bx by] wd ht]
  (fn [data]
    (if (some? (.-active data))
      (let [id (keyword (.. data -active -id))
            dx (.. data -transform -x)
            dy (.. data -transform -y)]
        (js/Object.assign
         #js {} (.-transform data)
         #js {"x" (case id
                    :rg (max (- ax)           (min dx (- wd bx)))
                    :nw (max (- ay) (- ax)    (min dx (- bx ax)))
                    :ne (max (- ax bx)        (min dx (- wd bx) ay))
                    :se (max (- ax bx)        (min dx (- wd bx) (- ht by)))
                    :sw (max (- by ht) (- ax) (min dx (- bx ax))))
              "y" (case id
                    :rg (max (- ay)           (min dy (- ht by)))
                    :nw (max (- ay) (- ax)    (min dy (- by ay)))
                    :ne (max (- ay)           (min dy (- by ay)))
                    :se (max (- ay by)        (min dy (- ht by) (- wd bx)))
                    :sw (max (- ay by)        (min dy (- ht by))))}))
      (.-transform data))))

(defui ^:private anchor [{:keys [id]}]
  (let [drag (use-draggable #js {"id" (name id)})]
    ($ :.token-editor-region-anchor
      {:ref (.-setNodeRef drag)
       :class (str "token-editor-region-anchor-" (name id))
       :on-pointer-down (.. drag -listeners -onPointerDown)})))

(defui ^:private region [{:keys [children x y width height]}]
  (let [drag (use-draggable #js {"id" "rg"})]
    ($ :.token-editor-region
      ($ :.token-editor-region-drag
        {:ref (.-setNodeRef drag)
         :style {:left x :top y :width width :height height}
         :on-pointer-down (.. drag -listeners -onPointerDown)}
        children))))

(defui ^:private editor [props]
  (let [{{hash   :image/hash
          width  :image/width
          height :image/height} :image} props
        publish (use-publish)
        element (use-ref nil)
        obj-url (use-image hash)
        initial (or (:image/thumbnail-rect (:image props)) (default-region width height))
        [delta set-delta] (use-state [0 0 0 0])
        [bound set-bound] (use-state initial)
        [scale set-scale] (use-state nil)
        scale-fn (use-memo #(dnd-scale-fn scale) [scale])
        clamp-fn (use-memo #(dnd-clamp-fn bound width height) [bound width height])
        on-change-thumbnail
        (use-callback
         (fn [event]
           (.preventDefault event)
           (publish {:topic :image/change-thumbnail :args [hash bound]})) [publish hash bound])
        on-drag-move
        (use-callback
         (fn [data]
           (let [id (.. data -active -id)
                 dx (.. data -delta -x)
                 dy (.. data -delta -y)
                 si (js/Math.sign dx)
                 mx (abs dx)]
             (set-delta
              (fn [[ax ay bx by]]
                (case (keyword id)
                  :rg [dx dy dx dy]
                  :nw [(min dx dy) (min dx dy) bx by]
                  :ne [ax (- (* si mx)) (* si mx) by]
                  :se [ax ay (max dx dy) (max dx dy)]
                  :sw [(* si mx) ay bx (- (* si mx))]))))) [])
        on-drag-stop
        (use-callback
         (fn [data]
           (let [id (.. data -active -id)
                 dx (.. data -delta -x)
                 dy (.. data -delta -y)
                 si (js/Math.sign dx)
                 mx (abs dx)]
             (set-delta [0 0 0 0])
             (set-bound
              (fn [[ax ay bx by]]
                (case (keyword id)
                  :rg [(+ ax dx) (+ ay dy) (+ bx dx) (+ by dy)]
                  :nw [(+ ax (min dx dy)) (+ ay (min dx dy)) bx by]
                  :ne [ax (+ ay (- (* si mx))) (+ bx (* si mx)) by]
                  :se [ax ay (+ bx (max dx dy)) (+ by (max dx dy))]
                  :sw [(+ ax (* si mx)) ay bx (+ by (- (* si mx)))]))))) [])
        on-resize-image
        (use-callback
         (fn [[entry]]
           (let [con-rect (.-contentRect entry)
                 con-wdth (js/Math.round (.-width con-rect))
                 con-hght (js/Math.round (.-height con-rect))]
             (set-scale (object-fit-scale width height con-wdth con-hght)))) [width height])]

    ;; Observe changes to the size of the token image in order to recalculate
    ;; the scale of the image relative to its natural dimensions.
    (use-effect
     (fn []
       (let [resizes (js/ResizeObserver. on-resize-image)]
         (if-let [node (deref element)] (.observe resizes node))
         (fn [] (.disconnect resizes)))) [on-resize-image])

    ($ dnd-context
      #js {"modifiers"  #js [scale-fn dnd-integer-fn clamp-fn]
           "onDragMove" on-drag-move
           "onDragEnd"  on-drag-stop}
      (let [[ax ay bx by] bound
            [cx cy dx dy] delta]
        ($ :form
          {:on-submit on-change-thumbnail}
          ($ :img.token-editor-image
            {:ref element
             :src obj-url
             :width width
             :height height
             :data-loaded (some? obj-url)})
          ($ region
            {:x      (* (- (+ ax cx) (/ width 2)) scale)
             :y      (* (- (+ ay cy) (/ height 2)) scale)
             :width  (* (+ (- bx ax cx) dx) scale)
             :height (* (+ (- by ay cy) dy) scale)}
            ($ anchor {:key :nw :id :nw})
            ($ anchor {:key :ne :id :ne})
            ($ anchor {:key :se :id :se})
            ($ anchor {:key :sw :id :sw}))
          ($ :button.token-editor-button
            {:type "submit" :disabled (= initial bound)}
            ($ icon {:name "crop" :size 16})
            "Crop and Resize"))))))

(def ^:private modal-query
  [{:root/token-images
    [:image/hash
     :image/name
     :image/size
     :image/width
     :image/height
     :image/thumbnail-rect
     {:image/thumbnail
      [:image/hash :image/size]}]}])

(defui ^:private modal [props]
  (let [result (use-query modal-query [:db/ident :root])
        [selected set-selected] (use-state nil)
        [page set-page] (use-state 1)
        data  (vec (reverse (:root/token-images result)))
        limit 20
        pages (js/Math.ceil (/ (count data) limit))
        start (max (* (dec (min page pages)) limit) 0)
        end   (min (+ start limit) (count data))
        part  (subvec data start end)]
    (use-shortcut ["Escape"]
      (:on-close props))
    ($ modal-fullscreen
      ($ :.token-editor
        (if-let [entity (first (filter (comp #{selected} :image/hash) data))]
          ($ :.token-editor-workspace
            ($ editor {:key (:image/hash entity) :image entity}))
          ($ :.token-editor-placeholder
            ($ icon {:name "crop" :size 64})
            "Select an image to crop and resize." ($ :br)
            "The original image will always be preserved."))
        ($ :.token-editor-gallery
          ($ :.token-editor-gallery-paginated
            ($ :.token-editor-gallery-thumbnails
              (for [{{hash :image/hash} :image/thumbnail key :image/hash} part]
                ($ image {:key key :hash hash}
                  (fn [url]
                    ($ :label.token-editor-gallery-thumbnail
                      {:style {:background-image (str "url(" url ")")}}
                      ($ :input
                        {:type "radio"
                         :name "token-editor-image"
                         :checked (= selected key)
                         :value key
                         :on-change
                         (fn []
                           (set-selected key))}))))))
            ($ pagination
              {:name "token-editor-gallery"
               :pages pages
               :value page
               :on-change set-page
               :class-name "dark"}))
          ($ :button.token-editor-button
            {:on-click (:on-close props)}
            "Exit"))))))

(defui footer []
  (let [[editing set-editing] (use-state false)
        dispatch (use-dispatch)
        result   (use-query query-footer [:db/ident :root])
        {{type :user/type} :root/user
         images :root/token-images} result
        upload   (use-image-uploader {:type :token})
        input    (use-ref)]
    ($ :<>
      (if editing
        (let [node (js/document.querySelector "#root")]
          (create-portal
           ($ modal {:on-close #(set-editing false)}) node)))
      ($ :button.button.button-neutral
        {:type     "button"
         :title    "Upload token image"
         :on-click #(.click (deref input))}
        ($ :input
          {:type "file" :hidden true :accept "image/*" :multiple true :ref input
           :on-change
           (fn [event]
             (upload (.. event -target -files))
             (set! (.. event -target -value) ""))})
        ($ icon {:name "camera-fill" :size 16}) "Upload images")
      ($ :button.button.button-neutral
        {:type "button"
         :title "Crop"
         :disabled (or (not= type :host) (not (seq images)))
         :on-click (partial set-editing not)}
        ($ icon {:name "crop" :size 18})
        "Edit images")
      ($ :button.button.button-danger
        {:type "button"
         :title "Remove all tokens"
         :aria-label "Remove all tokens"
         :disabled (or (= type :conn) (not (seq images)))
         :on-click
         (fn []
           (let [xf (mapcat (juxt :image/hash (comp :image/hash :image/thumbnail)))]
             (dispatch :token-images/remove-all (into #{} xf images))))}
        ($ icon {:name "trash3-fill" :size 16})))))
