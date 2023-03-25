(ns ogres.app.form.tokens
  (:require [ogres.app.form.render :as render]
            [ogres.app.hooks :refer [use-dispatch use-image use-image-uploader use-query]]
            [ogres.app.render :refer [icon pagination]]
            [uix.core.alpha :as uix]
            [uix.dom.alpha :refer [create-portal]]
            ["@dnd-kit/core"
             :refer  [DragOverlay useDndMonitor useDraggable useDroppable]
             :rename {DragOverlay   drag-overlay
                      useDndMonitor use-dnd-monitor
                      useDraggable  use-draggable
                      useDroppable  use-droppable}]))

(def ^:private per-page 19)

(def ^:private header-query
  [{:root/stamps [:image/checksum]}])

(def ^:private query
  [{:root/stamps [:image/checksum]}
   {:root/local
    [[:bounds/self :default [0 0 0 0]]
     {:local/window
      [[:window/scale :default 1]
       [:window/vec :default [0 0]]]}]}])

(defn- image [checksum render-fn]
  (render-fn (use-image checksum)))

(defn- token [checksum render-fn]
  (render-fn
   (use-image checksum)
   (use-draggable #js {"id" checksum})))

(defn- draggable [id render-fn]
  (render-fn (use-draggable #js {"id" id})))

(defn- tokens [props _]
  (let [{:keys [data on-create on-remove]
         :or   {on-create identity
                on-remove identity}} props
        active (uix/state nil)
        delete (uix/state false)
        option (use-droppable #js {"id" "tokens-trash"})]
    (use-dnd-monitor
     #js {"onDragStart"
          (fn [event]
            (reset! active (.. event -active -id)))
          "onDragMove"
          (fn [event]
            (if (and (.-over event) (not= (.-id (.-active event)) "default"))
              (reset! delete true)
              (reset! delete false)))
          "onDragEnd"
          (fn [event]
            (if (and (.-over event) (not= (.-id (.-active event)) "default"))
              (do (on-remove @active)
                  (reset! active nil)
                  (reset! delete nil))
              (let [target (.. event -activatorEvent -target)
                    delta  (.-delta event)]
                (on-create @active target delta)
                (reset! active nil)
                (reset! delete nil))))})
    [:<>
     (for [[idx data] (sequence (map-indexed vector) data)]
       (cond (map? data)
             (let [checksum (:image/checksum data)]
               ^{:key checksum}
               [token checksum
                (fn [url ^js/object options]
                  [:figure.tokens-template
                   {:ref   (.-setNodeRef options)
                    :style {:background-image (str "url(" url ")")}
                    :on-pointer-down (.. options -listeners -onPointerDown)
                    :on-key-down     (.. options -listeners -onKeyDown)}])])

             (= data :default)
             ^{:key idx}
             [draggable "default"
              (fn [^js/object options]
                [:figure.tokens-default
                 {:ref (.-setNodeRef options)
                  :on-pointer-down (.. options -listeners -onPointerDown)
                  :on-key-down     (.. options -listeners -onKeyDown)}
                 [icon {:name "dnd"}]])]

             (= data :placeholder)
             [:figure.tokens-placeholder
              {:key idx}]))
     [:figure.tokens-trashcan
      {:ref (.-setNodeRef option)
       :css {:is-deleting @delete}}
      [icon {:name "trash3-fill" :size 26}]]
     [create-portal
      [:> drag-overlay {:drop-animation nil}
       (if (= (deref active) "default")
         [:figure.tokens-default
          [icon {:name "dnd"}]]
         (if (string? (deref active))
           [image (deref active)
            (fn [url]
              [:figure.tokens-template
               {:css   {:is-deleting @delete}
                :style {:background-image (str "url(" url ")")}}])]))]
      (.-body js/document)]]))

(defn- form []
  (let [dispatch  (use-dispatch)
        result    (use-query query [:db/ident :root])
        data      (into [:default] (vec (:root/stamps result)))
        page      (uix/state 1)
        pages     (int (js/Math.ceil (/ (count data) per-page)))
        start     (-> (min @page pages) (dec) (* per-page) (max 0))
        end       (-> (+ start per-page) (min (count data)))
        on-remove (uix/callback (partial dispatch :stamp/remove) [])
        on-create (uix/callback
                   (fn [checksum element delta]
                     (let [{{[bx by bw bh] :bounds/self
                             {[cx cy] :window/vec
                              scale :window/scale} :local/window} :root/local} result
                           rect    (.getBoundingClientRect element)
                           [tw th] [(.-width rect) (.-height rect)]
                           [tx ty] [(.-x rect) (.-y rect)]
                           [dx dy] [(.-x delta) (.-y delta)]
                           [mx my] [(- (+ tx dx (/ tw 2)) bx) (- (+ ty dy (/ th 2)) by)]
                           [sx sy] [(- (* mx (/ scale)) cx) (- (* my (/ scale)) cy)]]
                       (if (and (<= bx mx (+ bx bw)) (<= by my (+ by bh)))
                         (dispatch :token/create sx sy checksum)))) [result])]
    [:section.tokens
     [tokens
      {:data (->> (repeat :placeholder) (concat (subvec data start end)) (take per-page))
       :on-create on-create
       :on-remove on-remove}]
     [pagination
      {:pages (max pages 1)
       :value (max @page 1)
       :on-change (partial reset! page)}]]))

(defn- footer []
  (let [dispatch (use-dispatch)
        result   (use-query header-query [:db/ident :root])
        stamps   (sequence (map :image/checksum) (:root/stamps result))
        upload   (use-image-uploader {:type :token})
        input    (uix/ref)]
    [:<>
     [:button.button.button-neutral
      {:type     "button"
       :title    "Upload token image"
       :on-click #(.click (deref input))}
      [:input
       {:type "file" :hidden true :accept "image/*" :multiple true :ref input
        :on-change
        (fn [event]
          (doseq [file (.. event -target -files)]
            (upload file))
          (set! (.. event -target -value) ""))}]
      [icon {:name "camera-fill" :size 16}] "Select Files"]
     [:button.button.button-danger
      {:type     "button"
       :title    "Remove all"
       :disabled (empty? stamps)
       :on-click #(dispatch :stamp/remove-all stamps)}
      [icon {:name "trash3-fill" :size 16}]]]))

(defmethod render/form :tokens [] form)
(defmethod render/footer :tokens [] footer)
