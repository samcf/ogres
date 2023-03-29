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

(def ^:private query-footer
  [{:root/local [:local/type]}
   {:root/stamps [:image/checksum]}])

(def ^:private query-form
  [{:root/stamps [:image/checksum :image/scope]}
   {:root/local
    [[:local/type :default :conn]
     [:bounds/self :default [0 0 0 0]]
     {:local/window
      [[:window/scale :default 1]
       [:window/vec :default [0 0]]]}]}])

(defn- image [checksum render-fn]
  (render-fn (use-image checksum)))

(defn- draggable [id render-fn]
  (render-fn (use-draggable #js {"id" id})))

(defn- token [checksum render-fn]
  (render-fn
   (use-image checksum)
   (use-draggable #js {"id" checksum})))

(defn- drag-handler [props _]
  (let [{:keys [on-create on-remove]
         :or   {on-create identity
                on-remove identity}} props
        delete (uix/state false)
        active (uix/state nil)]
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
     (.querySelector js/document "#root")]))

(defn- tokens [props _]
  (let [option (use-droppable #js {"id" (random-uuid)})]
    [:<>
     (for [[idx data] (sequence (map-indexed vector) (:data props))]
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
      {:ref (.-setNodeRef option)}
      [icon {:name "trash3-fill" :size 26}]]]))

(defn- paginated [props _]
  (let [{:keys [data limit] :or {data [] limit 10}} props
        page  (uix/state 1)
        limit (dec limit)
        pages (-> (count data) (/ limit) (js/Math.ceil))
        start (-> (min @page pages) (dec) (* limit) (max 0))
        stop  (-> (+ start limit) (min (count data)))
        data  (->> (repeat :placeholder)
                   (concat (subvec data start stop))
                   (take limit))]
    [:<>
     [tokens {:data data}]
     [pagination
      {:pages (max pages 1)
       :value (max (min pages @page) 1)
       :on-change (partial reset! page)}]]))

(defn- xf-scope [scope]
  (filter (fn [entity] (= (:image/scope entity) scope))))

(defn- form []
  (let [dispatch  (use-dispatch)
        result    (use-query query-form [:db/ident :root])
        {data :root/stamps
         {type :local/type} :root/local} result
        data-pub  (into [:default] (xf-scope :public) data)
        data-prv  (into [] (xf-scope :private) data)
        on-scope  (uix/callback (fn []) [])
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
    [:<>
     (if (= type :host)
       [:<>
        [:header "Public [" (count data-pub) "]"]
        [:section.tokens.host.public
         [paginated {:data data-pub :limit 10}]]
        [:header "Private [" (count data-prv) "]"]
        [:section.tokens.host.private
         [paginated {:data data-prv :limit 20}]]]
       [:section.tokens.conn.public
        [paginated {:data data-pub :limit 30}]])
     [drag-handler
      {:on-create on-create
       :on-remove on-remove
       :on-scope  on-scope}]]))

(defn- footer []
  (let [dispatch (use-dispatch)
        result   (use-query query-footer [:db/ident :root])
        {{type :local/type} :root/local
         images :root/stamps} result
        images   (sequence (map :image/checksum) images)
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
       :disabled (or (= type :conn) (empty? images))
       :on-click #(dispatch :stamp/remove-all images)}
      [icon {:name "trash3-fill" :size 16}]]]))

(defmethod render/form :tokens [] form)
(defmethod render/footer :tokens [] footer)
