(ns ogres.app.form.tokens
  (:require [ogres.app.hooks :refer [use-dispatch use-image use-image-uploader use-query]]
            [ogres.app.render :refer [css icon pagination]]
            [uix.core :as uix :refer [defui $ use-callback use-ref use-state]]
            [uix.dom :refer [create-portal]]
            ["@dnd-kit/core"
             :refer  [DragOverlay useDndMonitor useDraggable useDroppable]
             :rename {DragOverlay   drag-overlay
                      useDndMonitor use-dnd-monitor
                      useDraggable  use-draggable
                      useDroppable  use-droppable}]))

(def ^:private query-footer
  [{:root/local [:local/type]}
   {:root/token-images [:image/checksum]}])

(def ^:private query-form
  [{:root/token-images [:image/checksum :image/scope]}
   {:root/local
    [[:local/type :default :conn]
     [:bounds/self :default [0 0 0 0]]
     {:local/window
      [[:window/scale :default 1]
       [:window/point :default [0 0]]]}]}])

(defui image [{:keys [checksum children]}]
  (let [data-url (use-image checksum)]
    (children {:data-url data-url})))

(defui draggable [{:keys [id children]}]
  (let [options (use-draggable #js {"id" id})]
    (children {:options options})))

(defui token [{:keys [checksum children]}]
  (let [data-url (use-image checksum)
        options  (use-draggable #js {"id" checksum})]
    (children {:data-url data-url :options options})))

(defui drag-handler [props]
  (let [{:keys [on-create on-remove]
         :or   {on-create identity
                on-remove identity}} props
        [delete set-delete] (use-state false)
        [active set-active] (use-state nil)]
    (use-dnd-monitor
     #js {"onDragStart"
          (fn [event]
            (set-active (.. event -active -id)))
          "onDragMove"
          (fn [event]
            (if (and (.-over event) (not= (.-id (.-active event)) "default"))
              (set-delete true)
              (set-delete false)))
          "onDragEnd"
          (fn [event]
            (if (and (.-over event) (not= (.-id (.-active event)) "default"))
              (do (on-remove active)
                  (set-active nil)
                  (set-delete false))
              (let [target (.. event -activatorEvent -target)
                    delta  (.-delta event)]
                (on-create active target delta)
                (set-active nil)
                (set-delete false))))})
    (create-portal
     ($ drag-overlay {:drop-animation nil}
       (cond (= active "default")
             ($ :figure.tokens-default ($ icon {:name "dnd"}))
             (string? active)
             ($ image {:checksum active}
               (fn [{:keys [data-url]}]
                 ($ :figure.tokens-template
                   {:class (css {:is-deleting delete})
                    :style {:background-image (str "url(" data-url ")")}})))))
     (.querySelector js/document "#root"))))

(defui tokens [props]
  (let [option (use-droppable #js {"id" "trash"})]
    ($ :<>
      (for [[idx data] (sequence (map-indexed vector) (:data props))]
        (cond (map? data)
              (let [checksum (:image/checksum data)]
                ($ token {:key checksum :checksum checksum}
                  (fn [{:keys [data-url ^js/object options]}]
                    ($ :figure.tokens-template
                      {:ref   (.-setNodeRef options)
                       :style {:background-image (str "url(" data-url ")")}
                       :on-pointer-down (.. options -listeners -onPointerDown)
                       :on-key-down     (.. options -listeners -onKeyDown)}))))
              (= data :default)
              ($ draggable {:key idx :id "default"}
                (fn [{:keys [^js/object options]}]
                  ($ :figure.tokens-default
                    {:ref (.-setNodeRef options)
                     :on-pointer-down (.. options -listeners -onPointerDown)
                     :on-key-down     (.. options -listeners -onKeyDown)}
                    ($ icon {:name "dnd"}))))
              (= data :placeholder)
              ($ :figure.tokens-placeholder {:key idx})))
      ($ :figure.tokens-trashcan
        {:ref (.-setNodeRef option)}
        ($ icon {:name "trash3-fill" :size 26})))))

(defui paginated [props]
  (let [{:keys [data limit] :or {data [] limit 10}} props
        [page set-page] (use-state 1)
        limit (dec limit)
        pages (-> (count data) (/ limit) (js/Math.ceil))
        start (-> (min page pages) (dec) (* limit) (max 0))
        stop  (-> (+ start limit) (min (count data)))
        data  (->> (repeat :placeholder)
                   (concat (subvec data start stop))
                   (take limit))]
    ($ :<>
      ($ tokens {:data data})
      ($ pagination
        {:pages (max pages 1)
         :value (max (min pages page) 1)
         :on-change set-page}))))

(defn- xf-scope [scope]
  (filter (fn [entity] (= (:image/scope entity) scope))))

(defui form []
  (let [dispatch  (use-dispatch)
        result    (use-query query-form [:db/ident :root])
        {data :root/token-images
         {type :local/type} :root/local} result
        data-pub  (into [:default] (xf-scope :public) data)
        data-prv  (into [] (xf-scope :private) data)
        on-scope  (use-callback (fn []) [])
        on-remove (use-callback (fn [key] (dispatch :tokens/remove key)) [dispatch])
        on-create (use-callback
                   (fn [checksum element delta]
                     (let [{{[bx by bw bh] :bounds/self
                             {[cx cy] :window/point
                              scale :window/scale} :local/window} :root/local} result
                           rect    (.getBoundingClientRect element)
                           [tw th] [(.-width rect) (.-height rect)]
                           [tx ty] [(.-x rect) (.-y rect)]
                           [dx dy] [(.-x delta) (.-y delta)]
                           [mx my] [(- (+ tx dx (/ tw 2)) bx) (- (+ ty dy (/ th 2)) by)]
                           [sx sy] [(- (* mx (/ scale)) cx) (- (* my (/ scale)) cy)]]
                       (if (and (<= bx mx (+ bx bw)) (<= by my (+ by bh)))
                         (dispatch :token/create sx sy checksum)))) [dispatch result])]
    ($ :<>
      (if (= type :host)
        ($ :<>
          ($ :header "Public [" (count data-pub) "]")
          ($ :section.tokens.host.public
            ($ paginated {:data data-pub :limit 10}))
          ($ :header "Private [" (count data-prv) "]")
          ($ :section.tokens.host.private
            ($ paginated {:data data-prv :limit 20})))
        ($ :section.tokens.conn.public
          ($ paginated {:data data-pub :limit 30})))
      ($ drag-handler
        {:on-create on-create
         :on-remove on-remove
         :on-scope  on-scope}))))

(defui footer []
  (let [dispatch (use-dispatch)
        result   (use-query query-footer [:db/ident :root])
        {{type :local/type} :root/local
         images :root/token-images} result
        images   (sequence (map :image/checksum) images)
        upload   (use-image-uploader {:type :token})
        input    (use-ref)]
    ($ :<>
      ($ :button.button.button-neutral
        {:type     "button"
         :title    "Upload token image"
         :on-click #(.click (deref input))}
        ($ :input
          {:type "file" :hidden true :accept "image/*" :multiple true :ref input
           :on-change
           (fn [event]
             (doseq [file (.. event -target -files)]
               (upload file))
             (set! (.. event -target -value) ""))})
        ($ icon {:name "camera-fill" :size 16}) "Select Files")
      ($ :button.button.button-danger
        {:type     "button"
         :title    "Remove all"
         :disabled (or (= type :conn) (empty? images))
         :on-click #(dispatch :tokens/remove-all images)}
        ($ icon {:name "trash3-fill" :size 16})))))
