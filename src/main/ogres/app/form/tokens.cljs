(ns ogres.app.form.tokens
  (:require [goog.object :refer [getValueByKeys]]
            [ogres.app.hooks :refer [use-dispatch use-image use-image-uploader use-query]]
            [ogres.app.render :refer [icon pagination]]
            [ogres.app.util :refer [separate comp-fn]]
            [uix.core :as uix :refer [defui $ use-callback use-ref use-state]]
            [uix.dom :refer [create-portal]]
            ["@dnd-kit/core"
             :refer  [DndContext DragOverlay useDndMonitor useDraggable useDroppable]
             :rename {DndContext    dnd-context
                      DragOverlay   drag-overlay
                      useDndMonitor use-dnd-monitor
                      useDraggable  use-draggable
                      useDroppable  use-droppable}]))

(def ^:private query-footer
  [{:root/local [:local/type]}
   {:root/token-images [:image/checksum]}])

(def ^:private query-form
  [{:root/token-images [:image/checksum :image/scope]}
   {:root/local
    [[:local/type :default :conn]]}])

(def ^:private query-bounds
  [[:bounds/self :default [0 0 0 0]]])

(defui ^:private image
  [{:keys [checksum children]}]
  (let [data-url (use-image checksum)]
    (children {:data-url data-url})))

(defui ^:private draggable
  [{:keys [id children]}]
  (let [options (use-draggable #js {"id" id})]
    (children {:options options})))

(defui ^:private token
  [{:keys [checksum children]}]
  (let [data-url (use-image checksum)
        options  (use-draggable #js {"id" checksum})]
    (children {:data-url data-url :options options})))

(defui ^:private overlay []
  (let [[active set-active] (use-state nil)]
    (use-dnd-monitor
     #js {"onDragStart" (fn [event] (set-active (.. event -active -id)))
          "onDragEnd"   (fn [_]     (set-active nil))})
    (create-portal
     ($ drag-overlay {:drop-animation nil}
       (if (= active "default")
         ($ :figure.token-gallery-item
           {:data-type "default"}
           ($ icon {:name "dnd"}))
         (if (some? active)
           ($ image {:checksum active}
             (fn [{:keys [data-url]}]
               ($ :figure.token-gallery-item
                 {:data-type "image"
                  :style {:background-image (str "url(" data-url ")")}}))))))
     (.querySelector js/document "#root"))))

(defui ^:private gallery [props]
  (let [option (use-droppable #js {"id" "trash"})]
    ($ :<>
      (for [[idx data] (sequence (map-indexed vector) (:data props))]
        (cond (map? data)
              (let [checksum (:image/checksum data)]
                ($ token {:key checksum :checksum checksum}
                  (fn [{:keys [data-url ^js/object options]}]
                    ($ :figure.token-gallery-item
                      {:ref (.-setNodeRef options)
                       :data-type "image"
                       :style {:background-image (str "url(" data-url ")")}
                       :on-pointer-down (.. options -listeners -onPointerDown)
                       :on-key-down     (.. options -listeners -onKeyDown)}))))
              (= data :default)
              ($ draggable {:key idx :id "default"}
                (fn [{:keys [^js/object options]}]
                  ($ :figure.token-gallery-item
                    {:ref (.-setNodeRef options)
                     :data-type "default"
                     :on-pointer-down (.. options -listeners -onPointerDown)
                     :on-key-down (.. options -listeners -onKeyDown)}
                    ($ icon {:name "dnd"}))))
              (= data :placeholder)
              ($ :figure.token-gallery-item {:key idx :data-type "placeholder"})))
      ($ :figure.token-gallery-item
        {:ref (.-setNodeRef option) :data-type "trash"}
        ($ icon {:name "trash3-fill" :size 26})))))

(defui ^:private paginated
  [props]
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
      ($ gallery {:data data})
      (if (> pages 1)
        ($ pagination
          {:pages (max pages 1)
           :value (max (min pages page) 1)
           :on-change set-page})))))

(defui tokens []
  (let [result (use-query query-form [:db/ident :root])
        {data :root/token-images
         {type :local/type} :root/local} result
        [pub prv] (separate (comp-fn = :image/scope :public) data)
        data-pub  (into [:default] (reverse pub))
        data-prv  (vec (reverse prv))
        drop-pub  (use-droppable #js {"id" "scope-pub"})
        drop-prv  (use-droppable #js {"id" "scope-prv"})]
    ($ :<>
      (if (= type :host)
        ($ :<>
          ($ :header "Public [" (count data-pub) "]")
          ($ :section.token-gallery
            {:ref (.-setNodeRef drop-pub) :data-type "host" :data-scope "public"}
            ($ paginated {:data data-pub :limit 10}))
          ($ :header "Private [" (count data-prv) "]")
          ($ :section.token-gallery
            {:ref (.-setNodeRef drop-prv) :data-type "host" :data-scope "private"}
            ($ paginated {:data data-prv :limit 20})))
        ($ :section.token-gallery
          {:ref (.-setNodeRef drop-pub) :data-type "conn" :data-scope "public"}
          ($ paginated {:data data-pub :limit 30}))))))

(defui form []
  (let [dispatch (use-dispatch)
        results  (use-query query-bounds [:db/ident :local])
        {[bx by bw bh] :bounds/self} results
        on-create
        (use-callback
         (fn [checksum element delta]
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
               (dispatch :token/create mx my (if (not= checksum "default") checksum nil)))))
         [dispatch bx by bw bh])]
    ($ dnd-context
      #js {"onDragEnd"
           (use-callback
            (fn [event]
              (let [drag (getValueByKeys event #js ["active" "id"])
                    drop (getValueByKeys event #js ["over" "id"])]
                (if (and (some? drop) (not= drag "default"))
                  (case drop
                    "scope-pub" (dispatch :tokens/change-scope drag :public)
                    "scope-prv" (dispatch :tokens/change-scope drag :private)
                    "trash" (dispatch :tokens/remove drag))
                  (let [target (.. event -activatorEvent -target)
                        delta  (.-delta event)]
                    (on-create drag target delta)))))
            [dispatch on-create])}
      ($ tokens)
      ($ overlay))))

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
