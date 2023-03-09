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
  '[{(:root/stamps :limit 1) [:image/checksum]}])

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

(defn- header []
  (let [dispatch (use-dispatch)
        result   (use-query header-query [:db/ident :root])
        upload   (use-image-uploader {:type :token})
        input    (uix/ref)]
    [:<>
     [:button.upload
      {:type     "button"
       :title    "Upload token image"
       :on-click (fn [event]
                   (.stopPropagation event)
                   (.click (deref input)))}
      [:input
       {:type "file" :hidden true :accept "image/*" :multiple true :ref input
        :on-click  (fn [event] (.stopPropagation event))
        :on-change (fn [event]
                     (doseq [file (.. event -target -files)]
                       (upload file)))}]
      [icon {:name "camera-fill" :size 16}] "Upload"]
     [:button.remove
      {:type     "button"
       :title    "Remove all"
       :disabled (empty? (:root/stamps result))
       :on-click (fn [event]
                   (.stopPropagation event)
                   (dispatch :stamp/remove-all))}
      [icon {:name "trash3-fill" :size 16}]]]))

(defn- tokens [props _]
  (let [{:keys [data on-create on-remove]
         :or   {on-create identity
                on-remove identity}} props
        active (uix/state nil)
        delete (uix/state false)
        option (use-droppable #js {"id" "tokens-trash"})
        tokens (->> (repeat per-page :placeholder)
                    (into data)
                    (take per-page)
                    (map-indexed vector))]
    (use-dnd-monitor
     #js {"onDragStart"
          (fn [event]
            (reset! active (.. event -active -id)))
          "onDragMove"
          (fn [event]
            (if (.-over event)
              (reset! delete true)
              (reset! delete false)))
          "onDragEnd"
          (fn [event]
            (if (.-over event)
              (do (on-remove @active)
                  (reset! active nil)
                  (reset! delete nil))
              (let [target (.. event -activatorEvent -target)
                    delta  (.-delta event)]
                (on-create @active target delta)
                (reset! active nil)
                (reset! delete nil))))})
    [:<>
     (for [[idx data] tokens]
       (if-let [checksum (:image/checksum data)]
         ^{:key checksum}
         [token checksum
          (fn [url ^js/object options]
            [:figure.tokens-template
             {:ref   (.-setNodeRef options)
              :style {:background-image (str "url(" url ")")}
              :on-pointer-down (.. options -listeners -onPointerDown)
              :on-key-down     (.. options -listeners -onKeyDown)}])]
         [:figure.tokens-placeholder
          {:key idx}]))
     [:figure.tokens-trashcan
      {:ref (.-setNodeRef option)
       :css {:is-deleting @delete}}
      [icon {:name "trash3-fill" :size 26}]]
     [create-portal
      [:> drag-overlay {:drop-animation nil}
       (if-let [checksum (deref active)]
         [image checksum
          (fn [url]
            [:figure.tokens-template
             {:css   {:is-deleting @delete}
              :style {:background-image (str "url(" url ")")}}])])]
      (.-body js/document)]]))

(defn- form []
  (let [dispatch  (use-dispatch)
        result    (use-query query [:db/ident :root])
        data      (vec (:root/stamps result))
        page      (uix/state 1)
        pages     (int (js/Math.ceil (/ (count data) per-page)))
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
                         (dispatch :token/create sx sy checksum))))
                   [result])
        on-remove (uix/callback
                   (partial dispatch :stamp/remove)
                   [])]
    (if (seq data)
      (let [src (* (dec (min @page pages)) per-page)
            dst (min (+ src per-page) (count data))]
        [:section.tokens
         [tokens
          {:data      (subvec data src dst)
           :on-create on-create
           :on-remove on-remove}]
         [pagination
          {:pages pages
           :value (min @page pages)
           :on-change (partial reset! page)}]])
      [:section.tokens
       [:div.prompt
        [:br] "Upload one or more images from your"
        [:br] "computer to use as tokens."]])))

(defmethod render/header :tokens [] header)
(defmethod render/form :tokens [] form)
