(ns ogres.app.form.tokens
  (:require [ogres.app.form.render :as render]
            [ogres.app.hooks :refer [use-dispatch use-image use-image-uploader use-query]]
            [ogres.app.render :refer [icon pagination]]
            [uix.core.alpha :as uix]
            [uix.dom.alpha :refer [create-portal]]
            ["@dnd-kit/core" :refer [DndContext DragOverlay useDraggable]]))

(def ^:private query
  [{:root/stamps [:image/checksum]}
   {:root/local
    [[:bounds/self :default [0 0 0 0]]
     {:local/window
      [[:window/scale :default 1]
       [:window/vec :default [0 0]]]}]}])

(defn- key-by
  "Returns a map of the given `coll` whose keys are the result of calling `f`
   with each element in the collection and whose values are the element
   itself."
  [f coll]
  (into {} (map (juxt f identity)) coll))

(defn- header []
  (let [input  (uix/ref)
        upload (use-image-uploader {:type :token})]
    [:button.upload-button
     {:type "button"
      :on-click (fn [event]
                  (.stopPropagation event)
                  (.click (deref input)))}
     [:input
      {:type "file" :hidden true :accept "image/*" :multiple true :ref input
       :on-click  (fn [event] (.stopPropagation event))
       :on-change (fn [event]
                    (doseq [file (.. event -target -files)]
                      (upload file)))}]
     [icon {:name "camera-fill" :size 18}]
     "Upload"]))

(defn- token [props _]
  (let [checksum (:image/checksum (:data props))
        data-url (use-image checksum)
        drag-opt (useDraggable #js {"id" checksum})]
    [:figure.tokens-template
     {:ref   (.-setNodeRef drag-opt)
      :style {:background-image (str "url(" data-url ")")}
      :on-pointer-down (.. drag-opt -listeners -onPointerDown)
      :on-key-down     (.. drag-opt -listeners -onKeyDown)}]))

(defn- overlay [props _]
  (let [checksum (:image/checksum (:data props))
        data-url (use-image checksum)]
    [:figure.tokens-template
     {:style {:background-image (str "url(" data-url ")")}}]))

(defn- tokens [props _]
  (let [lookup (key-by :image/checksum (:data props))
        active (uix/state nil)]
    [:> DndContext
     {:on-drag-start
      (fn [event]
        (reset! active (.. event -active -id)))
      :on-drag-end
      (fn [event]
        (let [checksum (.. event -active -id)
              target   (.. event -activatorEvent -target)
              delta    (.-delta event)]
          ((:on-drop-token props) checksum target delta)
          (reset! active nil)))}
     (for [{:keys [image/checksum] :as data} (:data props)]
       ^{:key checksum} [token {:data data}])
     [create-portal
      [:> DragOverlay
       {:drop-animation nil}
       (if-let [checksum (deref active)]
         [overlay {:data (lookup checksum)}])]
      (.-body js/document)]]))

(defn- form []
  (let [dispatch (use-dispatch)
        page (uix/state 1)
        rslt (use-query query [:db/ident :root])
        data (vec (:root/stamps rslt))
        {{[bx by bw bh] :bounds/self
          {[cx cy] :window/vec
           scale   :window/scale}
          :local/window}
         :root/local} rslt]
    (if (seq data)
      (let [src (-> (deref page) (dec) (* 20))
            dst (-> (+ src 20) (min (count data)))]
        [:section.tokens
         [tokens
          {:data (subvec data src dst)
           :on-drop-token
           (fn [checksum element delta]
             (let [rect    (.getBoundingClientRect element)
                   [tw th] [(.-width rect) (.-height rect)]
                   [tx ty] [(.-x rect) (.-y rect)]
                   [dx dy] [(.-x delta) (.-y delta)]
                   [mx my] [(- (+ tx dx (/ tw 2)) bx) (- (+ ty dy (/ th 2)) by)]
                   [sx sy] [(- (* mx (/ scale)) cx) (- (* my (/ scale)) cy)]]
               (if (and (<= bx mx (+ bx bw))
                        (<= by my (+ by bh)))
                 (dispatch :token/create sx sy checksum))))}]
         [pagination
          {:pages (int (js/Math.ceil (/ (count data) 20)))
           :value (deref page)
           :on-change (partial reset! page)}]])
      [:section.tokens
       [:div.prompt
        [:br] "Upload one or more images from your"
        [:br] "computer to use as tokens."]])))

(defmethod render/header :tokens [] header)
(defmethod render/form :tokens [] form)
