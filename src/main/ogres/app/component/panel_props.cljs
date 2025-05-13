(ns ogres.app.component.panel-props
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.hooks :as hooks]
            [ogres.app.vec :as vec :refer [Vec2]]
            [uix.core :as uix :refer [defui $]]
            [uix.dom :refer [create-portal]]
            ["@dnd-kit/core" :as dnd]
            ["@dnd-kit/modifiers" :as modifiers]))

(def ^:private query
  [{:root/props-images
    [:image/hash
     :image/name
     :image/width
     :image/height
     {:image/thumbnail
      [:image/hash]}]}])

(defui ^:private element [props]
  (let [{{hash :image/hash
          {thumb :image/hash}
          :image/thumbnail} :image} props
        url (hooks/use-image thumb)
        opt (dnd/useDraggable #js {"id" hash "data" #js {"hash" hash}})]
    ($ :.props-gallery-image
      {:ref (.-setNodeRef opt)
       :style {:background-image (str "url(" url ")")}
       :on-pointer-down (.. opt -listeners -onPointerDown)})))

(defui ^:private ^:memo overlay []
  (let [[active set-active] (uix/use-state nil)
        url (hooks/use-image active)]
    (dnd/useDndMonitor
     #js {"onDragStart" (fn [event] (set-active (.. event -active -data -current -hash)))
          "onDragEnd"   (fn [_]     (set-active nil))})
    (create-portal
     ($ dnd/DragOverlay
       {:modifiers #js [modifiers/snapCenterToCursor]
        :drop-animation nil}
       ($ :img.props-gallery-overlay-content
         {:src url}))
     js/document.body)))

(defui ^:private ^:memo gallery []
  (let [result (hooks/use-query query [:db/ident :root])
        images (:root/props-images result)]
    ($ :fieldset.fieldset.props-gallery
      ($ :legend "Images")
      ($ :.props-gallery-page
        (for [image images]
          ($ element {:key (:image/hash image) :image image}))))))

(defui ^:private ^:memo container []
  (let [dispatch (hooks/use-dispatch)]
    ($ dnd/DndContext
      #js {"onDragEnd"
           (fn [event]
             (let [initial (.. event -active -rect -current -initial)
                   initial (Vec2. (.-left initial) (.-top initial))
                   delta   (Vec2. (.-x (.-delta event)) (.-y (.-delta event)))
                   _       (vec/add initial delta)
                   image   (.. event -active -data -current -hash)]
               (dispatch :props/create (Vec2. 0 0) image)))}
      ($ gallery)
      ($ overlay))))

(defui ^:memo panel []
  ($ :.form-props
    ($ :header ($ :h2 "Props"))
    ($ container)
    ($ :.form-notice
      "Props are images you can upload and use in the scene as decoration,
       vehicles, or effects. Once placed within the scene, they can be
       resized, rotated, and moved around freely."
      ($ :br)
      ($ :br)
      ($ :strong "Locking props ") "will prevent them from being accidentally "
      "moved around. To unlock them, " ($ :strong "double-click") " it to "
      "select it.")))

(defui ^:memo footer []
  (let [upload (hooks/use-image-uploader {:type :props})
        input (uix/use-ref)]
    ($ :<>
      ($ :input
        {:ref input
         :type "file"
         :hidden true
         :accept "image/*"
         :multiple true
         :on-change
         (fn [event]
           (upload (.. event -target -files))
           (set! (.. event -target -value) ""))})
      ($ :button.button.button-neutral
        {:on-click (fn [] (.click @input))}
        ($ icon {:name "camera-fill" :size 16})
        "Upload images")
      ($ :button.button.button-danger
        ($ icon {:name "trash3-fill" :size 16})))))
