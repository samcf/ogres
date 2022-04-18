(ns ogre.tools.render.canvas
  (:require [clojure.set :refer [difference]]
            [clojure.string :refer [capitalize join]]
            [datascript.core :refer [squuid]]
            [ogre.tools.image :refer [load checksum]]
            [ogre.tools.geom :refer [bounding-box chebyshev euclidean triangle]]
            [ogre.tools.render :refer [icon use-image]]
            [ogre.tools.render.draw :refer [draw]]
            [ogre.tools.render.modal :refer [modal]]
            [ogre.tools.render.pattern :refer [pattern]]
            [ogre.tools.state :refer [use-query]]
            [ogre.tools.storage :refer [storage]]
            [react-draggable]
            [uix.core.alpha :as uix]))

(def draw-modes
  #{:grid :ruler :circle :rect :cone :line :poly :mask})

(def atmosphere
  {:none     [1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0 0.0 0.0 0.0 0.0 1.0 0.0]
   :dusk     [0.3 0.3 0.0 0.0 0.0 0.0 0.3 0.3 0.0 0.0 0.0 0.0 0.8 0.0 0.0 0.0 0.0 0.0 1.0 0.0]
   :midnight [0.0 0.0 0.0 0.0 0.0 0.0 0.1 0.0 0.0 0.0 0.1 0.1 0.1 0.0 0.0 0.0 0.0 0.0 1.0 0.0]})

(def conditions
  [[:player "people-fill"]
   [:blinded "eye-slash-fill"]
   [:charmed "arrow-through-heart-fill"]
   [:exhausted "moon-stars-fill"]
   [:invisible "incognito"]
   [:grappled "fist"]
   [:prone "falling"]
   [:frightened "black-cat"]
   [:incapacitated "dizzy"]
   [:unconscious "skull"]])

(defn separate
  "Split coll into two sequences, one that matches pred and one that doesn't."
  [pred coll]
  (let [pcoll (map (juxt identity pred) coll)]
    (vec (for [f [filter remove]]
           (map first (f second pcoll))))))

(defn stop-propagation [event]
  (.stopPropagation event))

(defn ft->px [ft size] (* (/ ft 5) size))

(defn visible? [flags]
  (or (nil? flags)
      (flags :player)
      (not (some flags [:hidden :invisible]))))

(defn label [{:keys [element/name initiative/suffix]}]
  (cond-> ""
    (string? name) (str name)
    (number? suffix) (str " " (char (+ suffix 64)))))

(defn checkbox [{:keys [checked on-change]} render-fn]
  (let [input (uix/ref)
        indtr (= checked :indeterminate)
        key   (deref (uix/state (squuid)))]
    (uix/effect!
     (fn [] (set! (.-indeterminate @input) indtr)) [indtr])
    (render-fn
     [:input
      {:id key :type "checkbox" :ref input :checked (if indtr false checked)
       :on-change
       (fn [event]
         (on-change (.. event -target -checked)))}] key)))

(def scene-query
  {:pull
   [{:root/canvas
     [[:canvas/color :default :none]
      {:canvas/scene
       [:image/checksum]}]}]})

(defn scene []
  (let [[result] (use-query scene-query)
        {{color :canvas/color
          {checksum :image/checksum} :canvas/scene} :root/canvas} result
        url (use-image checksum)]
    [:g.canvas-image
     [:defs {:key color}
      [:filter {:id "atmosphere"}
       [:feColorMatrix {:type "matrix" :values (join " " (atmosphere color))}]]]
     [:image {:x 0 :y 0 :href url :style {:filter "url(#atmosphere)"}}]]))

(def light-mask-query
  {:pull
   [:root/host?
    {:root/canvas
     [[:canvas/visibility :default :revealed]
      [:grid/size :default 70]
      {:canvas/tokens
       [:db/id
        [:element/flags :default #{}]
        [:token/light :default 15]
        [:pos/vec :default [0 0]]]}
      {:canvas/scene
       [:image/checksum
        :image/width
        :image/height]}]}]})

(defn light-mask []
  (let [[result] (use-query light-mask-query)
        {host? :root/host?
         {visibility :canvas/visibility
          tokens     :canvas/tokens
          size       :grid/size
          {checksum :image/checksum
           width    :image/width
           height   :image/height} :canvas/scene} :root/canvas} result]
    (if (and checksum (not= visibility :revealed))
      [:g.canvas-mask {:css {:is-dimmed (= visibility :dimmed)}}
       [:defs
        [pattern {:id "mask-pattern" :name :lines}]
        [:radialGradient {:id "mask-gradient"}
         [:stop {:offset "0%" :stop-color "black" :stop-opacity "100%"}]
         [:stop {:offset "70%" :stop-color "black" :stop-opacity "100%"}]
         [:stop {:offset "100%" :stop-color "black" :stop-opacity "0%"}]]
        [:mask {:id "light-mask"}
         [:rect {:x 0 :y 0 :width width :height height :fill "white" :fill-opacity "100%"}]
         (for [{id :db/id flags :element/flags [x y] :pos/vec radius :token/light} tokens
               :when (and (> radius 0) (or host? (visible? flags)))]
           [:circle {:key id :cx x :cy y :r (+ (ft->px radius size) (/ size 2)) :fill "url(#mask-gradient)"}])]]
       [:rect.canvas-mask-background
        {:x 0 :y 0 :width width :height height :mask "url(#light-mask)"}]
       (if (= visibility :hidden)
         [:rect.canvas-mask-pattern
          {:x 0 :y 0 :width width :height height
           :fill "url(#mask-pattern)" :mask "url(#light-mask)"}])])))

(def canvas-mask-query
  {:pull
   [:root/host?
    {:root/canvas
     [[:canvas/mode :default :select]
      [:mask/filled? :default false]
      {:canvas/scene [:image/width :image/height]}
      {:canvas/masks [:db/id :mask/vecs :mask/enabled?]}]}]})

(defn canvas-mask []
  (let [[result dispatch] (use-query canvas-mask-query)
        {host?    :root/host?
         {filled? :mask/filled?
          masks   :canvas/masks
          mode    :canvas/mode
          {width  :image/width
           height :image/height} :canvas/scene} :root/canvas} result
        modes #{:mask :mask-toggle :mask-remove}]
    [:g.canvas-mask
     [:defs
      [pattern {:id "mask-pattern" :name :lines}]
      [:mask {:id "canvas-mask"}
       (if filled?
         [:rect {:x 0 :y 0 :width width :height height :fill "white"}])
       (for [{id :db/id enabled? :mask/enabled? xs :mask/vecs} masks]
         [:polygon {:key id :points (join " " xs) :fill (if enabled? "white" "black")}])]]
     [:rect.canvas-mask-background {:x 0 :y 0 :width width :height height :mask "url(#canvas-mask)"}]
     [:rect.canvas-mask-pattern {:x 0 :y 0 :width width :height height :fill "url(#mask-pattern)" :mask "url(#canvas-mask)"}]
     (if (and host? (contains? modes mode))
       (for [{id :db/id xs :mask/vecs enabled? :mask/enabled?} masks]
         [:polygon.canvas-mask-polygon
          {:key id
           :data-enabled enabled?
           :points (join " " xs)
           :on-mouse-down stop-propagation
           :on-click
           (fn []
             (case mode
               :mask-toggle (dispatch :mask/toggle id (not enabled?))
               :mask-remove (dispatch :mask/remove id)))}]))]))

(def grid-query
  {:pull
   [:bounds/self
    {:root/canvas
     [[:canvas/mode :default :select]
      [:pos/vec :default [0 0]]
      [:grid/size :default 70]
      [:grid/show :default true]
      [:zoom/scale :default 1]]}]})

(defn grid []
  (let [[data] (use-query grid-query)
        {[_ _ w h] :bounds/self
         {mode    :canvas/mode
          size    :grid/size
          show    :grid/show
          scale   :zoom/scale
          [cx cy] :pos/vec} :root/canvas} data]
    (if (or show (= mode :grid))
      (let [w (/ w scale)
            h (/ h scale)
            [sx sy ax ay bx]
            [(- (* w -3) cx)
             (- (* h -3) cy)
             (- (* w  3) cx)
             (- (* h  3) cy)
             (- (* w -3) cx)]]
        [:g {:class "canvas-grid"}
         [:defs
          [:pattern {:id "grid" :width size :height size :patternUnits "userSpaceOnUse"}
           [:path
            {:d (join " " ["M" 0 0 "H" size "V" size])}]]]
         [:path {:d (join " " ["M" sx sy "H" ax "V" ay "H" bx "Z"]) :fill "url(#grid)"}]]))))

(defmulti shape (fn [props] (:shape/kind (:element props))))

(defmethod shape :circle [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [ax ay bx by] vecs]
    [:circle
     (merge
      attrs
      {:cx 0 :cy 0 :r (chebyshev ax ay bx by)
       :fill-opacity opacity :stroke color})]))

(defmethod shape :rect [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [ax ay bx by] vecs]
    [:path
     (merge
      attrs
      {:d (join " " ["M" 0 0 "H" (- bx ax) "V" (- by ay) "H" 0 "Z"])
       :fill-opacity opacity :stroke color})]))

(defmethod shape :line [props]
  (let [{:keys [element]} props
        {:keys [shape/vecs shape/color]} element
        [ax ay bx by] vecs]
    [:line {:x1 0 :y1 0 :x2 (- bx ax) :y2 (- by ay) :stroke color :stroke-width 4 :stroke-linecap "round"}]))

(defmethod shape :cone [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [ax ay bx by] vecs]
    [:polygon
     (merge
      attrs
      {:points (join " " (triangle 0 0 (- bx ax) (- by ay)))
       :fill-opacity opacity :stroke color})]))

(defn poly-xf [x y]
  (comp (partition-all 2)
        (mapcat (fn [[ax ay]] [(- ax x) (- ay y)]))))

(defmethod shape :poly [props]
  (let [{:keys [element attrs]} props
        {:keys [shape/vecs shape/color shape/opacity]} element
        [ax ay] (into [] (take 2) vecs)
        pairs   (into [] (poly-xf ax ay) vecs)]
    [:polygon (assoc attrs :points (join " " pairs) :fill-opacity opacity :stroke color)]))

(def shapes-query
  {:pull
   '[{:root/canvas
      [[:zoom/scale :default 1]
       [:grid/align :default false]
       {:canvas/shapes
        [:db/id
         :element/name
         :shape/kind
         :shape/vecs
         [:shape/color :default "#f44336"]
         [:shape/opacity :default 0.25]
         [:shape/pattern :default :solid]
         :canvas/_selected]}]}]})

(defn shapes []
  (let [[result dispatch] (use-query shapes-query)]
    (for [element (-> result :root/canvas :canvas/shapes)]
      (let [{id :db/id [ax ay] :shape/vecs} element]
        [:> react-draggable
         {:key      id
          :scale    (-> result :root/canvas :zoom/scale)
          :position #js {:x ax :y ay}
          :on-start stop-propagation
          :on-stop
          (fn [event data]
            (let [ox (.-x data) oy (.-y data)]
              (if (> (euclidean ax ay ox oy) 0)
                (dispatch :shape/translate id ox oy (not= (.-metaKey event) (-> result :root/canvas :grid/align)))
                (dispatch :element/select id))))}
         (let [id (squuid)]
           [:g
            {:css
             {:canvas-shape true
              :selected (:canvas/_selected element)
              (str "canvas-shape-" (name (:shape/kind element))) true}}
            [:defs [pattern {:id id :name (:shape/pattern element) :color (:shape/color element)}]]
            [shape {:element element :attrs {:fill (str "url(#" id ")")}}]])]))))

(defn stamp [{:keys [checksum]}]
  (let [url (use-image checksum)]
    [:image {:href url :width 1 :height 1 :preserveAspectRatio "xMidYMin slice"}]))

(def stamps-query
  {:query '[:find [?cs ...] :where
            [[:db/ident :canvas] :canvas/tokens ?tk]
            [?tk :token/stamp ?st]
            [?st :image/checksum ?cs]]})

(defn stamps []
  (let [[checksums] (use-query stamps-query)
        attrs  {:width "100%" :height "100%" :patternContentUnits "objectBoundingBox"}]
    [:defs
     [:pattern (merge attrs {:id "token-stamp-default" :viewBox "0 0 16 16" :fill "#f2f2eb"})
      [:rect {:x 0 :y 0 :width 16 :height 16 :fill "hsl(200, 20%, 12%)"}]
      [:path {:d "M11 6a3 3 0 1 1-6 0 3 3 0 0 1 6 0z"}]
      [:path {:d "M0 8a8 8 0 1 1 16 0A8 8 0 0 1 0 8zm8-7a7 7 0 0 0-5.468 11.37C3.242 11.226 4.805 10 8 10s4.757 1.225 5.468 2.37A7 7 0 0 0 8 1z" :fill-rule "evenodd"}]]
     [:pattern (merge attrs {:id "token-stamp-deceased" :viewBox "-2 -2 16 16" :fill "#f2f2eb"})
      [:rect {:x -2 :y -2 :width 16 :height 16 :fill "hsl(200, 20%, 12%)"}]
      [icon {:name "skull" :size 12}]]
     (for [checksum checksums]
       [:pattern (merge attrs {:key checksum :id (str "token-stamp-" checksum)})
        [stamp {:checksum checksum}]])]))

(defn file-uploader [props]
  [:input
   {:type "file" :ref (:ref props) :accept "image/*" :multiple true
    :style {:display "none"}
    :on-change
    (fn [event]
      (doseq [file (.. event -target -files)]
        (.then (load file) (:on-upload props))))}])

(defn thumbnail [checksum render-fn]
  (render-fn (use-image checksum)))

(defn images-form [{:keys [on-change]}]
  (let [[result dispatch] (use-query {:pull [{:root/stamps [:image/checksum]}]})
        {:keys [store]}   (uix/context storage)
        thumbnails        (into [] (comp (map :image/checksum) (partition-all 15)) (reverse (:root/stamps result)))
        page-index        (uix/state 0)
        upload-ref        (uix/ref)]
    [:<>
     [file-uploader
      {:ref upload-ref
       :on-upload
       (fn [[file data-url image]]
         (let [checksum (checksum data-url)
               record   #js {:checksum checksum :data data-url :created-at (.now js/Date)}
               entity   {:image/checksum checksum
                         :image/name     (.-name file)
                         :image/width    (.-width image)
                         :image/height   (.-height image)}]
           (.then
            (.put (.-images store) record)
            (fn [] (dispatch :stamp/create entity))
            (reset! page-index 0))))}]
     [:div.images-form
      (concat
       [[:button.button
         {:key "prev" :type "button" :disabled (= @page-index 0)
          :on-click #(swap! page-index dec)}
         [icon {:name "chevron-double-left"}]]
        [:button.button
         {:key "upload" :type "button" :on-click #(.click @upload-ref)}
         [icon {:name "arrow-up-circle-fill"}]
         [:span "Upload new images"]]
        [:button.button
         {:key "next" :type "button"
          :disabled (>= @page-index (- (count thumbnails) 1))
          :on-click #(swap! page-index inc)}
         [icon {:name "chevron-double-right"}]]]
       (for [checksum (nth thumbnails @page-index [])]
         ^{:key checksum}
         [thumbnail checksum
          (fn [url]
            [:figure
             {:style {:background-image (str "url(" url ")")} :on-click #(on-change checksum)}
             [:div
              {:title "Remove"
               :on-click
               (fn [event]
                 (.stopPropagation event)
                 (dispatch :stamp/remove checksum)
                 (.delete (.-images store) checksum))}
              (js/String.fromCharCode 215)]])]))]]))

(defmulti form :name)

(defmethod form :default [] nil)

(defmethod form :label [props]
  (let [input-ref   (uix/ref)
        input-val   (uix/state
                     (fn []
                       (let [vs ((:values props) :element/name)]
                         (if (= (count vs) 1) (first vs) ""))))
        modal-open? (uix/state false)]
    (uix/effect! #(.select @input-ref) [])
    [:form
     {:on-submit
      (fn [event]
        (.preventDefault event)
        ((:on-change props) :token/change-label @input-val)
        ((:on-close props)))}
     [:button.button
      {:type         "button"
       :data-tooltip "Select or upload an image"
       :on-click     #(swap! modal-open? not)}
      [icon {:name "person-circle" :size 22}]]
     [:input
      {:type "text"
       :ref input-ref
       :value @input-val
       :auto-focus true
       :placeholder "Press 'Enter' to submit..."
       :on-change #(reset! input-val (.. %1 -target -value))}]
     (if @modal-open?
       [modal
        [:div.context-form-modal
         [images-form
          {:on-change
           (fn [checksum]
             ((:on-change props) :token/change-stamp checksum))}]]])]))

(defmethod form :details [props]
  (for [[label tx-name attr min def]
        [["Size" :token/change-size :token/size 5 5]
         ["Light" :token/change-light :token/light 0 15]
         ["Aura" :token/change-aura :aura/radius 0 0]]]
    (let [values ((:values props) attr)]
      [:div {:key label}
       [:legend label]
       [:span
        (cond
          (> (count values) 1) "Multiple..."
          (= (count values) 0) (str def "ft.")
          (= (first values) 0) "None"
          (= (count values) 1) (str (first values) "ft."))]
       [:button.button
        {:type "button"
         :on-click
         (fn []
           (let [next (if (> (count values) 1) min (max (- (first values) 5) min))]
             ((:on-change props) tx-name next)))} "-"]
       [:button.button
        {:type "button"
         :on-click
         (fn []
           (let [next (if (> (count values) 1) 5 (+ (first values) 5))]
             ((:on-change props) tx-name next)))} "+"]])))

(defmethod form :conditions [props]
  (let [fqs (frequencies (reduce into [] ((:values props) :element/flags [])))
        ids ((:values props) :db/id)]
    (for [[flag icon-name] conditions]
      ^{:key flag}
      [checkbox
       {:checked
        (cond (= (get fqs flag 0) 0) false
              (= (get fqs flag 0) (count ids)) true
              :else :indeterminate)
        :on-change #((:on-change props) :element/flag flag %1)}
       (fn [input key]
         [:div input
          [:label {:for key :data-tooltip (capitalize (name flag))}
           [icon {:name icon-name :size 22}]]])])))

(defn context-menu [{tokens :tokens}]
  (let [dispatch   (use-query)
        idents     (map :db/id tokens)
        selected   (uix/state nil)]
    [:div.context-menu
     {:on-mouse-down stop-propagation}
     [:div.context-toolbar
      (for [[form icon-name tooltip]
            [[:label "fonts" "Label"]
             [:details "sliders" "Options"]
             [:conditions "flag-fill" "Conditions"]]]
        [:button
         {:key form :type "button" :data-tooltip tooltip
          :css {:selected (= @selected form)}
          :on-click
          (fn []
            (swap! selected (fn [prev] (if (not (= prev form)) form nil))))}
         [icon {:name icon-name :size 22}]])
      (let [on (every? (comp boolean :hidden :element/flags) tokens)]
        [:button
         {:type "button" :css {:selected on} :data-tooltip (if on "Reveal" "Hide")
          :on-click #(dispatch :element/flag idents :hidden (not on))}
         [icon {:name (if on "eye-slash-fill" "eye-fill") :size 22}]])
      (let [on (every? (comp vector? :canvas/_initiative) tokens)]
        [:button
         {:type "button" :css {:selected on} :data-tooltip "Initiative"
          :on-click #(dispatch :initiative/toggle idents (not on))}
         [icon {:name "hourglass-split" :size 22}]])
      [:button
       {:type "button" :data-tooltip "Remove"
        :on-click #(dispatch :element/remove idents)}
       [icon {:name "trash" :size 22}]]]
     (if-let [form-name @selected]
       [:div.context-form
        {:key form-name :css (str "context-form-" (name form-name))}
        [form
         {:name      form-name
          :on-close  #(reset! selected nil)
          :on-change #(apply dispatch %1 idents %&)
          :values    (fn vs
                       ([f] (vs f #{}))
                       ([f init] (into init (map f) tokens)))}]])]))

(defn token [{:keys [data size]}]
  (let [radius (-> data :token/size (ft->px size) (/ 2) (- 2) (max 16))]
    [:<>
     (if (> (:aura/radius data) 0)
       [:circle.canvas-token-aura
        {:cx 0 :cy 0 :r (+ (ft->px (:aura/radius data) size) (/ size 2))}])
     [:circle.canvas-token-ring
      {:cx 0 :cy 0 :style {:r radius :fill "transparent"}}]
     (let [checksum (:image/checksum (:token/stamp data))
           pattern  (cond
                      ((:element/flags data) :unconscious) "token-stamp-deceased"
                      (string? checksum)   (str "token-stamp-" checksum)
                      :else                "token-stamp-default")]
       [:circle.canvas-token-shape
        {:cx 0 :cy 0 :r radius :fill (str "url(#" pattern ")")}])
     (let [icons (into {} conditions)
           degrs [125 95 65 -125 -95 -65]
           exclu #{:player :hidden :unconscious}]
       (for [[index flag]
             (into [] (comp (take 6) (map-indexed vector))
                   (difference (:element/flags data) exclu))]
         (let [rn (* (/ js/Math.PI 180) (nth degrs index 0))
               cx (* (js/Math.sin rn) radius)
               cy (* (js/Math.cos rn) radius)]
           [:g.canvas-token-flags {:key flag :transform (str "translate(" cx ", " cy ")")}
            [:circle {:cx 0 :cy 0 :r 8}]
            [:g {:transform (str "translate(" -6 ", " -6 ")")}
             [icon {:name (icons flag) :size 12}]]])))
     (let [token-label (label data)]
       (if (seq token-label)
         [:foreignObject
          {:x -200 :y (- radius 8) :width 400 :height 32 :style {:pointer-events "none"}}
          [:div.canvas-token-label
           [:span token-label]]]))]))

(defn token-comparator [a b]
  (let [[ax ay] (:pos/vec a)
        [bx by] (:pos/vec b)]
    (compare [(:token/size b) by bx]
             [(:token/size a) ay ax])))

(def tokens-query
  {:pull
   [[:root/host? :default true]
    {:root/canvas
     [[:grid/size :default 70]
      [:grid/align :default false]
      [:zoom/scale :default 1]
      {:canvas/tokens
       [:db/id
        [:initiative/suffix :default nil]
        [:pos/vec :default [0 0]]
        [:element/flags :default #{}]
        [:element/name :default ""]
        [:token/size :default 5]
        [:token/light :default 15]
        [:aura/radius :default 0]
        {:token/stamp [:image/checksum]}
        {:canvas/_initiative [:db/id]}
        {:canvas/_selected [:db/id]}]}]}]})

(defn tokens []
  (let [[result dispatch] (use-query tokens-query)
        {host?   :root/host?
         {size   :grid/size
          align? :grid/align
          scale  :zoom/scale} :root/canvas} result

        flags-xf
        (comp (map name)
              (map (fn [s] (str "flag--" s)))
              (map (fn [s] [s true])))

        css
        (fn [token]
          (into {} flags-xf (:element/flags token)))

        [selected tokens]
        (->> (:canvas/tokens (:root/canvas result))
             (filter (fn [token] (or host? (visible? (:element/flags token)))))
             (sort token-comparator)
             (separate (fn [token] (contains? token :canvas/_selected))))]
    [:<>
     (for [data tokens :let [{id :db/id [ax ay] :pos/vec} data]]
       [:> react-draggable
        {:key      id
         :position #js {:x ax :y ay}
         :scale    scale
         :on-start stop-propagation
         :on-stop
         (fn [event data]
           (.stopPropagation event)
           (let [bx (.-x data) by (.-y data)]
             (if (= (euclidean ax ay bx by) 0)
               (dispatch :element/select id (not (.-shiftKey event)))
               (let [align? (not= (.-metaKey event) align?)]
                 (dispatch :token/translate id bx by align?)))))}
        [:g.canvas-token {:css (css data)}
         [token {:data data :size size}]]])
     (if (seq selected)
       (let [idents (map :db/id selected)
             [ax _ bx by] (apply bounding-box (map :pos/vec selected))]
         [:> react-draggable
          {:position #js {:x 0 :y 0}
           :scale    scale
           :on-start stop-propagation
           :on-stop
           (fn [event data]
             (let [ox (.-x data) oy (.-y data)]
               (if (and (= ox 0) (= oy 0))
                 (let [id (.. event -target (closest ".canvas-token[data-id]") -dataset -id)]
                   (dispatch :element/select (js/Number id) false))
                 (dispatch :token/translate-all idents ox oy (not= (.-metaKey event) align?)))))}
          [:g.canvas-selected {:key idents}
           (for [data selected :let [{id :db/id [x y] :pos/vec} data]]
             [:g.canvas-token
              {:key id :css (css data) :data-id id :transform (str "translate(" x "," y ")")}
              [token {:data data :size size}]])
           (if host?
             [:foreignObject
              {:x (- (+ (* ax scale) (/ (* (- bx ax) scale) 2)) (/ 400 2))
               :y (- (+ (* by scale) (* scale 56)) 24)
               :width 400 :height 400
               :transform (str "scale(" (/ scale) ")")
               :style {:pointer-events "none"}}
              [context-menu {:tokens selected}]])]]))]))

(defn bounds []
  (let [[result] (use-query {:pull [:bounds/host :bounds/guest]})
        {[_ _ hw hh] :bounds/host
         [_ _ gw gh] :bounds/guest} result
        [ox oy] [(/ (- hw gw) 2) (/ (- hh gh) 2)]]
    [:g.canvas-bounds {:transform (str "translate(" ox " , " oy ")")}
     [:rect {:x 0 :y 0 :width gw :height gh :rx 8}]]))

(def canvas-query
  {:pull
   [:root/privileged?
    :root/host?
    :bounds/host
    :bounds/guest
    {:root/canvas
     [:db/id
      [:pos/vec :default [0 0]]
      [:canvas/mode :default :select]
      [:canvas/theme :default :light]
      :canvas/modifier
      [:zoom/scale :default 1]]}]})

(defn canvas []
  (let [select-node       (uix/ref)
        [result dispatch] (use-query canvas-query)
        {priv? :root/privileged?
         host? :root/host?
         [_ _ hw hh] :bounds/host
         [_ _ gw gh] :bounds/guest
         {id    :db/id
          scale :zoom/scale
          mode  :canvas/mode
          theme :canvas/theme
          modif :canvas/modifier
          [cx cy] :pos/vec} :root/canvas} result
        cx (if host? cx (->> (- hw gw) (max 0) (* (/ -1 2 scale)) (+ cx)))
        cy (if host? cy (->> (- hh gh) (max 0) (* (/ -1 2 scale)) (+ cy)))]
    [:svg.canvas {:key id :css {(str "theme--" (name theme)) true :is-host host? :is-priv priv?}}
     [:> react-draggable
      {:position #js {:x 0 :y 0}
       :on-stop
       (fn [_ data]
         (let [ox (.-x data)
               oy (.-y data)]
           (if (and (= ox 0) (= oy 0))
             (dispatch :selection/clear)
             (let [tx (+ cx (* ox (/ scale)))
                   ty (+ cy (* oy (/ scale)))]
               (dispatch :camera/translate tx ty)))))}
      [:g {:style {:will-change "transform"}}
       [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]
       (if (and (= mode :select) (= modif :shift))
         [draw {:mode :select :node select-node}])
       [:g.canvas-board
        {:transform (str "scale(" scale ") translate(" cx ", " cy ")")}
        [stamps]
        [scene]
        [grid]
        [shapes]
        [tokens]
        [light-mask]
        [canvas-mask]]
       (if (and (= mode :select) (= modif :shift))
         [:g {:ref select-node :class "canvas-drawable canvas-drawable-select"}])
       (if (contains? draw-modes mode)
         [:g {:class (str "canvas-drawable canvas-drawable-" (name mode))}
          ^{:key mode} [draw {:mode mode :node nil}]])]]
     (if priv?
       [bounds])]))
