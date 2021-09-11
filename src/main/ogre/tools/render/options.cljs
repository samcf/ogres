(ns ogre.tools.render.options
  (:import goog.crypt.Md5)
  (:require  [clojure.string :as string]
             [ogre.tools.query :as query]
             [ogre.tools.render :refer [css use-image]]
             [ogre.tools.render.pattern :refer [pattern]]
             [ogre.tools.state :refer [state]]
             [ogre.tools.storage :refer [storage]]
             [uix.core.alpha :as uix]))

(def light-sources
  [["None" 0 0]
   ["Candle" 5 5]
   ["Torch" 20 20]
   ["Lamp" 15 30]
   ["Lantern" 30 30]])

(def conditions
  [:blinded :charmed :defeaned
   :exhausted :frightened :grappled
   :incapacitated :invisible :paralyzed
   :petrified :poisoned :prone
   :restrained :stunned :unconscious])

(def colors
  ["#182125" "#f2f2f2" "#f44336" "#e91e63"
   "#9c27b0" "#673ab7" "#3f51b5" "#2196f3"
   "#00bcd4" "#009688" "#4caf50" "#cddc39"
   "#ffeb3b" "#ffc107" "#ff9800"])

(defn checked? [pred coll]
  (let [pass (filter pred coll)]
    (cond
      (= (count coll) (count pass)) true
      (= (count pass) 0) false
      :else :indeterminate)))

(defn every-value? [coll f]
  (let [sample (f (first coll))]
    (if (every? #(= (f %) sample) coll)
      [true sample]
      [false nil])))

(defn checksum [data]
  (let [hash (new Md5)]
    (.update hash data)
    (reduce
     (fn [s b]
       (str s (.slice (str "0" (.toString b 16)) -2))) "" (.digest hash))))

(defn load-image [file handler]
  (let [reader (new js/FileReader)]
    (.readAsDataURL reader file)
    (.addEventListener
     reader "load"
     (fn [event]
       (let [data  (.. event -target -result)
             image (new js/Image)]
         (.addEventListener
          image "load"
          (fn []
            (this-as img (handler {:data data :filename (.-name file) :img img}))))
         (set! (.-src image) data))))))

(defn checkbox [{:keys [name type checked on-change] :as props} child]
  (let [indtr? (= checked :indeterminate)
        input  (uix/ref)
        id     (str name "/" (:value props))
        attrs  (merge
                props
                {:id        id
                 :ref       input
                 :type      "checkbox"
                 :class     "ogre-checkbox"
                 :checked   (if indtr? false checked)
                 :on-change (fn [event] (on-change (.. event -target -checked)))})]
    (uix/effect!
     (fn [] (set! (.-indeterminate @input) indtr?)) [indtr?])
    [:div
     [:input attrs]
     [:label {:for id} child]]))

(defn thumbnail [{:keys [board selected on-select on-remove]}]
  (let [url (use-image (:image/checksum board))]
    [:div {:key      (:image/checksum board)
           :class    (css {:selected selected})
           :style    {:background-image (str "url(" url ")")}
           :on-click
           (fn [event]
             (.stopPropagation event)
             (on-select))}
     [:div
      {:on-click
       (fn [event]
         (.stopPropagation event)
         (on-remove))} "×"]]))

(defn canvas []
  (let [{:keys [data workspace dispatch]} (uix/context state)
        {:keys [db/id element/name grid/size]} workspace
        {:keys [store]} (uix/context storage)]
    [:div.options.options-canvas
     [:section
      [:input
       {:type "text"
        :placeholder "Workspace name"
        :maxLength 24
        :spellCheck "false"
        :value (or name "")
        :on-change
        (fn [event]
          (let [value (.. event -target -value)]
            (dispatch :element/update [id] :element/name value)))}]
      [:button
       {:type "button" :on-click #(dispatch :canvas/toggle-mode :select)} "×"]]

     [:section
      [:header "Scene"]
      (when-let [boards (query/boards data)]
        [:div
         [:div.options-canvas-maps
          (for [board boards]
            [thumbnail
             {:key       (:image/checksum board)
              :board     board
              :selected  (= board (:canvas/map workspace))
              :on-select (fn [] (dispatch :canvas/change-map (:db/id board)))
              :on-remove (fn []
                           (.delete (.-images store) (:image/checksum board))
                           (dispatch :map/remove (:db/id board)))}])]])
      [:input
       {:type "file"
        :accept "image/*"
        :multiple true
        :on-change
        #(doseq [file (.. % -target -files)]
           (load-image
            file
            (fn [{:keys [data filename img]}]
              (let [checks (checksum data)
                    record #js {:checksum checks :data data :created-at (.now js/Date)}
                    entity {:image/checksum checks
                            :image/name     filename
                            :image/width    (.-width img)
                            :image/height   (.-height img)}]
                (-> (.put (.-images store) record)
                    (.then
                     (fn [] (dispatch :map/create workspace entity))))))))}]]
     [:section
      [:header "Grid Options"]
      [:input
       {:type "number"
        :value (or size 0)
        :min 0
        :placeholder "Grid size"
        :on-change
        (fn [event]
          (let [value (.. event -target -value)]
            (dispatch :grid/change-size value)))}]
      [:div {:style {:margin-top 4}}
       [checkbox
        {:name "canvas/display-grid"
         :checked (or (:grid/show workspace) false)
         :on-change #(dispatch :grid/toggle)}
        "Display Grid"]]]
     [:section
      [:header "Theme"]
      [:div.options-canvas-theme
       (for [theme [:light :dark] :let [checked? (= theme (:canvas/theme workspace))]]
         [checkbox
          {:key theme
           :name (str "canvas/theme/" theme)
           :checked checked?
           :on-change
           (fn []
             (when (not checked?)
               (dispatch :canvas/toggle-theme)))}
          (string/capitalize (clojure.core/name theme))])]]
     [:section
      [:header "Lighting"]
      [:div.options-canvas-lighting
       (for [option [:bright :dim :dark]
             :let [checked (= option (:canvas/lighting workspace))]]
         [checkbox
          {:key option
           :name "canvas/lighting"
           :value option
           :checked checked
           :on-change #(dispatch :canvas/change-lighting option)}
          (string/capitalize (clojure.core/name option))])]]]))

(defn token [props]
  (let [{:keys [data workspace dispatch]} (uix/context state)
        {:keys [canvas/selected]} workspace
        idents (map :db/id selected)]
    [:div.options.options-token
     [:section
      (let [[match? value] (every-value? selected :element/name)]
        [:input
         {:type "text"
          :value (or value "")
          :placeholder (if match? "Label" "Multiple selected...")
          :maxLength 24
          :spellCheck "false"
          :on-change
          (fn [event]
            (let [value (.. event -target -value)]
              (dispatch :element/update idents :element/name value)))}])
      [:button {:type "button" :on-click #(dispatch :element/remove idents)} "♼"]
      [:button {:type "button" :on-click #(dispatch :selection/clear)} "×"]]
     [:section
      (let [initiative? (not (empty? (query/initiating data)))
            checked     (checked? :initiative/member? selected)]
        [checkbox
         {:name "token/initiative"
          :value "on"
          :checked checked
          :on-change #(dispatch :initiative/toggle idents %)}
         (case [initiative? checked]
           [true true] "In Initiative - Remove"
           ([true false] [true :indeterminate]) "In Initiative - Include"
           "Start Initiative")])]
     [:section
      [:header "Status"]
      [:div.options-token-flags
       (for [flag [:player :hidden :darkvision]]
         [checkbox
          {:key flag
           :name "token/flag"
           :value flag
           :checked (checked? #(contains? (:element/flags %) flag) selected)
           :on-change #(dispatch :element/flag idents flag %)}
          (string/capitalize (clojure.core/name flag))])]]
     [:section
      [:header "Size"]
      [:div.options-token-sizes
       (for [[name size] [[:tiny 2.5] [:small 5] [:medium 5] [:large 10] [:huge 15] [:gargantuan 20]]]
         [checkbox
          {:key name
           :name "token/size"
           :value name
           :checked (checked? #(= (:name (:token/size %)) name) selected)
           :on-change #(dispatch :token/change-size idents name size)}
          (string/capitalize (clojure.core/name name))])]]
     [:section
      [:header "Light"]
      [:div.options-token-lights
       (for [[name bright dim] light-sources]
         [checkbox
          {:key name
           :name "token/light"
           :value name
           :checked (checked? #(= [bright dim] (:token/light %)) selected)
           :on-change #(dispatch :token/change-light idents bright dim)}
          name])]]
     [:section
      [:header "Conditions"]
      [:div.options-token-flags
       (for [flag conditions]
         [checkbox
          {:key flag
           :name "token.flag"
           :value flag
           :checked (checked? #(contains? (:element/flags %) flag) selected)
           :on-change #(dispatch :element/flag idents flag %)}
          (string/capitalize (clojure.core/name flag))])]]
     [:section
      [:header "Aura"]
      (let [[match? value] (every-value? selected :aura/label)]
        [:div
         [:input
          {:type "text"
           :placeholder (if match? "Label" "Multiple selected...")
           :value (or value "")
           :maxLength 24
           :spellCheck "false"
           :on-change #(dispatch :aura/change-label idents (.. % -target -value))}]])
      [:div.options-token-auras
       (for [radius [0 10 15 20 30 60]]
         [checkbox
          {:key radius
           :name "token/aura-radius"
           :checked (checked? #(= (:aura/radius %) radius) selected)
           :value radius
           :on-change #(dispatch :aura/change-radius idents radius)}
          (if (= radius 0) "None" (str radius " ft."))])]]]))

(defn shape [props]
  (let [{:keys [workspace dispatch]} (uix/context state)
        {:keys [canvas/selected]} workspace
        idents (map :db/id selected)]
    [:div.options.options-shape
     [:section
      (let [[match? value] (every-value? selected :element/name)]
        [:input
         {:type "text"
          :value (or value "")
          :placeholder (if match? "Label" "Multiple selected...")
          :spellCheck "false"
          :on-change
          (fn [event]
            (let [value (.. event -target -value)]
              (dispatch :element/update idents :element/name value)))}])
      [:button {:type "button" :on-click #(dispatch :element/remove idents)} "♼"]
      [:button {:type "button" :on-click #(dispatch :selection/clear)} "×"]]
     [:section
      [:header "Color"]
      [:div.options-shape-colors
       (for [color colors]
         [checkbox
          {:key color
           :name "shape/color"
           :value color
           :checked (checked? #(= (:shape/color %) color) selected)
           :on-change
           (fn []
             (dispatch :element/update idents :shape/color color))}
          [:div {:style {:background-color color}}]])]]
     [:section
      [:header "Pattern"]
      [:div.options-shape-patterns
       (for [name [:solid :lines :circles :crosses :caps :waves]]
         [checkbox
          {:key name
           :name "shape/pattern"
           :value name
           :checked (checked? #(= (:shape/pattern %) name) selected)
           :on-change
           (fn []
             (dispatch :element/update idents :shape/pattern name))}
          (let [id (str "template-pattern-" (clojure.core/name name))]
            [:svg {:width "100%" :height "32px"}
             [:defs [pattern {:id id :name name}]]
             [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill (str "url(#" id ")")}]])])]]
     [:section
      [:header "Opacity"]
      (let [[match? value] (every-value? selected :shape/opacity)]
        [:input
         {:type "range"
          :style {:width "100%"}
          :value (or value 0.25)
          :on-change
          (fn [event]
            (let [value (.. event -target -value)]
              (dispatch :element/update idents :shape/opacity value)))
          :min 0
          :max 1
          :step 0.25}])]]))

(defn help []
  (let [{:keys [dispatch]} (uix/context state)]
    [:div.options
     [:div]
     [:section
      [:header "About"]
      [:p "ogre.tools is a free and open-source virtual table top for your "
       [:strong "Dungeons & Dragons 5th Edition"]
       " games."]
      [:p "Find a bug or want to explore the project code?"
       [:br]
       [:a {:href "https://github.com/samcf/ogre.tools" :target "_blank" :title "Repository home"}
        "https://github.com/samcf/ogre.tools"]]]
     [:section
      [:header "Local Data"]
      [:p "This application stores all uploaded images and work on your
           browser. You may restore the application to its factory defaults
           by clicking the button below."]
      [:p [:strong "All uploaded images and work will be permanently deleted."]]
      [:button {:type "button" :on-click #(dispatch :storage/reset) :style {:margin-top 8}}
       "Reset Data"]]]))

(defn type? [type entity]
  (= (:element/type entity) type))

(defn options []
  (let [{:keys [workspace]} (uix/context state)
        {:keys [canvas/mode canvas/selected]} workspace]
    (cond
      (= mode :canvas) [canvas]
      (= mode :help)   [help]
      (= mode :select)
      (cond
        (empty? selected) nil
        (every? (partial type? :token) selected) [token]
        (every? (partial type? :shape) selected) [shape]
        :else nil)
      :default nil)))
