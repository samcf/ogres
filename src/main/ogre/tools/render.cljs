(ns ogre.tools.render
  (:require [datascript.core :as ds]
            [dexie :as dexie]
            [ogre.tools.utils :refer [css]]
            [react-draggable :as draggable]
            [rum.core :as rum]))

(defn query-workspaces [data]
  (->> (ds/q '[:find ?ws ?tx :where [_ :viewer/workspaces ?ws ?tx]] data)
       (sort-by second)
       (map first)
       (map #(ds/entity data %))))

(defn query-maps [data]
  (->> (ds/q '[:find ?id ?tx :where [?id :map/id _ ?tx]] data)
       (sort-by second)
       (map first)
       (map #(ds/entity data %))))

(defn query-viewer [data]
  (ds/entity data (:e (first (ds/datoms data :aevt :viewer/workspace)))))

(defn load-image [file handler]
  (let [reader (new js/FileReader)]
    (.readAsDataURL reader file)
    (.addEventListener
     reader "load"
     (fn [event]
       (let [data  (.. event -target -result)
             image (new js/Image)
             url   (.createObjectURL js/URL file)]
         (.addEventListener
          image "load"
          (fn []
            (this-as img (handler {:data data :url url :img img}))))
         (set! (.-src image) url))))))

(rum/defcontext *context*)
(rum/defcontext *dexie*)

(defn camera [props & children]
  (let [{:keys [element dispatch]} props
        {:keys [position/x position/y]} element]
    [:> draggable
     {:position #js {:x 0 :y 0}
      :onStop (fn [event data]
                (let [ox (.-x data) oy (.-y data)]
                  (dispatch :camera/translate (:db/id element) (+ ox x) (+ oy y))))}
     [:g
      [:g {:transform (str "translate(" x ", " y ")")} children]
      [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill "transparent"}]]]))

(defn element [props & children]
  (let [{:keys [element dispatch]} props]
    (case (:element/type (:element props))
      :workspace
      [:g
       #_[:rect {:x 0 :y 0 :fill "url(#grid)" :width 640 :height 640}]
       #_[:path {:d "M 640.5 0 L 640.5 640.5 0 640.5" :fill "none" :stroke "black" :stroke-width 0.5}]
       (when-let [url (-> element :workspace/map :map/url)]
         [:image {:x 0 :y 0 :href url}])
       children]
      nil)))

(rum/defc options [props & children]
  (let [{:keys [element dispatch]} props]
    (rum/with-context [[data dispatch] *context*]
      (rum/with-context [dexie *dexie*]
        (case (:element/type element)
          :workspace
          [:div.options
           [:section.options-header
            [:label "Workspace Settings"]
            [:button {:type "button" :on-click #(dispatch :view/close (:db/id element))} "×"]]

           [:section
            [:label
             [:input
              {:type "text"
               :placeholder "Workspace name"
               :value (or (:element/name element) "")
               :on-change
               (fn [event]
                 (let [value (.. event -target -value)]
                   (dispatch :element/update (:db/id element) :element/name value)))}]]]

           (let [boards (query-maps data)]
             [:section
              (when (seq boards)
                [:div.options-boards
                 [:label "Select an existing map"]
                 [:div.options-boards-maps
                  (for [board boards :let [{:keys [db/id map/url]} board]]
                    [:img {:key id
                           :src url
                           :width 80
                           :height 80
                           :style {:object-fit "cover"}
                           :on-click #(dispatch :workspace/change-map (:db/id element) id)}])]])
              [:input
               {:type "file"
                :accept "image/*"
                :multiple true
                :on-change
                #(doseq [file (.. % -target -files)]
                   (load-image
                    file
                    (fn [{:keys [data url img]}]
                      (let [id (ds/squuid) w (.-width img) h (.-height img)]
                        (-> (.-images dexie) (.put  #js {:id (str id) :data data :created-at (.now js/Date)}))
                        (dispatch :map/create element {:map/id id :map/url url :map/width w :map/height h})))))}]])]
          nil)))))

(rum/defc layout [props & children]
  (rum/with-context [[data dispatch] *context*]
    (let [viewer (query-viewer data) current (:viewer/workspace viewer)]
      [:div.table
       [:div.command
        (for [c ["S" "P" "M"]]
          [:button {:type "button" :key c} c])
        [:button
         {:type "button"
          :on-click #(dispatch :view/toggle-settings (:db/id current))} "B"]]
       [:div.content
        [:div.workspaces
         (for [workspace (query-workspaces data) :let [id (:db/id workspace)]]
           [:div {:key id}
            [:label
             {:className (css {:active (= current workspace)})}
             [:input
              {:type "radio"
               :name "window"
               :value id
               :checked (= current workspace)
               :on-change #(dispatch :workspace/change id)}]
             (let [name (clojure.string/trim (or (:element/name workspace) ""))]
               (if (empty? name) [:em "Unnamed Workspace"] [:span name]))]
            [:button {:type "button" :on-click #(dispatch :workspace/remove id)} "×"]])
         [:button {:type "button" :on-click #(dispatch :workspace/create)} "+"]]
        [:div.workspace
         [:svg.canvas
          [:defs
           [:pattern {:id "grid" :width 64 :height 64 :patternUnits "userSpaceOnUse"}
            [:path {:d "M 64 0 L 0 0 0 64" :stroke "black" :stroke-width "1" :fill "none"}]]]
          (camera {:element current :dispatch dispatch}
                  (element {:element current}))]
         [:div.vignette]
         [:div.viewing
          (for [element (:workspace/viewing current)]
            (rum/with-key (options {:element current :dispatch dispatch})
              (:db/id element)))]]]])))

(rum/defc root [props & children]
  (let [{:keys [data transact]} props]
    (let [[state update!] (rum/use-state data)
          indexeddb       (new dexie "ogre.tools")
          handler         (fn [event & args]
                            (update! (fn [current]
                                       (ds/db-with current (apply transact state event args)))))]
      (-> (.version indexeddb 1)
          (.stores #js {:images "id"}))

      (rum/bind-context
       [*dexie* indexeddb]
       (rum/bind-context
        [*context* [state handler]]
        (layout {}))))))
