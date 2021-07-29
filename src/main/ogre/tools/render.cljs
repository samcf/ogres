(ns ogre.tools.render
  (:require [datascript.core :as ds]
            [ogre.tools.utils :refer [css]]
            [react-draggable :as draggable]
            [rum.core :as rum]))

(defn workspaces [data]
  (->> (ds/q '[:find ?ws ?tx :where [_ :viewer/workspaces ?ws ?tx]] data)
       (sort-by second)
       (map first)
       (map #(ds/entity data %))))

(defn viewer [data]
  (ds/entity data (:e (first (ds/datoms data :aevt :viewer/workspace)))))

(rum/defcontext *context*)

(rum/defc provider [value children]
  (rum/bind-context [*context* value] children))

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
       (when-let [url (:map/imageURL element)]
         [:image {:x 0 :y 0 :href url}])
       children]
      nil)))

(rum/defc options [props & children]
  (let [{:keys [element dispatch]} props
        update (fn [attr value]
                 (dispatch :element/update (:db/id element) attr value))]

    (case (:element/type element)
      :workspace
      [:div.options
       [:div.options-header
        [:div.options-title "Workspace & Map Settings"]
        [:button {:type "button"
                  :onClick #(dispatch :view/close (:db/id element))} "×"]]
       [:label
        [:input
         {:type "text"
          :placeholder "Workspace name"
          :value (or (:element/name element) "")
          :onChange (fn [event]
                      (let [value (.. event -target -value)]
                        (update :element/name value)))}]]
       [:label
        [:div "Upload image"]
        [:input
         {:type "file" :accept "image/*" :name "board"
          :onChange (fn [event] nil
                      (let [file   (aget (.. event -target -files) 0)
                            reader (new js/FileReader)]
                        (.readAsDataURL reader file)
                        (.addEventListener
                         reader "load"
                         (fn [event]
                           (let [result (.. event -target -result)]
                             (update :map/imageURL result))))))}]]]
      nil)))

(rum/defc layout [props & children]
  (rum/with-context [[state dispatch] *context*]
    (let [viewer (viewer state)
          current (:viewer/workspace viewer)]
      [:div.table
       [:div.command
        (for [c ["S" "P" "M"]]
          [:button {:type "button" :key c} c])
        [:button
         {:type "button"
          :onClick #(dispatch :view/toggle-settings (:db/id current))} "B"]]
       [:div.content
        [:div.workspaces
         (for [workspace (workspaces state) :let [id (:db/id workspace)]]
           [:div {:key id}
            [:label
             {:className (css {:active (= current workspace)})}
             [:input
              {:type "radio"
               :name "window"
               :value id
               :checked (= current workspace)
               :onChange #(dispatch :workspace/change id)}]
             (let [name (clojure.string/trim (or (:element/name workspace) ""))]
               (if (empty? name) [:em "Unnamed Workspace"] [:span name]))]
            [:button {:type "button" :onClick #(dispatch :workspace/remove id)} "×"]])
         [:button {:type "button" :onClick #(dispatch :workspace/create)} "+"]]
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
  (let [{:keys [data transact]}  props
        [state update!] (rum/use-state data)
        handler         (fn [event & args]
                          (let [next (ds/db-with state (apply transact state event args))]
                            (update! next)))]
    (provider [state handler] (layout {}))))
