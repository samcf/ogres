(ns ogres.app.render.panel
  (:require [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render :refer [icon]]
            [ogres.app.form.render :as render]))

(defn- index-of [xs x]
  (reduce (fn [_ [i v]] (if (= v x) (reduced i))) nil
          (map-indexed vector xs)))

(def ^{:private true} panel-forms
  {:host [{:key :session    :label "Friends"    :icon "people-fill"}
          {:key :scenes     :label "Maps"       :icon "images"}
          {:key :tokens     :label "Tokens"     :icon "person-circle"}
          {:key :canvas     :label "Options"    :icon "wrench-adjustable-circle"}
          {:key :initiative :label "Initiative" :icon "hourglass-split"}
          {:key :help       :label "Help"       :icon "question-diamond"}]
   :conn [{:key :session    :label "Friends"    :icon "people-fill"}
          {:key :initiative :label "Initiative" :icon "hourglass-split"}
          {:key :help       :label "Help"       :icon "question-diamond"}]})

(def ^{:private true} query
  [[:local/type :default :conn]
   [:panel/expanded :default #{}]])

(defn container []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        forms    (panel-forms (:local/type result))
        found    (index-of (map :key forms) (first (:panel/expanded result)))]
    [:section.panel
     [:div.panel-forms
      (for [[index form] (map-indexed vector forms)
            :let [key (:key form)
                  distance (js/Math.abs (- found index))
                  expanded (contains? (:panel/expanded result) key)]]
        [:div.panel-form
         {:key key
          :css {(str "panel-form-" (name key)) true
                "panel-form--expanded"         expanded
                "panel-form--collapsed"        (not expanded)}
          :data-distance distance}
         [:div.panel-header
          {:on-click #(dispatch :interface/toggle-panel key)}
          [:<>
           [icon {:name (:icon form) :size 20}]
           [:div.panel-header-label (:label form)]
           (if-let [render-fn (render/header {:form key})]
             [render-fn])]]
         (if expanded
           [:div.panel-content
            [:div.panel-container
             [:div.panel-container-content [(render/form {:form key})]]
             (if-let [render-fn (render/footer {:form key})]
               [:div.panel-container-footer
                [render-fn]])]])])]]))
