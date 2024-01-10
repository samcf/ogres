(ns ogres.app.render.panel
  (:require [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.render.status :as status]
            [ogres.app.form.session :as session]
            [ogres.app.form.scenes  :as scenes]
            [ogres.app.form.tokens  :as tokens]
            [ogres.app.form.help    :as help]
            [ogres.app.form.initiative :as initiative]
            [ogres.app.render :refer [icon]]
            [uix.core :refer [defui $]]))

(def ^:private panel-forms
  {:host [{:key :session    :label "Friends"    :icon "people-fill"}
          {:key :scenes     :label "Scene"      :icon "images"}
          {:key :tokens     :label "Tokens"     :icon "person-circle"}
          {:key :initiative :label "Initiative" :icon "hourglass-split"}
          {:key :help       :label "Help"       :icon "wrench-adjustable-circle"}]
   :conn [{:key :session    :label "Friends"    :icon "people-fill"}
          {:key :tokens     :label "Tokens"     :icon "person-circle"}
          {:key :initiative :label "Initiative" :icon "hourglass-split"}
          {:key :help       :label "Help"       :icon "wrench-adjustable-circle"}]})

(def ^:private components
  {:help       {:form help/form}
   :initiative {:form initiative/form :footer initiative/footer}
   :scenes     {:form scenes/form}
   :session    {:form session/form :footer session/footer}
   :tokens     {:form tokens/form :footer tokens/footer}})

(def ^:private query
  [[:local/type :default :conn]
   [:panel/selected :default :session]
   [:panel/expanded :default true]])

(defui container []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {type :local/type
         selected :panel/selected
         expanded :panel/expanded} result
        forms    (panel-forms type)]
    ($ :nav.panel {:data-expanded expanded}
      (if expanded
        ($ :.panel-session
          ($ status/button)))
      ($ :ul.panel-tabs
        (for [form forms :let [name (:key form) selected (= selected name)]]
          ($ :li {:key name}
            ($ :input
              {:id name
               :type "radio"
               :name "panel"
               :value name
               :hidden true
               :checked (and expanded selected)
               :on-change #(dispatch :local/select-panel name)})
            ($ :label {:for name}
              ($ icon {:name (:icon form) :size 20}))))
        ($ :li.panel-tabs-control
          {:on-click #(dispatch :local/toggle-panel)}
          ($ icon {:name (if expanded "chevron-double-right" "chevron-double-left")})))
      (if expanded
        (let [current (:key (first (filter (comp #{selected} :key) forms)))]
          ($ :.form {:data-form (name current)}
            ($ :.form-container
              ($ :.form-content
                (if-let [component (get-in components [current :form])]
                  ($ :.form-body ($ component)))
                (if-let [component (get-in components [current :footer])]
                  ($ :.form-footer ($ component)))))))))))
