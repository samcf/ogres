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

(def ^:private panel-data
  {:session    {:icon "people-fill" :label "Online options"}
   :scenes     {:icon "images" :label "Scene options"}
   :tokens     {:icon "person-circle" :label "Tokens"}
   :initiative {:icon "hourglass-split" :label "Initiative"}
   :help       {:icon "wrench-adjustable-circle" :label "Manage local data"}})

(def ^:private panel-forms
  {:host [:session :scenes :tokens :initiative :help]
   :conn [:session :tokens :initiative]})

(def ^:private components
  {:help       {:form help/form}
   :initiative {:form initiative/form :footer initiative/footer}
   :scenes     {:form scenes/form}
   :session    {:form session/form :footer session/footer}
   :tokens     {:form tokens/form :footer tokens/footer}})

(def ^:private query
  [[:user/type :default :conn]
   [:panel/selected :default :session]
   [:panel/expanded :default true]])

(defui container []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {type :user/type
         selected :panel/selected
         expanded :panel/expanded} result
        forms (panel-forms type)]
    ($ :.panel
      {:data-expanded expanded}
      (if expanded
        ($ :.panel-session
          ($ status/button)))
      ($ :ul.panel-tabs
        {:role "tablist"
         :aria-controls "form-panel"
         :aria-orientation "vertical"}
        (for [[key data] (map (juxt identity panel-data) forms)
              :let [selected (= selected key)]]
          ($ :li {:key key :role "tab" :aria-selected (and expanded selected)}
            ($ :label {:aria-label (:label data)}
              ($ :input
                {:type "radio"
                 :name "panel"
                 :value key
                 :checked (and expanded selected)
                 :on-change #(dispatch :user/select-panel key)})
              ($ icon {:name (:icon data) :size 20}))))
        ($ :li.panel-tabs-control
          {:role "tab" :on-click #(dispatch :user/toggle-panel)}
          ($ :button {:type "button" :aria-label "Collapse or expand"}
            ($ icon {:name (if expanded "chevron-double-right" "chevron-double-left")}))))
      (if expanded
        ($ :.form
          {:id "form-panel"
           :role "tabpanel"
           :data-form (name selected)}
          ($ :.form-container
            ($ :.form-content
              (if-let [component (get-in components [selected :form])]
                ($ :.form-body ($ component)))
              (if-let [component (get-in components [selected :footer])]
                ($ :.form-footer ($ component))))))))))
