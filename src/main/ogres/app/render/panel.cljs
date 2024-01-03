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
          {:key :help       :label "Help"       :icon "question-diamond"}]
   :conn [{:key :session    :label "Friends"    :icon "people-fill"}
          {:key :tokens     :label "Tokens"     :icon "person-circle"}
          {:key :initiative :label "Initiative" :icon "hourglass-split"}
          {:key :help       :label "Help"       :icon "question-diamond"}]})

(def ^:private components
  {:help       {:form help/form}
   :initiative {:form initiative/form :footer initiative/footer}
   :scenes     {:form scenes/form}
   :session    {:form session/form :footer session/footer}
   :tokens     {:form tokens/form :footer tokens/footer}})

(def ^:private query
  [[:local/type :default :conn]
   [:panel/selected :default :session]])

(defui container []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        {type :local/type
         selected :panel/selected} result
        forms    (panel-forms type)]
    ($ :nav.panel
      ($ :.panel-session
        ($ status/button))
      ($ :ul.panel-tabs
        (for [form forms :let [name (:key form) selected (= selected name)]]
          ($ :li
            {:key name :data-selected selected :on-click #(dispatch :local/select-panel name)}
            ($ icon {:name (:icon form) :size 20}))))
      (let [current (:key (first (filter (comp #{selected} :key) forms)))]
        ($ :.form {:data-form (name current)}
          ($ :.form-container
            ($ :.form-content
              (if-let [component (get-in components [current :form])]
                ($ :.form-body ($ component)))
              (if-let [component (get-in components [current :footer])]
                ($ :.form-footer ($ component))))))))))
