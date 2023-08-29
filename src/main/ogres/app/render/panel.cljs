(ns ogres.app.render.panel
  (:require [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.form.session :as session]
            [ogres.app.form.scenes  :as scenes]
            [ogres.app.form.tokens  :as tokens]
            [ogres.app.form.options :as options]
            [ogres.app.form.help    :as help]
            [ogres.app.form.initiative :as initiative]
            [ogres.app.render :refer [icon]]
            [uix.core :refer [defui $]]))

(def ^:private panel-forms
  {:host [{:key :session    :label "Friends"    :icon "people-fill"}
          {:key :scenes     :label "Scenes"     :icon "images"}
          {:key :tokens     :label "Tokens"     :icon "person-circle"}
          {:key :scene      :label "Options"    :icon "wrench-adjustable-circle"}
          {:key :initiative :label "Initiative" :icon "hourglass-split"}
          {:key :help       :label "Help"       :icon "question-diamond"}]
   :conn [{:key :session    :label "Friends"    :icon "people-fill"}
          {:key :tokens     :label "Tokens"     :icon "person-circle"}
          {:key :initiative :label "Initiative" :icon "hourglass-split"}
          {:key :help       :label "Help"       :icon "question-diamond"}]})

(def ^:private components
  {:scene      {:form options/form}
   :help       {:form help/form}
   :initiative {:form initiative/form :footer initiative/footer}
   :scenes     {:form scenes/form :footer scenes/footer}
   :session    {:form session/form :footer session/footer}
   :tokens     {:form tokens/form :footer tokens/footer}})

(def ^:private query
  [[:local/type :default :conn]
   [:panel/expanded :default #{}]])

(defui container []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        forms    (panel-forms (:local/type result))]
    ($ :nav.panel
      ($ :ul.forms
        (for [form forms :let [key (:key form) expanded (contains? (:panel/expanded result) key)]]
          ($ :li {:key key :class "form" :data-form (name key) :data-expanded expanded}
            ($ :div.form-header
              {:on-click #(dispatch :local/toggle-panel key)}
              ($ :<>
                ($ icon {:name (:icon form) :size 20})
                ($ :div.form-label (:label form))
                ($ :div.form-chevron
                  ($ icon {:name (if expanded "chevron-double-up" "chevron-double-down") :size 18}))))
            (if expanded
              ($ :div.form-container
                ($ :div.form-content
                  (if-let [component (-> components key :form)]
                    ($ :div.form-body ($ component)))
                  (if-let [component (-> components key :footer)]
                    ($ :div.form-footer ($ component))))))))))))
