(ns ogres.app.render.panel
  (:require [ogres.app.hooks :refer [use-dispatch use-query]]
            [ogres.app.form.session :as session]
            [ogres.app.form.scenes  :as scenes]
            [ogres.app.form.tokens  :as tokens]
            [ogres.app.form.options :as options]
            [ogres.app.form.help    :as help]
            [ogres.app.form.initiative :as initiative]
            [ogres.app.render :refer [css icon]]
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
   :session    {:header session/header :form session/form :footer session/footer}
   :tokens     {:form tokens/form :footer tokens/footer}})

(def ^:private query
  [[:local/type :default :conn]
   [:panel/expanded :default #{}]])

(defui container []
  (let [dispatch (use-dispatch)
        result   (use-query query)
        forms    (panel-forms (:local/type result))]
    ($ :section.panel
      ($ :div.panel-forms
        (for [form forms
              :let [key (:key form)
                    expanded (contains? (:panel/expanded result) key)]]
          ($ :div.panel-form
            {:key key
             :class (css {(str "panel-form-" (name key)) true
                          "panel-form--expanded" expanded
                          "panel-form--collapsed" (not expanded)})}
            ($ :div.panel-header
              {:on-click #(dispatch :local/toggle-panel key)}
              ($ :<>
                ($ icon {:name (:icon form) :size 20})
                ($ :div.panel-header-label (:label form))
                (if-let [component (-> components key :header)]
                  ($ component))))
            (if expanded
              ($ :div.panel-content
                ($ :div.panel-container
                  (if-let [component (-> components key :form)]
                    ($ :div.panel-container-content
                      ($ component)))
                  (if-let [component (-> components key :footer)]
                    ($ :div.panel-container-footer
                      ($ component))))))))))))
