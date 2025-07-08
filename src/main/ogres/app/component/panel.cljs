(ns ogres.app.component.panel
  (:require [ogres.app.component :refer [icon]]
            [ogres.app.component.panel-data :as data]
            [ogres.app.component.panel-initiative :as initiative]
            [ogres.app.component.panel-lobby :as lobby]
            [ogres.app.component.panel-scene :as scene]
            [ogres.app.component.panel-tokens :as tokens]
            [ogres.app.component.panel-props :as props]
            [ogres.app.hooks :as hooks]
            [uix.core :refer [defui $]]))

(def ^:private query
  [[:user/host :default true]
   [:panel/selected :default :tokens]
   [:panel/expanded :default true]])

(def ^:private query-status
  [{:root/user [:user/host [:session/status :default :initial]]
    :root/session [:session/conns :session/room]}])

(def ^:private status-icon
  ($ icon {:name "globe-americas" :size 16}))

(defui status []
  (let [dispatch (hooks/use-dispatch)
        result (hooks/use-query query-status [:db/ident :root])
        {{host :user/host status :session/status} :root/user
         {code  :session/room
          conns :session/conns} :root/session} result
        connected (cond-> (count conns) host (inc))]
    (case status
      :initial      ($ :button.button {:on-click #(dispatch :session/request)} status-icon "Start online game")
      :connecting   ($ :button.button {:disabled true} status-icon "Connecting...")
      :connected    ($ :button.button {:disabled true} status-icon "Connected / " code " / [ " connected " ]")
      :disconnected ($ :button.button {:disabled true} status-icon "Disconnected")
      ($ :button.button {:disabled true} status-icon "Status not known"))))

(def ^:private data
  {:data       {:icon "wrench-adjustable-circle" :label "Manage local data"}
   :initiative {:icon "hourglass-split" :label "Initiative"}
   :lobby      {:icon "people-fill" :label "Online options"}
   :scene      {:icon "easel" :label "Scene options"}
   :tokens     {:icon "person-circle" :label "Token images"}
   :props      {:icon "images" :label "Prop images"}})

(def ^:private forms
  {true  [:tokens :scene :props :initiative :lobby :data]
   false [:tokens :initiative :lobby]})

(def ^:private components
  {:data       {:form data/panel}
   :initiative {:form initiative/panel :footer initiative/actions}
   :lobby      {:form lobby/panel :footer lobby/actions}
   :scene      {:form scene/panel}
   :tokens     {:form tokens/panel :footer tokens/actions}
   :props      {:form props/panel :footer props/actions}})

(defui ^:memo panel []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query)
        {host :user/host
         selected :panel/selected
         expanded :panel/expanded} result]
    ($ :.panel
      {:data-expanded expanded}
      (if expanded
        ($ :.panel-status
          ($ status)))
      ($ :ul.panel-tabs
        {:role "tablist"
         :aria-controls "form-panel"
         :aria-orientation "vertical"}
        (for [[key data] (map (juxt identity data) (forms host))
              :let [selected (= selected key)]]
          ($ :li.panel-tabs-tab
            {:key key :role "tab" :aria-selected (and expanded selected)}
            ($ :label {:aria-label (:label data)}
              ($ :input
                {:type "radio"
                 :name "panel"
                 :value key
                 :checked (and expanded selected)
                 :on-change #(dispatch :user/select-panel key)})
              ($ icon {:name (:icon data) :size 22}))))
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
