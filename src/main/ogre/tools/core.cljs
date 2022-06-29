(ns ogre.tools.core
  (:require [datascript.core :as ds]
            [ogre.tools.form.core]
            [ogre.tools.env :as env]
            [ogre.tools.errors :as errors]
            [ogre.tools.events :as events]
            [ogre.tools.render :refer [css]]
            [ogre.tools.render.canvas :refer [canvas]]
            [ogre.tools.render.panel :refer [container]]
            [ogre.tools.render.portal :as portal]
            [ogre.tools.render.tokens :refer [tokens]]
            [ogre.tools.render.toolbar :refer [toolbar]]
            [ogre.tools.render.workspaces :refer [workspaces]]
            [ogre.tools.session :as session]
            [ogre.tools.shortcut :as shortcut]
            [ogre.tools.state :as state :refer [use-query]]
            [ogre.tools.storage :as storage]
            [ogre.tools.txs :as txs]
            [ogre.tools.window :as window]
            [react-helmet :refer [Helmet]]
            [uix.core.alpha :as uix]
            [uix.dom.alpha :as uix.dom]))
 
(uix/add-transform-fn
 (fn [attrs]
   (if (:css attrs)
     (assoc (dissoc attrs :css) :class (css (:class attrs) (:css attrs)))
     attrs)))

(def ^{:private true} query
  [[:local/type :default :conn]
   [:local/loaded? :default false]
   [:local/shortcuts? :default true]
   [:local/tooltips? :default true]])

(defn ^{:private true} layout []
  (let [result (use-query query)
        {:local/keys [loaded? type shortcuts? tooltips?]} result
        attrs {:data-view-type      (name type)
               :data-show-shortcuts shortcuts?
               :data-show-tooltips  tooltips?}]
    (if loaded?
      (if (or (= type :host) (= type :conn))
        [:div.root.layout attrs
         (if (= type :host)
           [:div.layout-workspaces [workspaces]])
         [:div.layout-canvas [canvas]]
         [portal/create
          (fn [ref]
            [:div.layout-modal {:ref ref}]) :modal]
         [:div.layout-toolbar [toolbar]]
         [:div.layout-tokens [tokens]]
         [:div.layout-panel [container]]]
        [:div.root.layout attrs
         [:div.layout-canvas [canvas]]]))))

(def ^{:private true} context-query
  [:entity/key {:local/window [:entity/key {:window/canvas [:entity/key]}]}])

(defn ^{:private true} handle-txs
  "Subscribes to all events and creates DataScript transactions for those
   registered as event handlers in `ogre.tools.txs/transact`."
  []
  (let [conn     (uix/context state/context)
        dispatch (events/use-dispatch)
        result   (use-query context-query)
        context  {:local  (:entity/key result)
                  :window (:entity/key (:local/window result))
                  :canvas (:entity/key (:window/canvas (:local/window result)))}]
    (events/subscribe!
     (fn [event]
       (let [context (assoc context :data @conn :event (:topic event))
             tx-data (apply txs/transact context (:args event))]
         (if (seq tx-data)
           (let [report (ds/transact! conn tx-data)] 
             (dispatch :tx/commit report))))) [context]) nil))

(defn ^{:private true} root [{:keys [path]}]
  [:<>
   [:> Helmet
    [:link {:rel "stylesheet" :href (str path "/reset.css")}]
    [:link {:rel "stylesheet" :href (str path "/ogre.tools.css")}]]
   [errors/boundary
    [events/provider
     [state/provider
      [storage/provider
       [:<>
        [handle-txs]
        [storage/handlers]
        [window/provider]
        [shortcut/handlers]
        [session/handlers]
        [portal/provider
         [layout]]]]]]]])

(defn ^{:private true} main []
  (let [element (.querySelector js/document "#root")]
    (uix.dom/render [root {:path env/PATH}] element)))

(main)
