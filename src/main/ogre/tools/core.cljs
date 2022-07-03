(ns ogre.tools.core
  (:require [ogre.tools.form.core]
            [ogre.tools.env :as env]
            [ogre.tools.hooks :refer [use-query create-portal]]
            [ogre.tools.provider.events :as provider.events]
            [ogre.tools.provider.portal :as provider.portal]
            [ogre.tools.provider.state :as provider.state]
            [ogre.tools.provider.storage :as provider.storage]
            [ogre.tools.provider.window :as provider.window]
            [ogre.tools.render :refer [css error-boundary]]
            [ogre.tools.render.canvas :refer [canvas]]
            [ogre.tools.render.panel :refer [container]]
            [ogre.tools.render.tokens :refer [tokens]]
            [ogre.tools.render.toolbar :refer [toolbar]]
            [ogre.tools.render.workspaces :refer [workspaces]]
            [ogre.tools.session :as session]
            [ogre.tools.shortcut :as shortcut]
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
         [create-portal
          (fn [ref]
            [:div.layout-modal {:ref ref}]) :modal]
         [:div.layout-toolbar [toolbar]]
         [:div.layout-tokens [tokens]]
         [:div.layout-panel [container]]]
        [:div.root.layout attrs
         [:div.layout-canvas [canvas]]]))))

(defn ^{:private true} root [{:keys [path]}]
  [:<>
   [:> Helmet
    [:link {:rel "stylesheet" :href (str path "/reset.css")}]
    [:link {:rel "stylesheet" :href (str path "/ogre.tools.css")}]]
   [error-boundary
    [provider.events/provider
     [provider.state/provider
      [provider.storage/provider
       [:<>
        [provider.storage/handlers]
        [provider.window/provider]
        [shortcut/handlers]
        [session/handlers]
        [provider.portal/provider
         [layout]]]]]]]])

(defn main []
  (let [element (.querySelector js/document "#root")]
    (uix.dom/render [root {:path env/PATH}] element)))
