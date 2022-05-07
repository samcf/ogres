(ns ogre.tools.core
  (:require [ogre.tools.errors :as errors]
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
            [ogre.tools.window :as window]
            [react-helmet :refer [Helmet]]
            [uix.core.alpha :as uix]
            [uix.dom.alpha :as uix.dom]
            ogre.tools.form.core))

(uix/add-transform-fn
 (fn [attrs]
   (if (:css attrs)
     (assoc (dissoc attrs :css) :class (css (:class attrs) (:css attrs)))
     attrs)))

(def layout-query
  {:pull
   [:root/loaded?
    :root/host?
    [:root/shortcuts? :default true]
    [:root/tooltips? :default true]]})

(defn layout []
  (let [[result] (use-query layout-query)
        {:root/keys [loaded? host? shortcuts? tooltips?]} result
        classes
        {:global--host       host?
         :global--guest      (not host?)
         :global--shortcuts  shortcuts?
         :global--tooltips   tooltips?}]
    (if loaded?
      (if host?
        [:div.layout {:css classes}
         [:div.layout-workspaces [workspaces]]
         [:div.layout-canvas [canvas]]
         [portal/create
          (fn [ref]
            [:div.layout-modal {:ref ref}]) :modal]
         [:div.layout-toolbar [toolbar]]
         [:div.layout-tokens [tokens]]
         [:div.layout-panel [container]]]
        [:div.layout {:css classes}
         [:div.layout-canvas [canvas]]]))))

(defn root [{:keys [path]}]
  [:<>
   [:> Helmet
    [:link {:rel "stylesheet" :href (str path "/reset.css")}]
    [:link {:rel "stylesheet" :href (str path "/fonts.css")}]
    [:link {:rel "stylesheet" :href (str path "/ogre.tools.css")}]]
   [errors/boundary
    [state/provider
     [storage/provider
      [:<>
       [storage/handlers]
       [window/provider]
       [session/handlers]
       [shortcut/handlers]
       [portal/provider
        [layout]]]]]]])

(defn main []
  (let [element (.querySelector js/document "#root")]
    (uix.dom/render [root {:path state/PATH}] element)))

(main)
