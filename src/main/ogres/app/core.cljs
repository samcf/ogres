(ns ogres.app.core
  (:require [ogres.app.env :as env]
            [ogres.app.form.core]
            [ogres.app.layout :refer [layout]]
            [ogres.app.provider.events :as events]
            [ogres.app.provider.portal :as portal]
            [ogres.app.provider.state :as state]
            [ogres.app.provider.storage :as storage]
            [ogres.app.provider.window :as window]
            [ogres.app.render :refer [css error-boundary]]
            [ogres.app.session :as session]
            [ogres.app.shortcut :as shortcut]
            [react-helmet :refer [Helmet]]
            [uix.core.alpha :as uix]
            [uix.dom.alpha :as uix.dom]
            ["@dnd-kit/core" :refer [DndContext]
             :rename {DndContext dnd-context}])) 

(uix/add-transform-fn
 (fn [attrs]
   (if (:css attrs)
     (assoc (dissoc attrs :css) :class (css (:class attrs) (:css attrs)))
     attrs)))

(defn root []
  [:<>
   [:> Helmet
    [:link {:rel "stylesheet" :href (str env/PATH "/reset.css")}]
    [:link {:rel "stylesheet" :href (str env/PATH "/ogres.app.css")}]]
   [events/provider
    [state/provider
     [storage/provider
      [portal/provider
       [:<>
        [storage/handlers]
        [window/provider]
        [shortcut/handlers]
        [session/handlers]
        [error-boundary
         [:> dnd-context
          [layout]]]]]]]]])

(defn main []
  (let [element (.querySelector js/document "#root")]
    (uix.dom/render [root] element)))
