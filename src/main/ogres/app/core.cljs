(ns ogres.app.core
  (:require [ogres.app.env :as env]
            [ogres.app.form.core]
            [ogres.app.layout :refer [layout]]
            [ogres.app.provider.events :as provider.events]
            [ogres.app.provider.portal :as provider.portal]
            [ogres.app.provider.state :as provider.state]
            [ogres.app.provider.storage :as provider.storage]
            [ogres.app.provider.window :as provider.window]
            [ogres.app.render :refer [css error-boundary]]
            [ogres.app.session :as session]
            [ogres.app.shortcut :as shortcut]
            [react-helmet :refer [Helmet]]
            [uix.core.alpha :as uix]
            [uix.dom.alpha :as uix.dom])) 

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
   [provider.events/provider
    [provider.state/provider
     [provider.storage/provider
      [provider.portal/provider
       [:<>
        [provider.storage/handlers]
        [provider.window/provider]
        [shortcut/handlers]
        [session/handlers]
        [error-boundary
         [layout]]]]]]]])

(defn main []
  (let [element (.querySelector js/document "#root")]
    (uix.dom/render [root] element)))
