(ns ogre.tools.core
  (:require [ogre.tools.env :as env]
            [ogre.tools.form.core]
            [ogre.tools.layout :refer [layout]]
            [ogre.tools.provider.events :as provider.events]
            [ogre.tools.provider.portal :as provider.portal]
            [ogre.tools.provider.state :as provider.state]
            [ogre.tools.provider.storage :as provider.storage]
            [ogre.tools.provider.window :as provider.window]
            [ogre.tools.render :refer [css]]
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

(defn root []
  [:<>
   [:> Helmet
    [:link {:rel "stylesheet" :href (str env/PATH "/reset.css")}]
    [:link {:rel "stylesheet" :href (str env/PATH "/ogre.tools.css")}]]
   [provider.events/provider
    [provider.state/provider
     [provider.storage/provider
      [provider.portal/provider
       [:<>
        [provider.storage/handlers]
        [provider.window/provider]
        [shortcut/handlers]
        [session/handlers]
        [layout]]]]]]])

(defn main []
  (let [element (.querySelector js/document "#root")]
    (uix.dom/render [root] element)))
