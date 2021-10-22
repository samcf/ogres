(ns ogre.tools.core
  (:require [ogre.tools.errors :as errors]
            [ogre.tools.render :refer [css]]
            [ogre.tools.render.layout :refer [layout]]
            [ogre.tools.shortcut :as shortcut]
            [ogre.tools.state :as state]
            [ogre.tools.storage :as storage]
            [ogre.tools.window :as window]
            [react-helmet :refer [Helmet]]
            [uix.core.alpha :as uix]
            [uix.dom.alpha :as uix.dom]))

(uix/add-transform-fn
 (fn [attrs]
   (if (:css attrs)
     (assoc (dissoc attrs :css) :class (css (:class attrs) (:css attrs)))
     attrs)))

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
       [shortcut/handlers]
       [layout]]]]]])

(defn main []
  (let [element (.querySelector js/document "#root")]
    (uix.dom/render [root {:path state/PATH}] element)))

(main)
