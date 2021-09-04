(ns ogre.tools.core
  (:require [ogre.tools.errors :as errors]
            [ogre.tools.render.layout :refer [layout]]
            [ogre.tools.state :as state]
            [ogre.tools.storage :as storage]
            [ogre.tools.window :as window]
            [uix.dom.alpha :as uix.dom]))

(defn root []
  [errors/boundary
   [state/provider
    [storage/provider
     [:<>
      [storage/handlers]
      [window/provider]
      [layout]]]]])

(defn main []
  (let [element (.querySelector js/document "#root")]
    (uix.dom/render [root] element)))

(defonce _
  (do (.addEventListener js/window "DOMContentLoaded" main)))
