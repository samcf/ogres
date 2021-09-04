(ns ogre.tools.core
  (:require [uix.core.alpha :as uix]
            [uix.dom.alpha :as uix.dom]
            [ogre.tools.root :refer [root]]))

(defn main []
  (.removeEventListener js/window "DOMContentLoaded" main)
  (let [element (.querySelector js/document "#root")]
    (uix.dom/render [root] element)))

(.addEventListener js/window "DOMContentLoaded" main)
