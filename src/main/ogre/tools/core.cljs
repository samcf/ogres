(ns ogre.tools.core
  (:require [ogre.tools.render :as render]
            [ogre.tools.render.layout :refer [layout]]
            [ogre.tools.txs :as txs]
            [uix.core.alpha :as uix]
            [uix.dom.alpha :as uix.dom]
            [dexie]))

(defn main []
  ;; Initialize the Dexie datastore which will serve to persist the application
  ;; state and image data in IndexedDB.
  (let [store (new dexie "ogre.tools")]
    (-> (.version store 1)
        (.stores #js {:images "checksum"}))

    ;; Render the application using a library called uix, an idiomatic
    ;; ClojureScript library for building ReactJS interfaces.
    (let [element (.querySelector js/document "#root")]
      (uix.dom/render
       [render/root {:data (txs/initial-data) :store store} [layout]]
       element))))

(.addEventListener js/window "DOMContentLoaded" main)
