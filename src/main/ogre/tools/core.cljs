(ns ogre.tools.core
  (:require [uix.core.alpha :as uix]
            [uix.dom.alpha :as uix.dom]
            [dexie]
            [ogre.tools.render.root :refer [root]]
            [ogre.tools.txs :as txs]))

(defn main []
  ;; Initialize the Dexie datastore which will serve to persist the application
  ;; state and image data in IndexedDB.
  (let [store (new dexie "ogre.tools")]
    (.stores (.version store 1) #js {:images "checksum" :states "++id"})
    (.open store)

    ;; Render the application using a library called uix, an idiomatic
    ;; ClojureScript library for building ReactJS interfaces.
    (let [element (.querySelector js/document "#root")]
      (uix.dom/render
       [root {:data (txs/initial-data) :store store}]
       element))))

(.addEventListener js/window "DOMContentLoaded" main)
