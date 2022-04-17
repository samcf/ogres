(ns ogre.tools.render.modal
  (:require [uix.core.alpha :as uix :refer [defcontext]]
            [uix.dom.alpha :refer [create-portal]]))

(defcontext context)

(defn provider [props child]
  (uix/context-provider [context (:ref props)] child))

(defn modal [child]
  (let [element (uix/context context)]
    (create-portal child @element)))
