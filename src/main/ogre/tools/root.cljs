(ns ogre.tools.root
  (:require  [ogre.tools.errors :as errors]
             [ogre.tools.render.layout :refer [layout]]
             [ogre.tools.state :as state]
             [ogre.tools.storage :as storage]
             [ogre.tools.window :as window]))

(defn root []
  [state/provider
   [storage/provider
    [errors/boundary
     [:<>
      [storage/unmarshaller]
      [storage/marshaller]
      [storage/handlers]
      [window/provider]
      [layout]]]]])
