(ns ogres.app.provider.release
  (:require [clojure.string :refer [split-lines]]
            [uix.core :as uix :refer [defui $]]))

(def context (uix/create-context))

(defui provider [{:keys [children]}]
  (let [[releases set-releases] (uix/use-state nil)]
    (uix/use-effect
     (fn []
       (-> (js/fetch "/releases.txt")
           (.then (fn [res] (.text res)))
           (.then (fn [txt] (set-releases (split-lines txt)))))) [])
    ($ context {:value releases} children)))
