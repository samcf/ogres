(ns ogres.app.provider.release
  (:require [clojure.string :refer [split-lines]]
            [uix.core :refer [$ defui create-context use-effect use-state]]))

(def context (create-context))

(defui provider [{:keys [children]}]
  (let [[releases set-releases] (use-state nil)]
    (use-effect
     (fn []
       (-> (js/fetch "/releases.txt")
           (.then (fn [res] (.text res)))
           (.then (fn [txt] (set-releases (split-lines txt)))))) [])
    ($ (.-Provider context) {:value releases} children)))
