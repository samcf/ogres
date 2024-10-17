(ns ogres.app.provider.portal
  (:refer-clojure :exclude [use])
  (:require [uix.core :as uix :refer [defui $]]
            [uix.dom :as dom]))

(def ^:private context (uix/create-context))

(defui provider
  "Context provider that allows the user to define portal targets and associate
   them with a label; other components within the provider can then render
   into these targets by label."
  [{:keys [children]}]
  (let [[portals set-portals] (uix/use-state {})
        register              (uix/use-callback
                               (fn [name ref]
                                 (set-portals
                                  (fn [portals]
                                    (assoc portals name ref)))) [])]
    ($ context {:value [portals register]} children)))

(defui create
  "Registers the DOM element returned by `f` as a portal labeled by `label`.
   This element will be used as the target node for rendering the contents
   of `use`."
  [{:keys [name children]}]
  (let [[_ register] (uix/use-context context)
        callback     (uix/use-callback
                      (fn [node]
                        (register name node)) [register name])]
    (children {:ref callback})))

(defui use
  "Renders the given children into the portal target associated with the given
   label; if the label is nil, renders the contents normally."
  [{:keys [name children]}]
  (let [[portals _] (uix/use-context context)
        node        (get portals name)]
    (if (some? node)
      (dom/create-portal children node)
      children)))
