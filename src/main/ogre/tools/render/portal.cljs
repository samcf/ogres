(ns ogre.tools.render.portal
  (:refer-clojure :exclude [use])
  (:require [uix.core.alpha :as uix :refer [defcontext]]
            [uix.dom.alpha :refer [create-portal]]))

(defcontext portal)

(defn provider
  "Context provider that allows the user to define portal targets and associate
   them with a label; other components within the provider can then render
   into these targets by label."
  [& children]
  (let [portals  (uix/state {})
        register (uix/callback
                  (fn [label ref]
                    (swap! portals assoc label ref)) [])]
    (uix/context-provider [portal [@portals register]] children)))

(defn create
  "Registers the DOM element returned by `f` as a portal labeled by `label`.
   This element will be used as the target node for rendering the contents
   of `use`."
  [f label]
  (let [[_ register] (uix/context portal)
        callback     (uix/callback
                      (fn [node]
                        (register label node)) [])]
    (f callback)))

(defn use
  "Renders the given children into the portal target associated with the given
   label; if the label is nil, renders the contents normally."
  [{:keys [label]} children]
  (let [[portals _] (uix/context portal)
        node        (get portals label)]
    (if (not (nil? node))
      (create-portal children node)
      children)))
