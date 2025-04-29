(ns ogres.app.provider.window
  (:require [goog.functions :refer [throttle]]
            [ogres.app.hooks :as hooks]
            [ogres.app.vec :as vec :refer [Vec2 Segment]]
            [uix.core :as uix :refer [defui $]]))

(defui ^:private register-bounds [_]
  (let [[retry set-retry] (uix/use-state 0)
        dispatch (hooks/use-dispatch)
        listener
        (uix/use-memo
         (fn []
           (throttle
            (fn [entries]
              (let [ent (aget entries 0)
                    rct (.-contentRect ent)
                    trg (.-target ent)
                    src (Vec2. (.-offsetLeft trg) (.-offsetTop trg))
                    dst (vec/shift src (.-width rct) (.-height rct))
                    bnd (vec/rnd (Segment. src dst))]
                (dispatch :user/change-bounds bnd))) 256)) [dispatch])
        [observer] (uix/use-state (js/ResizeObserver. listener))]
    (uix/use-effect
     (fn []
       (if-let [element (js/document.querySelector ".layout-scene")]
         (do (.observe observer element)
             (fn [] (.unobserve observer element)))
         (do (set-retry inc)
             (fn [])))) [observer retry])))

(defui listeners []
  ($ register-bounds))
