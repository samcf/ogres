(ns ogres.app.provider.window
  (:require [goog.functions :refer [throttle]]
            [ogres.app.hooks :as hooks]
            [ogres.app.segment :refer [Segment]]
            [ogres.app.vec :as vec :refer [Vec2]]
            [uix.core :as uix :refer [defui $]]))

(defui ^:private register-bounds [{:keys [ref]}]
  (let [dispatch (hooks/use-dispatch)
        listener
        (uix/use-memo
         (fn []
           (throttle
            (fn [entries]
              (let [ent (aget entries 0)
                    rct (.-contentRect ent)
                    trg (.-target ent)
                    src (Vec2. (.-offsetLeft trg) (.-offsetTop trg))
                    dst (vec/shift src (.-width rct) (.-height rct))]
                (dispatch :user/change-bounds (vec/rnd (Segment. src dst))))) 256)) [dispatch])
        [observer] (uix/use-state (js/ResizeObserver. listener))]
    (uix/use-effect
     (fn []
       (.observe observer ref)
       (fn [] (.unobserve observer ref))) [ref observer])))

(def context (uix/create-context))

(defui provider [{:keys [children]}]
  (let [[ref set-ref] (uix/use-state nil)]
    ($ context {:value set-ref}
      (if (some? ref)
        ($ register-bounds {:ref ref}))
      children)))
