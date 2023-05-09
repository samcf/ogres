(ns ogres.app.render.pattern
  (:require [uix.core :refer [defui $]]))

(defui pattern-solid [{:keys [id color] :or {color "transparent"}}]
  ($ :pattern {:id id :pattern-units "userSpaceOnUse" :width 10 :height 10}
    ($ :rect {:x 0 :y 0 :width 10 :height 10 :fill color :stroke "none"})))

(defui pattern-lines [{:keys [id color] :or {color "currentColor"}}]
  ($ :pattern {:id id :pattern-units "userSpaceOnUse" :width 20 :height 20}
    ($ :path
      {:d "M 0,20 l 20,-20 M -5,5 l 10,-10 M 15,25 l 10,-10"
       :stroke color
       :stroke-width "1px"
       :stroke-linecap "square"})))

(defui pattern-circles [{:keys [id color] :or {color "currentColor"}}]
  ($ :pattern {:id id :pattern-units "userSpaceOnUse" :width 12 :height 12}
    ($ :circle {:cx 7 :cy 7 :r 3 :fill "none" :stroke color})))

(defui pattern-crosses [{:keys [id color] :or {color "currentColor"}}]
  ($ :pattern {:id id :pattern-units "userSpaceOnUse" :width 12 :height 12}
    ($ :path
      {:d "M 2.5,2.5l5,5M2.5,7.5l5,-5"
       :fill "transparent"
       :stroke color
       :stroke-width "1px"
       :shape-rendering "auto"
       :stroke-linecap "square"})))

(defui pattern-caps [{:keys [id color] :or {color "currentColor"}}]
  ($ :pattern {:id id :pattern-units "userSpaceOnUse" :width 12 :height 12}
    ($ :path
      {:d "M 2.5,7.5l2.5,-5l2.5,5"
       :fill "transparent"
       :stroke color
       :stroke-width 1
       :stroke-linecap "square"})))

(defui pattern-waves [{:keys [id color] :or {color "currentColor"}}]
  ($ :pattern {:id id :pattern-units "userSpaceOnUse" :width 10 :height 10}
    ($ :path
      {:d "M 0 5 c 1.25 -2.5 , 3.75 -2.5 , 5 0 c 1.25 2.5 , 3.75 2.5 , 5 0 M -5 5 c 1.25 2.5 , 3.75 2.5 , 5 0 M 10 5 c 1.25 -2.5 , 3.75 -2.5 , 5 0"
       :fill "transparent"
       :stroke color
       :stroke-width 2
       :stroke-linecap "square"})))

(defui pattern [{:keys [name] :as props}]
  (let [fns {:caps    pattern-caps
             :circles pattern-circles
             :crosses pattern-crosses
             :lines   pattern-lines
             :solid   pattern-solid
             :waves   pattern-waves}]
    (if-let [component (fns name)]
      ($ component props))))
