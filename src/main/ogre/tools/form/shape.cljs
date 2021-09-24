(ns ogre.tools.form.shape
  (:require [clojure.string :refer [capitalize]]
            [ogre.tools.form.render :refer [form]]
            [ogre.tools.form.util :refer [checked? every-value?]]
            [ogre.tools.query :as query]
            [ogre.tools.render :refer [button checkbox]]
            [ogre.tools.render.icon :refer [icon]]
            [ogre.tools.render.pattern :refer [pattern]]
            [ogre.tools.state :refer [state]]
            [uix.core.alpha :as uix]))

(def colors
  ["#182125" "#f2f2f2" "#f44336" "#e91e63"
   "#9c27b0" "#673ab7" "#3f51b5" "#2196f3"
   "#00bcd4" "#009688" "#4caf50" "#cddc39"
   "#ffeb3b" "#ffc107" "#ff9800"])

(defn shape [props]
  (let [{:keys [workspace dispatch]} (uix/context state)
        {:keys [canvas/selected]} workspace
        idents (map :db/id selected)]
    [:<>
     [:section
      [:fieldset.group
       (let [[match? value] (every-value? selected :element/name)]
         [:input
          {:type "text"
           :value (or value "")
           :placeholder (if match? "Label" "Multiple selected...")
           :spellCheck "false"
           :style {:flex 1}
           :on-change
           (fn [event]
             (let [value (.. event -target -value)]
               (dispatch :element/update idents :element/name value)))}])
       [button {:style {:flex 0} :on-click #(dispatch :element/remove idents)}
        [icon {:name :trash :size 16}]]]]
     [:section
      [:legend "Color"]
      [:fieldset.form-shape-colors
       (for [color colors]
         ^{:key color}
         [checkbox
          {:checked (checked? #(= (:shape/color %) color) selected)
           :on-change #(dispatch :element/update idents :shape/color color)}
          [:div {:style {:background-color color}}]])]]
     [:section
      [:legend "Pattern"]
      [:fieldset.table.form-shape-patterns
       (for [name [:solid :lines :circles :crosses :caps :waves]]
         ^{:key name}
         [checkbox
          {:checked (checked? #(= (:shape/pattern %) name) selected)
           :on-change #(dispatch :element/update idents :shape/pattern name)}
          (let [id (str "template-pattern-" (clojure.core/name name))]
            [:svg {:width "100%" :height "32px"}
             [:defs [pattern {:id id :name name}]]
             [:rect {:x 0 :y 0 :width "100%" :height "100%" :fill (str "url(#" id ")")}]])])]]
     [:section
      [:legend "Opacity"]
      (let [[match? value] (every-value? selected :shape/opacity)]
        [:input
         {:type "range"
          :style {:width "100%"}
          :value (or value 0.25)
          :on-change
          (fn [event]
            (let [value (.. event -target -value)]
              (dispatch :element/update idents :shape/opacity value)))
          :min 0
          :max 1
          :step 0.25}])]]))

(defn container []
  (let [{:keys [workspace]} (uix/context state)]
    [:<>
     [:section [:header "Shape Options"]]
     (let [selected (:canvas/selected workspace)]
       (if (and (seq selected) (every? #(= :shape (:element/type %)) selected))
         [shape]
         [:section
          [:div.prompt
           [icon {:name :triangle :size 48}]
           [:br] "Configure shapes by selecting"
           [:br] "one or more of them from the canvas"]]))]))

(defmethod form :shape []
  container)
