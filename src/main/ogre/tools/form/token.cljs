(ns ogre.tools.form.token
  (:require [clojure.string :refer [capitalize]]
            [ogre.tools.form.render :refer [form]]
            [ogre.tools.form.util :refer [checked? every-value?]]
            [ogre.tools.query :as query]
            [ogre.tools.render :refer [button checkbox]]
            [ogre.tools.render.icon :refer [icon]]
            [ogre.tools.state :refer [state]]
            [uix.core.alpha :as uix]))

(def light-sources
  [["None" 0 0] ["Candle" 5 5] ["Torch" 20 20] ["Lamp" 15 30] ["Lantern" 30 30]])

(def conditions
  [:blinded :charmed :defeaned
   :exhausted :frightened :grappled
   :incapacitated :invisible :paralyzed
   :petrified :poisoned :prone
   :restrained :stunned :unconscious])

(defn label [keyword]
  (capitalize (name keyword)))

(defn token [props]
  (let [{:keys [data workspace dispatch]} (uix/context state)
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
           :maxLength 24
           :spellCheck "false"
           :style {:flex 1 :width "100%"}
           :on-change
           (fn [event]
             (let [value (.. event -target -value)]
               (dispatch :element/update idents :element/name value)))}])
       [button {:style {:flex 0} :on-click #(dispatch :element/remove idents)}
        [icon {:name "trash" :size 16}]]]]
     [:section
      (let [initiative? (not (empty? (query/initiating data)))
            checked     (checked? :initiative/member? selected)]
        [checkbox
         {:checked checked
          :on-change #(dispatch :initiative/toggle idents %)}
         (case [initiative? checked]
           [true true] "In Initiative - Remove"
           ([true false] [true :indeterminate]) "In Initiative - Include"
           "Start Initiative")])]
     [:section
      [:legend "Status"]
      [:fieldset.table
       (for [flag [:player :hidden :darkvision]]
         [checkbox
          {:key flag
           :name "token/flag"
           :value flag
           :checked (checked? #(contains? (:element/flags %) flag) selected)
           :on-change #(dispatch :element/flag idents flag %)}
          [label flag]])]]
     [:section
      [:legend "Size"]
      [:fieldset.table
       (for [[name size] [[:tiny 2.5] [:small 5] [:medium 5] [:large 10] [:huge 15] [:gargantuan 20]]]
         [checkbox
          {:key name
           :name "token/size"
           :value name
           :checked (checked? #(= (:name (:token/size %)) name) selected)
           :on-change #(dispatch :token/change-size idents name size)}
          [label name]])]]
     [:section
      [:legend "Light"]
      [:fieldset.table
       (for [[name bright dim] light-sources]
         [checkbox
          {:key name
           :name "token/light"
           :value name
           :checked (checked? #(= [bright dim] (:token/light %)) selected)
           :on-change #(dispatch :token/change-light idents bright dim)}
          name])]]
     [:section
      [:legend "Conditions"]
      [:fieldset.table
       (for [flag conditions]
         [checkbox
          {:key flag
           :name "token.flag"
           :value flag
           :checked (checked? #(contains? (:element/flags %) flag) selected)
           :on-change #(dispatch :element/flag idents flag %)}
          [label flag]])]]
     [:section
      [:legend "Aura"]
      (let [[match? value] (every-value? selected :aura/label)]
        [:fieldset
         [:input
          {:type "text"
           :placeholder (if match? "Label" "Multiple selected...")
           :value (or value "")
           :maxLength 24
           :spellCheck "false"
           :on-change #(dispatch :aura/change-label idents (.. % -target -value))}]])
      [:fieldset.table
       (for [radius [0 10 15 20 30 60]]
         [checkbox
          {:key radius
           :name "token/aura-radius"
           :checked (checked? #(= (:aura/radius %) radius) selected)
           :value radius
           :on-change #(dispatch :aura/change-radius idents radius)}
          (if (= radius 0) "None" (str radius " ft."))])]]]))

(defn container []
  (let [{:keys [workspace]} (uix/context state)]
    [:<>
     [:section [:header "Token Options"]]
     (let [selected (:canvas/selected workspace)]
       (if (and (seq selected) (every? #(= :token (:element/type %)) selected))
         [token]
         [:section
          [:div.prompt
           [icon {:name :person-circle :size 48}]
           [:br] "Configure tokens by selecting"
           [:br] "one or more of them from the canvas"]]))]))

(defmethod form :token []
  container)
