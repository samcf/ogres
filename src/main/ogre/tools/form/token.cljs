(ns ogre.tools.form.token
  (:require [clojure.string :refer [capitalize]]
            [ogre.tools.form.render :refer [form]]
            [ogre.tools.form.util :refer [checked? every-value?]]
            [ogre.tools.storage :refer [storage]]
            [ogre.tools.render :refer [button checkbox icon use-image]]
            [ogre.tools.state :refer [use-query]]
            [ogre.tools.image :as image]
            [uix.core.alpha :as uix]))

(defn linear [dx dy rx ry]
  (fn [n] (+ (* (/ (- n dx) (- dy dx)) (- ry rx)) rx)))

(def light-radii
  [[0 "None"] [15 "15 ft."] [20 "20 ft."] [30 "30 ft."] [40 "40 ft."] [50 "50 ft."]])

(def conditions
  [:blinded :charmed :defeaned
   :exhausted :frightened :grappled
   :incapacitated :invisible :paralyzed
   :petrified :poisoned :prone
   :restrained :stunned :unconscious])

(defn label [keyword]
  (capitalize (name keyword)))

(def query
  {:pull
   [{:root/stamps [:image/checksum]}
    {:root/canvas
     [:canvas/initiative
      {:canvas/selected
       [:db/id
        :canvas/_initiative
        :element/name
        :element/flags
        {:token/stamp [:image/checksum]}
        [:token/size :default {:name :medium :size 5}]
        [:token/light :default 15]
        :aura/label
        [:aura/radius :default 0]]}]}]})

(defn stamp [{:keys [checksum]}]
  (let [url (use-image checksum)]
    [:div.stamp-image
     {:style {:background-image (str "url(" url ")")}}]))

(defn upload [{:keys [ref]}]
  (let [[dispatch]      (use-query)
        {:keys [store]} (uix/context storage)]
    [:input
     {:type "file"
      :ref ref
      :accept "image/*"
      :multiple true
      :style {:display "none"}
      :on-change
      (fn [event]
        (let [files (.. event -target -files)]
          (doseq [file files]
            (.then
             (image/load file)
             (fn [{:keys [data filename element]}]
               (let [checksum (image/checksum data)
                     record   #js {:checksum checksum :data data :created-at (.now js/Date)}
                     entity   {:image/checksum checksum
                               :image/name     filename
                               :image/width    (.-width element)
                               :image/height   (.-height element)}]
                 (.then
                  (.put (.-images store) record)
                  (fn [] (dispatch :stamp/create entity)))))))))}]))

(defn token []
  (let [[data dispatch] (use-query query)
        display-tokens? (uix/state false)
        file-upload     (uix/ref)
        initiative      (-> data :root/canvas :canvas/initiative)
        selected        (-> data :root/canvas :canvas/selected)
        idents          (map :db/id selected)]
    [:<>
     [:section.form-token-profile
      (let [sample    (take 3 selected)
            samples   (count sample)
            size      ((linear 1 3 54 42) samples)
            icon-size ((linear 1 3 32 30) samples)]
        [:div.stamp-profile
         {:on-click #(swap! display-tokens? not)}
         (for [token sample :let [checksum (-> token :token/stamp :image/checksum)]]
           (let [attrs {:key (:db/id token) :type "button" :style {:width size :height size}}]
             (if checksum
               [:button attrs [stamp {:checksum checksum}]]
               [:button.stamp-default attrs [icon {:name "person-circle" :size icon-size}]])))])
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
      [button {:on-click #(dispatch :element/remove idents)}
       [icon {:name "trash" :size 16}]]
      (let [checked (checked? :canvas/_initiative selected)]
        [:div {:style {:grid-column "2 / 4"}}
         [checkbox
          {:checked checked
           :on-change #(dispatch :initiative/toggle idents %)}
          (case [(> (count initiative) 0) checked]
            [true true] "In Initiative - Remove"
            ([true false] [true :indeterminate]) "In Initiative - Include"
            "Start Initiative")]])]
     (if @display-tokens?
       [:section
        [:legend "Image"]
        [:div.stamp-options
         [:div.stamp-default {:on-click #(dispatch :token/remove-stamp idents)}
          [icon {:name "person-circle" :size 36}]]
         [:div.stamp-default {:on-click #(.click @file-upload)}
          [icon {:name "plus-circle" :size 36}]]
         (for [{:keys [image/checksum]} (:root/stamps data)]
           [:div {:key checksum :on-click #(dispatch :token/change-stamp idents checksum)}
            [:div
             {:on-click
              (fn [event]
                (.stopPropagation event)
                (dispatch :stamp/remove checksum))} "×"]
            [stamp {:checksum checksum}]])]
        [upload {:ref file-upload}]])
     [:section
      [:legend "Status"]
      [:fieldset.table
       (for [flag [:player :hidden :deceased]]
         ^{:key flag}
         [checkbox
          {:name "token/flag"
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
      [:legend "Light Radius"]
      [:fieldset.table
       (for [[radius name] light-radii]
         [checkbox
          {:key name
           :name "token/light"
           :value name
           :checked (checked? (fn [token] (= (:token/light token) radius)) selected)
           :on-change #(dispatch :token/change-light idents radius)}
          [label name]])]]
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

(def selected-query
  {:pull
   [{:root/canvas
     [{:canvas/selected
       [:canvas/_tokens]}]}]})

(defn container []
  (let [[data]   (use-query selected-query)
        selected (-> data :root/canvas :canvas/selected)]
    [:<>
     [:section [:header "Token Options"]]
     (if (seq selected)
       [token]
       [:section
        [:div.prompt
         [icon {:name "person-circle" :size 48}]
         [:br] "Configure tokens by selecting"
         [:br] "one or more of them from the canvas"]])]))

(defmethod form :token []
  container)
