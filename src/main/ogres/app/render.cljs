(ns ogres.app.render
  (:require [clojure.string :refer [join trim]]
            [ogres.app.env :as env]
            [ogres.app.provider.storage :refer [initialize]]
            [uix.core.alpha :as uix]))

(defn- create-range [min max val]
  (cond (< (- max min) 7)
        (range min (inc max))
        (<= val (+ min 3))
        [min (+ min 1) (+ min 2) (+ min 3) (+ min 4) :space max]
        (>= val (- max 3))
        [min :space (- max 4) (- max 3) (- max 2) (- max 1) max]
        :else
        [min :space (- val 1) val (+ val 1) :space max]))

(defn css [& class-names]
  (->> (reduce (fn [names value]
                 (cond
                   (string?  value) (conj names (trim value))
                   (keyword? value) (conj names value)
                   (number?  value) (conj names (str value))
                   (vector?  value) (vec (concat names value))
                   (map?     value) (->> (reduce
                                          (fn [names [k v]]
                                            (if v (conj names k) names)) [] value)
                                         (concat names)
                                         vec)
                   :else            names)) [] class-names)
       (mapv name)
       (join " ")))

(defn icon [{:keys [name size] :or {size 22}}]
  [:svg {:class "icon" :width size :height size :fill "currentColor"}
   [:use {:href (str env/PATH "/icons.svg" "#icon-" name)}]])

(def error-boundary
  (uix/create-error-boundary
   {:error->state (fn [error] {:error error})
    :handle-catch (fn [])}
   (fn [state [child]]
     (let [{:keys [error]} @state
           store (initialize)]
       (if error
         [:div {:style {:padding "32px" :max-width 640}}
          [:p "The application crashed! Try refreshing the page. If the problem
               persists, click the button below to delete your local data. This
               may resolve your issue."]
          [:button
           {:on-click
            (fn []
              (.delete store)
              (.reload (.-location js/window)))} "Delete your local data"]]
         child)))))

(defn pagination
  [{:keys [pages value on-change]
    :or   {pages 10 value 1 on-change identity}}]
  [:nav {:role "navigation"}
   [:ol.pagination
    [:li {:on-click #(on-change (dec value))
          :css {:selectable (> value 1)}}
     [icon {:name "chevron-left" :size 16}]]
    (for [[idx term] (->> (create-range 1 pages value) (map-indexed vector))]
      [:li {:key idx
            :on-click #(on-change term)
            :css {:selected   (= term value)
                  :selectable (and (not= term value) (not= term :space))}}
       (if (= term :space) \… term)])
    [:li {:on-click #(on-change (inc value))
          :css {:selectable (< value pages)}}
     [icon {:name "chevron-right" :size 16}]]]])
