(ns ogres.app.render
  (:require [clojure.string :refer [join]]
            [ogres.app.env :as env]
            [ogres.app.provider.storage :refer [initialize]]
            [uix.core :refer [defui $ create-error-boundary]]))

(defn ^:private create-range
  [min max val]
  (cond (< (- max min) 7)
        (range min (inc max))
        (<= val (+ min 3))
        [min (+ min 1) (+ min 2) (+ min 3) (+ min 4) :space max]
        (>= val (- max 3))
        [min :space (- max 4) (- max 3) (- max 2) (- max 1) max]
        :else
        [min :space (- val 1) val (+ val 1) :space max]))

(def ^:private css-xf
  (comp (filter (comp identity val)) (map key) (map name)))

(defn css [class-names]
  (join " " (sequence css-xf class-names)))

(defui icon [{:keys [name size] :or {size 22}}]
  ($ :svg {:class "icon" :width size :height size :fill "currentColor"}
    ($ :use {:href (str env/PATH "/icons.svg" "#icon-" name)})))

(def error-boundary
  (create-error-boundary
   {:derive-error-state (fn [error] {:error error})}
   (fn [[state _] {:keys [children]}]
     (let [store (initialize)]
       (if (:error state)
         ($ :div {:style {:padding "32px" :max-width 640}}
           ($ :p "The application crashed! Try refreshing the page. If the problem
               persists, click the button below to delete your local data. This
               may resolve your issue.")
           ($ :br)
           ($ :button.button.button-danger
             {:on-click
              (fn []
                (.delete store)
                (.reload (.-location js/window)))} "Delete your local data"))
         children)))))

(defui pagination
  [{:keys [pages value on-change]
    :or   {pages 10 value 1 on-change identity}}]
  ($ :nav {:role "navigation"}
    ($ :ol.pagination
      ($ :li {:on-click #(on-change (dec value)) :class (css {:selectable (> value 1)})}
        ($ icon {:name "chevron-left" :size 16}))
      (for [[idx term] (->> (create-range 1 pages value) (map-indexed vector))]
        ($ :li {:key idx
                :class (css {:selected (= term value) :selectable (and (not= term value) (not= term :space))})
                :on-click #(on-change term)}
          (if (= term :space) \… term)))
      ($ :li {:on-click #(on-change (inc value)) :class (css {:selectable (< value pages)})}
        ($ icon {:name "chevron-right" :size 16})))))
