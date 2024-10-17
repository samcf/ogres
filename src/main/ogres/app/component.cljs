(ns ogres.app.component
  (:require [ogres.app.const :refer [PATH]]
            [ogres.app.hooks :as hooks]
            [uix.core :refer [defui $]]))

(defn ^:private create-range
  "Returns a vector of page indexes or spacing sentinels suitable for use
   in a pagination component. Sentinels include `:spacel` and `:spacer`.
   The vector will contain anywhere between 1 and 7 elements.
   ```
   (create-range 1 78 23) ;; [1 :spacel 22 23 24 :spacer 78]
   ```"
  [min max val]
  (cond (< (- max min) 7)
        (range min (inc max))
        (<= val (+ min 3))
        [min (+ min 1) (+ min 2) (+ min 3) (+ min 4) :spacel max]
        (>= val (- max 3))
        [min :spacel (- max 4) (- max 3) (- max 2) (- max 1) max]
        :else
        [min :spacel (- val 1) val (+ val 1) :spacer max]))

(defui icon
  "Renders the `<svg>` definition found in `icons.svg` matching the given name.
   ```
   ($ icon {:name 'arrow-right-short' :size 16})
   ```"
  [{:keys [name size] :or {size 22}}]
  ($ :svg
    {:fill "currentColor"
     :role "presentation"
     :class "icon"
     :width size
     :height size}
    ($ :use {:href (str PATH "/icons.svg" "#icon-" name)})))

(defui pagination
  "Renders a `<nav>` element that provides a means to navigate through
   a paginated resource, including buttons to go backward, forward,
   and directly to different pages.
   ```
   ($ pagination
     {:name 'token-gallery'
      :pages 3
      :value 1
      :on-change (fn [page] ...)})
   ```"
  [{:keys [class-name name pages value on-change]
    :or   {pages 10 value 1 on-change identity}}]
  ($ :nav {:role "navigation"}
    ($ :ol.pagination
      {:class (if class-name (str "pagination-" class-name))}
      ($ :li
        ($ :button
          {:aria-disabled (= value 1)
           :on-click
           (fn []
             (if (not (= value 1))
               (on-change (dec value))))}
          ($ icon {:name "chevron-left" :size 16})))
      (for [term (create-range 1 pages value)]
        ($ :li {:key term}
          (if (or (= term :spacel) (= term :spacer))
            ($ :label \…)
            ($ :label
              ($ :input
                {:type "radio"
                 :name name
                 :value value
                 :checked (= term value)
                 :on-change #(on-change term)}) term))))
      ($ :li
        ($ :button
          {:aria-disabled (= value pages)
           :on-click
           (fn []
             (if (not (= value pages))
               (on-change (inc value))))}
          ($ icon {:name "chevron-right" :size 16}))))))

(defui image
  "Renders the image identified by the given hash. Accepts a
   render function as its children which is passed a URL that
   references the image resource.
   ```
   ($ image {:hash 'fa7b887b1ce364732beb9ac70892775a'}
     (fn [url]
       ($ :img {:src url})))
   ```"
  [{:keys [hash children]}]
  (let [url (hooks/use-image hash)]
    (children url)))

(defui modal-fullscreen [props]
  ($ :.modal-fullscreen {:tab-index -1 :role "dialog"}
    ($ :.modal-fullscreen-dialog {:role "document"}
      ($ :.modal-fullscreen-content
        (:children props)))))
