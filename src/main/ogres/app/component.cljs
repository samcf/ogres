(ns ogres.app.component
  (:require [ogres.app.const :refer [PATH]]
            [ogres.app.hooks :refer [use-image]]
            [ogres.app.provider.storage :refer [initialize]]
            [uix.core :refer [$ defui use-insertion-effect]]))

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

(defn error-boundary
  "Returns a 2-arity React error boundary that includes a control to reset
   the application state. Must be used as the callback for React's
   <ErrorBoundary /> component."
  [[state _] {:keys [children]}]
  (let [store (initialize)]
    (if (some? state)
      ($ :.layout-error
        ($ :.layout-error-content
          ($ :div {:style {:margin-top 4 :color "hsl(6, 73%, 60%)"}}
            ($ icon {:name "exclamation-triangle-fill"}))
          ($ :div
            ($ :div {:style {:font-size 20 :line-height 1}}
              "The application crashed unexpectedly.")
            ($ :div {:style {:margin-top 16 :margin-bottom 8 :max-width 512}}
              "Try refreshing the page; if that doesn't work you may have to
               delete your local data. This will reset the application to
               its original state, removing your data and images.")
            ($ :button.button.button-danger
              {:on-click
               #(.then
                 (.delete store)
                 (.reload (.-location js/window)))} "Delete your local data")
            ($ :nav {:style {:margin-top 32 :color "var(--color-danger-500)"}}
              ($ :ul {:style {:display "flex" :gap 16}}
                ($ :li ($ :a {:href "/"} "Home"))
                ($ :li ($ :a {:href "https://github.com/samcf/ogres/wiki"} "Wiki"))
                ($ :li ($ :a {:href "https://github.com/samcf/ogres/discussions"} "Support")))))))
      children)))

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
  [{:keys [name pages value on-change]
    :or   {pages 10 value 1 on-change identity}}]
  ($ :nav {:role "navigation"}
    ($ :ol.pagination
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
  "Renders the image identified by the given checksum. Must be passed a
   render function as its sole child which receives a map as its only
   argument containing a `:data-url` string of the resource.
   ```
   ($ image {:checksum 'fa7b887b1ce364732beb9ac70892775a'}
     (fn [{:keys [data-url]}]
       ($ :img {:src data-url})))
   ```"
  [{:keys [checksum children]}]
  (let [data-url (use-image checksum)]
    (children {:data-url data-url})))

(defui stylesheet
  "Injects a stylesheet into the head via `<link rel='stylesheet' href='...' />`."
  [props]
  (let [{:keys [name]} props]
    (use-insertion-effect
     (fn []
       (let [element (js/document.createElement "link")]
         (set! (.-href element) (str PATH "/" name))
         (set! (.-rel element) "stylesheet")
         (.. js/document -head (appendChild element)))) [name])) nil)
