(ns ogre.tools.render
  (:require [clojure.string :refer [join trim]]
            [ogre.tools.env :as env]
            [ogre.tools.provider.storage :refer [initialize]]
            [uix.core.alpha :as uix]))

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

(defn checkbox [{:keys [checked on-change]} child]
  (let [key   (uix/state (random-uuid))
        indtr (= checked :indeterminate)
        input (uix/ref)]
    (uix/effect!
     (fn [] (set! (.-indeterminate @input) indtr)) [indtr])
    [:div
     [:input
      {:id @key :ref input :type "checkbox"
       :class "ogre-checkbox" :checked (if indtr false checked)
       :on-change (fn [event] (on-change (.. event -target -checked)))}]
     [:label {:for @key} child]]))

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
