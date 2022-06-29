(ns ogre.tools.render
  (:require [clojure.string :refer [join trim]]
            [datascript.core :as ds :refer [squuid]]
            [ogre.tools.env :as env]
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
  (let [key   (uix/state (squuid))
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

(defn button [props children]
  [:button (merge {:class "ogre-button" :type "button"} props) children])

(defn icon [{:keys [name size] :or {size 22}}]
  [:svg {:class "icon" :width size :height size :fill "currentColor"}
   [:use {:href (str env/PATH "/icons.svg" "#icon-" name)}]])

(defn listen!
  "Manages the registration and cleanup of a DOM event handler."
  ([handler event dependencies]
   (listen! handler js/window event dependencies))
  ([handler element event dependencies]
   (uix/effect!
    (fn [] (if element (.addEventListener element event handler #js {:passive false}))
      (fn [] (if element (.removeEventListener element event handler #js {:passive false})))) dependencies)))

(defn use-interval
  "Periodically runs the given function `f` every `delay` milliseconds."
  [f delay]
  (uix/effect!
   (fn []
     (let [id (js/window.setInterval f delay)]
       (fn [] (js/window.clearInterval id))))))

(defn use-modal []
  (let [ref (uix/ref) state (uix/state false)]
    (listen!
     (fn [event]
       (if (and @ref (not (.contains @ref (.-target event))))
         (swap! state not))) (if @state js/document false) "click" [@state])
    [state ref]))
