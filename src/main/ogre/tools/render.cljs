(ns ogre.tools.render
  (:require [clojure.string :as string]
            [datascript.core :refer [squuid]]
            [ogre.tools.storage :refer [storage]]
            [ogre.tools.state :refer [PATH]]
            [uix.core.alpha :as uix]))

(defn css [& class-names]
  (->> (reduce (fn [names value]
                 (cond
                   (string?  value) (conj names (string/trim value))
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
       (string/join " ")))

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
   [:use {:href (str PATH "/icons.svg" "#icon-" name)}]])

(defn listen!
  "Manages the registration and cleanup of a DOM event handler."
  ([handler event dependencies]
   (listen! handler js/window event dependencies))
  ([handler element event dependencies]
   (uix/effect!
    (fn [] (if element (.addEventListener element event handler #js {:passive false}))
      (fn [] (if element (.removeEventListener element event handler #js {:passive false})))) dependencies)))

(defn use-modal []
  (let [ref (uix/ref) state (uix/state false)]
    (listen!
     (fn [event]
       (if (and @ref (not (.contains @ref (.-target event))))
         (swap! state not))) (if @state js/document false) "click" [@state])
    [state ref]))

(def cache (atom {}))

(defn use-image [checksum]
  (let [{:keys [store]}  (uix/context storage)
        sentinel         (uix/state 0)
        watch-key        (deref (uix/state (squuid)))
        [loading cached] (get @cache checksum [false nil])]

    (if (not (or loading cached))
      (swap! cache assoc checksum [true nil]))

    (uix/effect!
     (fn []
       (if (string? checksum)
         (add-watch
          cache watch-key
          (fn [_ _ _ value]
            (if (not cached)
              (let [[_ cached] (get value checksum [false nil])]
                (if cached (swap! sentinel inc)))))))
       (fn [] (remove-watch cache watch-key))) [checksum cached])

    (uix/effect!
     (fn []
       (if (and (string? checksum) (not (or loading cached)))
         (-> (.table store "images")
             (.get checksum)
             (.then (fn [r] (.fetch js/window (.-data r))))
             (.then (fn [r] (.blob r)))
             (.then (fn [b]
                      (->> (js/File. #js [b] "image" #js {:type (.-type b)})
                           (.createObjectURL js/URL)
                           (vector false)
                           (swap! cache assoc checksum))))))) [checksum])

    cached))
