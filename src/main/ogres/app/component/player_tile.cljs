(ns ogres.app.component.player-tile
  (:require [uix.core :refer [defui $]]
            [ogres.app.component :refer [icon image]]))

(defui player-tile
  [{:keys [user editable auto-focus on-click-portrait]
    :or {editable false auto-focus false on-click-portrait (fn [])}}]
  ($ :.player-tile
    {:data-color (:user/color user)
     :data-editable editable}
    ($ :input {:type "hidden" :name "id" :value (:user/uuid user)})
    ($ :.player-tile-color {:aria-hidden true})
    ($ :button.player-tile-image
      {:type "button"
       :on-click (fn [] (if editable (on-click-portrait)))
       :disabled (not editable)}
      ($ :.player-tile-image-frame)
      (if-let [hash (-> user :user/image :image/thumbnail :image/hash)]
        ($ image {:hash hash}
          (fn [url]
            ($ :.player-tile-image-content
              {:style {:background-image (str "url(" url ")")}})))
        ($ :.player-tile-image-default
          ($ icon {:name "dnd" :size 38})))
      ($ :.player-tile-image-edit "Change portrait"))
    ($ :.player-tile-content
      (if (not editable)
        ($ :.player-tile-label
          (if (seq (:user/label user))
            (:user/label user)
            "Player character"))
        ($ :input.player-tile-input
          {:type "text"
           :name "label"
           :disabled (not editable)
           :default-value (:user/label user)
           :placeholder (if editable "Name" "")
           :auto-focus (and auto-focus (= (:user/label user) ""))
           :auto-complete "off"}))
      (if (not editable)
        ($ :.player-tile-description
          (:user/description user))
        ($ :input.player-tile-input
          {:type "text"
           :name "description"
           :disabled (not editable)
           :default-value (:user/description user)
           :placeholder (if editable "Description" "")
           :auto-complete "off"})))))
