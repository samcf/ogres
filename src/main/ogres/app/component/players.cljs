(ns ogres.app.component.players
  (:require [ogres.app.component :as component]
            [ogres.app.hooks :as hooks]
            [uix.core :refer [defui $]]))

(def ^:private truncation-limit 5)

(defn ^:private conns-xf [self]
  (comp (filter (comp #{:conn} :user/type))
        (filter (comp (complement #{(:user/uuid self)}) :user/uuid))
        (filter
         (fn [user]
           (= (:db/id (:camera/scene (:user/camera user)))
              (:db/id (:camera/scene (:user/camera self))))))))

(def ^:private query-user
  [:user/uuid
   :user/color
   :user/type
   :user/label
   :user/description
   {:user/image
    [{:image/thumbnail [:image/hash]}]}
   {:user/camera
    [{:camera/scene
      [:db/id]}]}])

(def ^:private query
  [{:root/user query-user}
   {:root/session
    [:session/room
     {:session/conns query-user}]}])

(defui players []
  (let [dispatch (hooks/use-dispatch)
        result   (hooks/use-query query [:db/ident :root])
        {self :root/user
         {conns :session/conns} :root/session} result
        users (sequence (conns-xf self) conns)]
    ($ :.players
      {:data-truncated
       (> (count users)
          (if (= (:user/type self) :host)
            truncation-limit
            (dec truncation-limit)))}
      (if (= (:user/type self) :conn)
        (let [{:user/keys [color label description image]} self]
          ($ :.player-tile {:data-editable true}
            ($ :.player-tile-color {:data-color color})
            ($ :button.player-tile-image
              {:type "button"
               :on-click (fn [] (dispatch :user/select-panel :lobby))}
              ($ :.player-tile-image-frame)
              (if (not (nil? image))
                ($ component/image {:hash (:image/hash (:image/thumbnail image))}
                  (fn [url]
                    ($ :.player-tile-image-content
                      {:style {:background-image (str "url(" url ")")}})))
                ($ :.player-tile-image-default
                  ($ component/icon {:name "dnd"})))
              ($ :.player-tile-image-edit
                ($ component/icon {:name "wrench-adjustable-circle" :size 26})))
            ($ :.player-tile-content
              ($ :.player-tile-label
                {:data-placeholder "Player character (You)"}
                label)
              ($ :.player-tile-description
                {:data-placeholder "Click on portrait to edit"}
                description)))))
      (for [{:user/keys [uuid color label description image]} users]
        ($ :.player-tile {:key uuid}
          ($ :.player-tile-color {:data-color color})
          ($ :.player-tile-image
            ($ :.player-tile-image-frame)
            (if (not (nil? image))
              ($ component/image {:hash (:image/hash (:image/thumbnail image))}
                (fn [url]
                  ($ :.player-tile-image-content
                    {:style {:background-image (str "url(" url ")")}})))
              ($ :.player-tile-image-default
                ($ component/icon {:name "dnd"}))))
          ($ :.player-tile-content
            ($ :.player-tile-label {:data-placeholder "Player character"} label)
            ($ :.player-tile-description {:data-placeholder ""} description)))))))
