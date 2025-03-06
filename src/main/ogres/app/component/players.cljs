(ns ogres.app.component.players
  (:require [ogres.app.component :as component]
            [ogres.app.hooks :as hooks]
            [ogres.app.util :refer [separate]]
            [uix.core :refer [defui $]]))

(def ^:private query-user
  [:db/id
   :user/uuid
   :user/color
   :user/type
   :user/label
   :user/description
   {:user/image
    [{:image/thumbnail [:image/hash]}]}])

(def ^:private query
  [{:root/user [:user/uuid :user/type]}
   {:root/session [:session/room {:session/conns query-user}]}])

(defui players []
  (let [dispatch (hooks/use-dispatch)
        {{self-uuid :user/uuid self-type :user/type} :root/user {conns :session/conns} :root/session} (hooks/use-query query [:db/ident :root])
        users (filter (comp #{:conn} :user/type) conns)
        [[user] users] (separate (comp #{self-uuid} :user/uuid) users)]
    ($ :.players
      ($ :<>
        (if (= self-type :conn)
          (let [{:user/keys [color label description image]} user]
            ($ :.player
              ($ :.player-image {:data-editable true}
                ($ :.player-image-frame)
                ($ :.player-image-edit
                  {:on-click (fn [] (dispatch :user/select-panel :lobby))}
                  ($ component/icon {:name "wrench-adjustable-circle" :size 30}))
                (if (not (nil? image))
                  ($ component/image {:hash (:image/hash (:image/thumbnail image))}
                    (fn [url]
                      ($ :.player-image-content
                        {:style {:background-image (str "url(" url ")")}})))
                  ($ :.player-image-content
                    ($ component/icon {:name "dnd" :size 30}))))
              ($ :.player-tile {:style {:border-bottom-color color}}
                ($ :.player-label {:data-placeholder "Player character (You)"} label)
                ($ :.player-description {:data-placeholder "Click on portrait to edit"} description)))))
        (for [{:user/keys [uuid color label description image]} users]
          ($ :.player {:key uuid}
            ($ :.player-image
              ($ :.player-image-frame)
              (if (not (nil? image))
                ($ component/image {:hash (:image/hash (:image/thumbnail image))}
                  (fn [url]
                    ($ :.player-image-content
                      {:style {:background-image (str "url(" url ")")}})))
                ($ :.player-image-content
                  ($ component/icon {:name "dnd" :size 30}))))
            ($ :.player-tile {:style {:border-bottom-color color}}
              ($ :.player-label {:data-placeholder "Player character"} label)
              ($ :.player-description {:data-placeholder ""} description))))))))
