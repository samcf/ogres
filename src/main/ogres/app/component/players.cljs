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
   {:user/image [:image/hash]}])

(def ^:private query
  [{:root/user [:user/uuid :user/type]}
   {:root/session [:session/room {:session/conns query-user}]}])

(defui players []
  (let [{{self-uuid :user/uuid self-type :user/type} :root/user {conns :session/conns} :root/session} (hooks/use-query query [:db/ident :root])
        users (filter (comp #{:conn} :user/type) conns)
        [[user] users] (separate (comp #{self-uuid} :user/uuid) users)]
    ($ :.players
      ($ :<>
        (if (= self-type :conn)
          (let [{:user/keys [uuid color label description image]} user]
            ($ :.players-player {:key uuid}
              ($ :.players-player-image {:data-editable true}
                ($ :.players-player-image-frame)
                (if (some? image)
                  ($ component/image {:hash (:image/hash image)}
                    (fn [url]
                      (if (some? url)
                        ($ :.players-player-image-content {:style {:background-image (str "url(" url ")")}})
                        ($ :.players-player-image-content))))
                  ($ :<>
                    ($ :.players-player-image-edit
                      ($ component/icon {:name "wrench-adjustable-circle" :size 30}))
                    ($ :.players-player-image-content
                      ($ component/icon {:name "dnd" :size 30})))))
              ($ :.players-player-tile {:style {:border-bottom-color color}}
                ($ :.players-player-label {:data-placeholder "Player character (You)"} label)
                ($ :.players-player-description {:data-placeholder "Click on portrait to edit"} description)))))
        (for [{:user/keys [uuid color label description image]} users]
          ($ :.players-player {:key uuid}
            ($ :.players-player-image
              ($ :.players-player-image-frame)
              (if (some? image)
                ($ image {:hash (:image/hash image)}
                  (fn [url]
                    (if (some? url)
                      ($ :.players-player-image-content {:style {:background-image (str "url(" url ")")}})
                      ($ :.players-player-image-content))))
                ($ :.players-player-image-content
                  ($ component/icon {:name "dnd" :size 30}))))
            ($ :.players-player-tile {:style {:border-bottom-color color}}
              ($ :.players-player-label {:data-placeholder "Player character"} label)
              ($ :.players-player-description {:data-placeholder ""} description))))))))
