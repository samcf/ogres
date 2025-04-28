(ns ogres.app.dom)

(defn user-type []
  (let [search (.. js/window -location -search)
        params (js/URLSearchParams. search)]
    (if (string? (.get params "join"))
      :conn
      :host)))
