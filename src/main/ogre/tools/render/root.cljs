(ns ogre.tools.render.root
  (:require [cognitect.transit :as t]
            [uix.core.alpha :as uix]
            [datascript.core :as ds]
            [datascript.transit :as dt]
            [ogre.tools.render :refer [context]]
            [ogre.tools.render.layout :refer [layout]]
            [ogre.tools.query :as query]
            [ogre.tools.txs :refer [schema transact]]))

(def ^{:private true} pk 1)

(def boundary
  (uix/create-error-boundary
   {:error->state (fn [error] {:error error})
    :handle-catch (fn [error info])}
   (fn [state [props child]]
     (let [{:keys [error]} @state]
       (if error
         [:div {:style {:padding "32px" :max-width 640}}
          [:p "The application crashed! Try refreshing the page. If the problem
               persists, click the button below to delete your local data. This
               may resolve your issue."]
          [:button {:on-click (:on-reset props)} "Delete your local data"]]
         child)))))

(defn root [{:keys [store] :as props}]
  (let [count (uix/state 0)
        guest (uix/state nil)
        pawn  (uix/state (ds/conn-from-db (:data props)))
        conn  @pawn
        data  @conn
        marsh (t/writer :json)
        unmar (t/reader :json)
        host? (-> (.. js/window -location -search) (js/URLSearchParams.) (.get "share") (not= "true"))
        ready (:viewer/loaded? (ds/entity data [:db/ident :viewer]))
        value {:data      data
               :store     store
               :workspace (query/workspace data)
               :dispatch  (fn [event & args]
                            (let [tx (apply transact data event args)]
                              (ds/transact! conn tx [event args tx])))}]

    (comment
      "Load the application state from IndexedDB, if available. This is only
       run once on initialization.")
    (uix/effect!
     (fn []
       (when (not ready)
         (->
          (.table store "states")
          (.get pk)
          (.then
           (fn [record]
             (if (nil? record)
               (ds/transact!
                conn
                [[:db/add [:db/ident :viewer] :viewer/loaded? true]
                 [:db/add [:db/ident :viewer] :viewer/host? host?]])
               (->
                (.-data record)
                (dt/read-transit-str)
                (ds/conn-from-datoms schema)
                (ds/db)
                (ds/db-with
                 [[:db/add [:db/ident :viewer] :viewer/loaded? true]
                  [:db/add [:db/ident :viewer] :viewer/host? host?]])
                (as-> data (ds/reset-conn! conn data))))))))) [ready])

    (comment
      "Listen for all DataScript transactions, serializing and persisting the
       state to IndexedDB.")
    (uix/effect!
     (fn []
       (ds/listen!
        conn
        (fn [report]
          (swap! count inc)
          (when host?
            (-> (ds/filter (:db-after report) (constantly true))
                (ds/datoms :eavt)
                (dt/write-transit-str)
                (as-> marshalled (.put (.table store "states") #js {:id pk :data marshalled}))))))) [nil])

    (comment
      "Listen for all DataScript transactions, serializing the tx-data and
       sending it to the guest window for evaluation.")
    (uix/effect!
     (fn []
       (ds/listen!
        conn
        (fn [{[_ _ tx] :tx-meta}]
          (when (not (nil? @guest))
            (->>
             #js {:detail (t/write marsh tx)}
             (js/CustomEvent. "AppStateChange")
             (.dispatchEvent @guest)))))) [nil])

    (comment
      "When the guest window is opened, listen for changes from the host
       window.")
    (uix/effect!
     (fn []
       (let [listener
             (fn [event]
               (->> (.-detail event) (t/read unmar) (ds/transact! conn)))]
         (js/window.addEventListener "AppStateChange" listener)
         (fn [] (js/window.removeEventListener "AppStateChange" listener)))) [nil])

    (comment
      "Listen for specific DataScript transactions and perform side-effects
       such as opening the guest window.")
    (uix/effect!
     (fn []
       (ds/listen!
        conn
        (fn [{[event] :tx-meta}]
          (case event
            :share/open
            (let [url (.. js/window -location -origin)
                  url (str url "?share=true")
                  win (js/window.open url "guest" "width=640,height=640")]
              (reset! guest win))
            nil)))) [nil])

    (uix/context-provider
     [context value]
     [boundary
      {:on-reset
       (fn []
         (-> (.table store "states")
             (.delete pk)
             (.then (.reload (.-location js/window)))))}
      [layout]])))
