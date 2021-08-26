(ns ogre.tools.render.root
  (:require [goog.async.Debouncer]
            [cognitect.transit :as t]
            [uix.core.alpha :as uix]
            [datascript.core :as ds]
            [datascript.transit :as dt]
            [ogre.tools.render :refer [context]]
            [ogre.tools.render.layout :refer [layout]]
            [ogre.tools.query :as query]
            [ogre.tools.txs :refer [schema transact]]))

(def ^{:private true} pk 1)

(def ^{:private true} unpersisted-attrs
  #{:viewer/host?
    :viewer/loaded?
    :viewer/privileged?})

(defn debounce [f interval]
  (let [d (goog.async.Debouncer. f interval)]
    (fn [& args]
      (.apply (.-fire d) d (to-array args)))))

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

(defn guest-window []
  (let [context (uix/context context)
        trigger "AppStateChange"
        host?   (-> context :data query/viewer :viewer/host?)
        guest   (uix/state nil)
        marsh   (t/writer :json)
        unmar   (t/reader :json)
        close   (fn []
                  (when @guest
                    (.close @guest)
                    (reset! guest nil)
                    ((:dispatch context) :share/toggle false)))]

    (uix/effect!
     (fn []
       (let [targets (filterv identity [js/window @guest])
             callback
             (debounce
              (fn [event]
                (let [search (.. event -target -location -search)
                      params (js/URLSearchParams. search)
                      host?  (not (= (.get params "share") "true"))
                      canvas (.. event -target -document (querySelector "svg.canvas"))
                      bounds (.getBoundingClientRect canvas)
                      [x y w h]
                      [(.-x bounds)
                       (.-y bounds)
                       (.-width bounds)
                       (.-height bounds)]]
                  ((:dispatch context) :canvas/change-bounds host? x y w h))) 128)]
         (when host?
           (doseq [target targets]
             (.addEventListener target "resize" callback)
             (.dispatchEvent target (js/Event. "resize"))))
         (fn []
           (doseq [target targets] (.removeEventListener target "resize" callback))))) [@guest])

    (uix/effect!
     (fn []
       (.addEventListener js/window "beforeunload" close)
       (fn [] (.removeEventListener js/window "beforeunload" close))) [nil])

    (uix/effect!
     (fn []
       (let [listener
             (fn [event]
               (->> (.-detail event) (t/read unmar) (ds/transact! (:conn context))))]
         (js/window.addEventListener trigger listener)
         (fn [] (js/window.removeEventListener trigger listener)))) [nil])

    (uix/effect!
     (fn []
       (ds/listen!
        (:conn context) :share
        (fn [{[event _ tx] :tx-meta}]
          (if (= event :share/initiate)
            (if (nil? @guest)
              (let [url (.. js/window -location -origin)
                    url (str url "?share=true")
                    win (.open js/window url "ogre.tools" "width=640,height=640")]
                (reset! guest win)
                ((:dispatch context) :share/toggle true)
                (.addEventListener
                 win "visibilitychange"
                 (fn []
                   (js/window.setTimeout
                    (fn [] (when (.-closed win) (close))) 128))))
              (close))
            (when @guest
              (->>
               #js {:detail (t/write marsh tx)}
               (js/CustomEvent. trigger)
               (.dispatchEvent @guest))))))
       (fn [] (ds/unlisten! (:conn context) :share))) [@guest])
    nil))

(defn root [{:keys [store] :as props}]
  (let [count (uix/state 0)
        pawn  (uix/state (ds/conn-from-db (:data props)))
        conn  @pawn
        data  @conn
        host? (-> (.. js/window -location -search) (js/URLSearchParams.) (.get "share") (not= "true"))
        ready (:viewer/loaded? (ds/entity data [:db/ident :viewer]))
        value {:conn      conn
               :data      data
               :store     store
               :workspace (query/workspace data)
               :dispatch  (fn [event & args]
                            (let [tx (apply transact data event args)]
                              (ds/transact! conn tx [event args tx])))}]

    ;; Load the application state from IndexedDB, if available. This is only
    ;; run once on initialization.
    (uix/effect!
     (fn []
       (when (not ready)
         (->
          (.table store "states")
          (.get pk)
          (.then
           (fn [record]
             (let [tx [[:db/add [:db/ident :viewer] :viewer/loaded? true]
                       [:db/add [:db/ident :viewer] :viewer/host? host?]]]
               (if (nil? record)
                 (ds/transact! conn tx)
                 (->
                  (.-data record)
                  (dt/read-transit-str)
                  (ds/conn-from-datoms schema)
                  (ds/db)
                  (ds/db-with tx)
                  (as-> data (ds/reset-conn! conn data)))))))))) [ready])

    ;; Re-renders the application and persists the current application state
    ;; to IndexedDB whenever DataScript transactions occur.
    (uix/effect!
     (fn []
       (ds/listen!
        conn :persist-state
        (fn [report]
          (let [viewer (query/viewer (:db-after report))]
            (when (or host? (not (:share/paused? viewer)))
              (swap! count inc)))
          (when host?
            (-> (ds/filter
                 (:db-after report)
                 (fn [_ [_ attr _ _]]
                   (not (contains? unpersisted-attrs attr))))
                (ds/datoms :eavt)
                (dt/write-transit-str)
                (as-> marshalled (.put (.table store "states") #js {:id pk :data marshalled}))))))
       (fn [] (ds/unlisten! conn :persist-state))) [nil])

    ;; Registers transaction handlers for performing side effects.
    (uix/effect!
     (fn []
       (ds/listen!
        conn :side-effects
        (fn [{[event] :tx-meta}]
          (case event
            :storage/reset
            (do (.delete store)
                (.reload (.-location js/window)))
            nil)))
       (fn [] (ds/unlisten! conn :side-effects))) [nil])

    (uix/context-provider
     [context value]
     [boundary {:on-reset #((:dispatch value) :storage/reset)}
      [:<>
       [guest-window]
       [layout]]])))
