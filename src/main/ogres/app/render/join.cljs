(ns ogres.app.render.join
  (:require [clojure.string :refer [blank? trim upper-case]]
            [ogres.app.render :refer [icon]]
            [ogres.app.hooks :refer [use-query]]
            [uix.core :refer [defui $ use-state use-callback]]
            [uix.dom :refer [create-portal]]))

(def ^:private join-query
  [{:root/local   [[:session/state :default :initial]]
    :root/session [{:session/conns [:db/key]} :session/room]}])

(def ^:private status-icon
  ($ icon {:name "globe-americas" :size 16}))

(defui ^:private prompt
  [{:keys [on-close] :or {on-close identity}}]
  (let [[input set-input] (use-state "")
        on-submit         (use-callback
                           (fn [event]
                             (.preventDefault event)
                             (let [params (js/URLSearchParams. #js {"release" "latest" "join" (trim input)})
                                   origin (.. js/window -location -origin)
                                   href   (str origin "?" (.toString params))]
                               (set! (.. js/window -location -href) href))) [input])]
    ($ :.modal.join-modal
      ($ :.modal-container
        ($ :form {:on-submit on-submit}
          ($ :.modal-content
            ($ :.modal-icon
              ($ icon {:name "globe-americas" :size 36}))
            ($ :.modal-body
              ($ :.modal-heading "Join lobby")
              ($ :.modal-paragraph
                "Enter the room code given to you by the host." ($ :br)
                "The code is not case-sensitive.")
              ($ :input
                {:type "text" :value input :auto-focus true
                 :style {:height 36 :font-size 16}
                 :on-change
                 (fn [event]
                   (set-input (upper-case (.. event -target -value))))})))
          ($ :.modal-footer
            ($ :button.button
              {:type "button" :on-click #(on-close)} "Close")
            ($ :button.button.button-primary
              {:type "submit" :disabled (blank? input)} "Join")))))))

(defui join []
  (let [[modal set-modal] (use-state false)
        result (use-query join-query [:db/ident :root])
        node   (js/document.querySelector ".root")
        {{state :session/state} :root/local
         {code  :session/room
          conns :session/conns} :root/session} result]
    ($ :div.join
      (case state
        :initial      ($ :button.button.button-primary {:on-click #(set-modal true)} status-icon "Join with code")
        :connecting   ($ :button.button {:disabled true} status-icon "Connecting...")
        :connected    ($ :button.button {:disabled true} status-icon "Connected / " code " / [" (inc (count conns)) "]")
        :disconnected ($ :button.button.button-danger {:disabled true} status-icon "Disconnected")
        ($ :button.button {:disabled true} status-icon "Status not known"))
      (if modal
        (create-portal
         ($ prompt {:on-close #(set-modal false)})
         node)))))
