(ns ogres.app.render.join
  (:require [clojure.string :refer [upper-case]]
            [ogres.app.render :refer [icon]]
            [ogres.app.hooks :refer [use-query]]
            [uix.core :refer [defui $ use-state use-callback use-ref]]
            [uix.dom :refer [create-portal]]))

(def ^:private join-query
  [{:root/local   [[:session/state :default :initial]]
    :root/session [{:session/conns [:db/key]} :session/room]}])

(def ^:private status-icon
  ($ icon {:name "globe-americas" :size 16}))

(defui ^:private prompt
  [{:keys [on-close] :or {on-close identity}}]
  (let [[code set-code] (use-state "")
        input           (use-ref)
        on-submit       (use-callback
                         (fn [event]
                           (.preventDefault event)
                           (let [params (js/URLSearchParams. #js {"release" "latest" "join" code})
                                 origin (.. js/window -location -origin)
                                 href   (str origin "?" (.toString params))]
                             (set! (.. js/window -location -href) href))) [code])]
    ($ :.modal.join-modal
      ($ :.modal-container
        ($ :form {:on-submit on-submit}
          ($ :.modal-content
            ($ :.modal-icon
              ($ icon {:name "globe-americas" :size 36}))
            ($ :.modal-body
              ($ :.modal-heading "Join Room")
              ($ :.modal-paragraph "Enter the 4-letter room code given to you by the host.")
              ($ :.join-code
                {:on-click #(.focus @input)}
                ($ :input.join-input
                  {:ref        input
                   :type       "text"
                   :value      code
                   :auto-focus true
                   :max-length 4
                   :on-change  (fn [event] (-> (.. event -target -value) upper-case set-code))})
                ($ :.join-codes
                  (for [indx (range 4) :let [focused (= indx (count code))]]
                    ($ :div {:key indx :data-focused focused}
                      (nth code indx nil)))))))
          ($ :.modal-footer
            ($ :button.button.button-neutral
              {:type "button" :on-click #(on-close)} "Cancel")
            ($ :button.button.button-primary
              {:type "submit" :disabled (not= (count code) 4)} "Join")))))))

(defui join []
  (let [[modal set-modal] (use-state false)
        result (use-query join-query [:db/ident :root])
        node   (js/document.querySelector ".root")
        {{state :session/state} :root/local
         {code  :session/room
          conns :session/conns} :root/session} result]
    ($ :div.join
      (case state
        :initial      ($ :button.button {:on-click #(set-modal true)} status-icon "Join with code")
        :connecting   ($ :button.button {:disabled true} status-icon "Connecting...")
        :connected    ($ :button.button {:disabled true} status-icon "Connected / " code " / [" (inc (count conns)) "]")
        :disconnected ($ :button.button {:disabled true} status-icon "Disconnected")
        ($ :button.button {:disabled true} status-icon "Status not known"))
      (if modal
        (create-portal
         ($ prompt {:on-close #(set-modal false)})
         node)))))
