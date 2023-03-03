(ns ogres.app.form.tokens
  (:require [ogres.app.form.render :as render]
            [ogres.app.hooks :refer [use-image use-image-uploader use-query]]
            [ogres.app.render :refer [icon pagination]]
            [uix.core.alpha :as uix]))

(def ^:private query
  [{:root/stamps [:image/checksum]}])

(defn- thumbnail [checksum render-fn]
  (render-fn (use-image checksum)))

(defn- header []
  (let [input  (uix/ref)
        upload (use-image-uploader {:type :token})]
    [:button.upload-button
     {:type "button"
      :on-click (fn [event]
                  (.stopPropagation event)
                  (.click (deref input)))}
     [:input
      {:type "file" :hidden true :accept "image/*" :multiple true :ref input
       :on-click  (fn [event] (.stopPropagation event))
       :on-change (fn [event]
                    (doseq [file (.. event -target -files)]
                      (upload file)))}]
     [icon {:name "camera-fill" :size 18}]
     "Upload"]))

(defn- form []
  (let [page-num (uix/state 1)
        result   (use-query query [:db/ident :root])
        tokens   (vec (:root/stamps result))]
    (if (seq tokens)
      (let [src (* (dec (deref page-num)) 20)
            dst (min (+ src 20) (count tokens))]
        [:section.tokens
         (for [{:image/keys [checksum]} (subvec tokens src dst)]
           ^{:key checksum}
           [thumbnail checksum
            (fn [url]
              [:figure {:style {:background-image (str "url(" url ")")}}])])
         [pagination
          {:pages (int (js/Math.ceil (/ (count tokens) 20)))
           :value (deref page-num)
           :on-change (partial reset! page-num)}]])
      [:section.tokens
       [:div.prompt
        [:br] "Upload one or more images from your"
        [:br] "computer to use as tokens."]])))

(defmethod render/header :tokens [] header)
(defmethod render/form :tokens [] form)
