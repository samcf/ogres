(ns ogre.tools.errors
  (:require [ogre.tools.provider.storage :refer [initialize]]
            [uix.core.alpha :as uix]))

(def boundary
  (uix/create-error-boundary
   {:error->state (fn [error] {:error error})
    :handle-catch (fn [])}
   (fn [state [child]]
     (let [{:keys [error]} @state
           store (initialize)]
       (if error
         [:div {:style {:padding "32px" :max-width 640}}
          [:p "The application crashed! Try refreshing the page. If the problem
               persists, click the button below to delete your local data. This
               may resolve your issue."]
          [:button
           {:on-click
            (fn []
              (.delete store)
              (.reload (.-location js/window)))} "Delete your local data"]]
         child)))))
