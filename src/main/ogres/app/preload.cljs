(ns ogres.app.preload
  (:require [uix.dev]))

(uix.dev/init-fast-refresh!)

(defn ^{:export true :dev/after-load true} refresh []
  (uix.dev/refresh!))
