(ns ogre.tools.timing
  (:require [goog.async.Debouncer]))

(defn debounce [f interval]
  (let [d (goog.async.Debouncer. f interval)]
    (fn [& args]
      (.apply (.-fire d) d (to-array args)))))
