(ns ogres.server.build
  (:require [clojure.tools.build.api :as build]))

(def class-dir "target/classes")

(def basis (delay (build/create-basis {:project "deps.edn"})))

(defn ^:export clean [_]
  (build/delete {:path "target"}))

(defn ^:export uber [{:keys [out]}]
  (assert (some? out) "Missing required :out parameter.")
  (let [basis (deref basis)]
    (build/copy-dir
     {:target-dir class-dir
      :src-dirs ["src"]})
    (build/compile-clj
     {:class-dir class-dir
      :basis basis
      :ns-compile '[ogres.server.core]})
    (build/uber
     {:class-dir class-dir
      :basis basis
      :main 'ogres.server.core
      :uber-file (str out)})))
