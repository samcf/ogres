(ns ogres.app.form.render)

(defmulti form :form)
(defmethod form :default [] (constantly nil))

(defmulti header :form)
(defmethod header :default [] (constantly nil))
