(ns ogres.app.form.render)

(defmulti form :form)
(defmethod form :default [] (constantly nil))
