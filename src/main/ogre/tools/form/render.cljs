(ns ogre.tools.form.render)

(defmulti form :form)
(defmethod form :default [] (constantly nil))
