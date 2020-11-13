(in-package #:ngl)


(defgeneric play (instance))


(defgeneric pause (instance))


(defgeneric stop (instance))


(defun auto-view (instance &optional (duration 0))
  (check-type instance (or stage structure))
  (jupyter-widgets:send-custom instance
                               (jupyter:json-new-obj
                                 ("do" "auto_view")
                                 ("duration" duration)))
  (values))



