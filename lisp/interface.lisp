(in-package #:ngl)


(defgeneric play (instance)
  (:method (instance)
    (declare (ignore instance))))


(defgeneric pause (instance)
  (:method (instance)
    (declare (ignore instance))))


(defgeneric stop (instance)
  (:method (instance)
    (declare (ignore instance))))


(defun auto-view (instance &optional (duration 0))
  (check-type instance (or stage component))
  (jupyter-widgets:send-custom instance
                               (jupyter:json-new-obj
                                 ("do" "auto_view")
                                 ("duration" duration)))
  (values))


(defun move (instance to &optional (duration 0))
  (check-type instance (or stage component))
  (jupyter-widgets:send-custom instance
                               (jupyter:json-new-obj
                                 ("do" "move")
                                 ("to" to)
                                 ("duration" duration)))
  (values))


#+(or)(defun rock (instance axis angle end &optional (duration 0))
  (check-type instance (or stage component))
  (jupyter-widgets:send-custom instance
                               (jupyter:json-new-obj
                                 ("do" "rock")
                                 ("axis" axis)
                                 ("angle" angle)
                                 ("end" end)
                                 ("duration" duration)))
  (values))


#+(or)(defun spin (instance axis angle end &optional (duration 0))
  (check-type instance (or stage component))
  (jupyter-widgets:send-custom instance
                               (jupyter:json-new-obj
                                 ("do" "spin")
                                 ("axis" axis)
                                 ("angle" angle)
                                 ("duration" duration)))
  (values))    
