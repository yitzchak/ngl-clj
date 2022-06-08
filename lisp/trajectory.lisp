(in-package #:ngl)


(jupyter/widgets:defwidget trajectory (jupyter/widgets:widget)
  ((name
     :accessor name
     :initarg :name
     :trait :string)
   (ext
     :accessor ext
     :initarg :ext
     :initform nil
     :trait :string)
   (value
     :accessor value
     :initarg :value
     :initform :null
     :trait :json)
   (frame
     :accessor frame
     :initarg :frame
     :initform 0
     :trait :int)
   (count
     :accessor %count
     :initarg :count
     :initform :null
     :trait :int)
   (step
     :accessor %step
     :initarg :step
     :initform 1
     :trait :int)
   (start
     :accessor start
     :initarg :start
     :initform 0
     :trait :int)
   (end
     :accessor end
     :initarg :end
     :initform :null
     :trait :int)
   (timeout
     :accessor timeout
     :initarg :default-timeout
     :initform 50
     :trait :int)
   (interpolate-type
     :accessor interpolate-type
     :initarg :interpolate-type
     :initform ""
     :trait :string)
   (interpolate-step
     :accessor interpolate-step
     :initarg :interpolate-step
     :initform 5
     :trait :int)
   (mode
     :accessor mode
     :initarg :mode
     :initform "loop"
     :trait :string)
   (direction
     :accessor direction
     :initarg :direction
     :initform "forward"
     :trait :string)
   (is-running
     :accessor is-running
     :initarg :is-running
     :initform nil
     :trait :bool)
   (trajectory-count
     :accessor trajectory-count
     :initarg :trajectory-count
     :initform (lambda (instance)
                 (declare (ignore instance))
                 0))
   (trajectory-frame
     :accessor trajectory-frame
     :initarg :trajectory-frame
     :initform (lambda (instance i atom-indices)
                 (declare (ignore instance i atom-indices))
                 (values i nil #() 0))))
  (:documentation "")
  (:default-initargs
    :%model-name "TrajectoryModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "TrajectoryView"
    :%view-module +module-name+
    :%view-module-version +module-version+))


(defmethod play ((instance trajectory))
  (jupyter/widgets:send-custom instance '(:object-plist "do" "play"))
  (values))


(defmethod pause ((instance trajectory))
  (jupyter/widgets:send-custom instance '(:object-plist "do" "pause"))
  (values))


(defmethod stop ((instance trajectory))
  (jupyter/widgets:send-custom instance '(:object-plist "do" "stop"))
  (values))


(defmethod jupyter/widgets:on-custom-message ((instance trajectory) content buffers)
  (declare (ignore buffers))
  (alexandria:switch ((gethash "event" content) :test #'string=)
    ("count"
      (jupyter/widgets:send-custom instance
                                   `(:object-plist "do" "count"
                                                   "count" ,(funcall (trajectory-count instance)
                                                                     instance))))
    ("frame"
      (multiple-value-bind (i box coords count)
                           (funcall (trajectory-frame instance)
                                    instance
                                    (gethash "i" content)
                                    (gethash "atom_indices" content))
        (jupyter/widgets:send-custom instance
                                     `(:object-plist "do" "frame"
                                                     "i" ,i
                                                     "box" ,(or box :null)
                                                     "count" ,count)
                                     (list coords))))

    (otherwise
      (call-next-method))))



