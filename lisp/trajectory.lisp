(in-package #:ngl)


(defclass trajectory (jupyter-widgets:widget)
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
   (step
     :accessor %step
     :initarg :step
     :initform nil
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
     :trait :bool))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "TrajectoryModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "TrajectoryView"
    :%view-module +module-name+
    :%view-module-version +module-version+))

(jupyter-widgets:register-widget trajectory)


(defmethod play ((instance trajectory))
  (jupyter-widgets:send-custom instance '(:object-plist "do" "play"))
  (values))


(defmethod pause ((instance trajectory))
  (jupyter-widgets:send-custom instance '(:object-plist "do" "pause"))
  (values))


(defmethod stop ((instance trajectory))
  (jupyter-widgets:send-custom instance '(:object-plist "do" "stop"))
  (values))

