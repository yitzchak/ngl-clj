(in-package #:ngl)


(defclass trajectory (jupyter-widgets:widget)
  ((name
     :accessor name
     :initarg :name
     :trait :string)
   (value
     :accessor value
     :initarg :value
     :initform :null
     :trait :json)
   (initial-frame
     :accessor initial-frame
     :initarg :initial-frame
     :initform 0
     :trait :int)
   (default-step
     :accessor default-step
     :initarg :default-step
     :initform nil
     :trait :int)
   (default-timeout
     :accessor default-timeout
     :initarg :default-timeout
     :initform 0
     :trait :int)
   (default-interpolate-type
     :accessor default-interpolate-type
     :initarg :default-interpolate-type
     :initform ""
     :trait :string)
   (default-interpolate-step
     :accessor default-interpolate-step
     :initarg :default-interpolate-step
     :initform 50
     :trait :int)
   (default-mode
     :accessor default-mode
     :initarg :default-mode
     :initform "loop"
     :trait :string)
   (default-direction
     :accessor default-direction
     :initarg :default-direction
     :initform "forward"
     :trait :string))
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



