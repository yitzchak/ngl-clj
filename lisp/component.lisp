(in-package #:ngl)


(defclass component (jupyter-widgets:widget)
  ((name
     :accessor name
     :initarg :name
     :trait :string)
   #+(or)(position
     :accessor position
     :initarg :position
     :initform #(0d0 0d0 0d0)
     :trait :float-vector)
   (representations
     :accessor representations
     :initarg :representations
     :initform nil
     :trait :widget-list)
   (quaternion
     :accessor quaternion
     :initarg :quaternion
     :initform #(0d0 0d0 0d0 0d0)
     :trait :float-vector)
   (scale
     :accessor scale
     :initarg :scale
     :initform 1d0
     :trait :float)
   (visible
     :accessor visible
     :initarg :visible
     :initform t
     :trait :bool))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-module +module-name+
    :%view-module-version +module-version+))


(defun auto-view (component &optional (duration 0))
  (jupyter-widgets:send-custom component
                               (jupyter:json-new-obj
                                 ("do" "auto_view")
                                 ("duration" duration))))


(defclass %structure (component)
  ((ext
     :accessor ext
     :initarg :ext
     :initform nil
     :trait :string)
   (value
     :accessor value
     :initarg :value
     :initform :null
     :trait :json)
   (positions
     :accessor positions
     :initarg :positions
     :initform :null
     :trait :vector)
   (as-trajectory
     :accessor as-trajectory
     :initarg :as-trajectory
     :initform nil
     :trait :bool)
   (trajectories
     :accessor trajectories
     :initarg :trajectories
     :initform nil
     :trait :widget-list))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "StructureModel"
    :%view-name "StructureView"
    :representations (list (make-instance 'cartoon)
                           (make-instance 'base)
                           (make-instance 'ball-and-stick :sele "ligand"))))

(jupyter-widgets:register-widget %structure)


(defmethod play ((instance %structure))
  (dolist (trajectory (trajectories instance) (values))
    (play trajectory)))


(defmethod pause ((instance %structure))
  (dolist (trajectory (trajectories instance) (values))
    (pause trajectory)))


(defmethod stop ((instance %structure))
  (dolist (trajectory (trajectories instance) (values))
    (stop trajectory)))    
