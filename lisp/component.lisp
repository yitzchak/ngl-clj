(in-package #:ngl)


(defclass component (jupyter-widgets:widget)
  ((name
     :accessor name
     :initarg :name
     :trait :string)
   (position
     :accessor %position
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


(defclass structure (component)
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
     :initform nil
     :trait :single-float-buffer)
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

(jupyter-widgets:register-widget structure)


(defmethod play ((instance structure))
  (dolist (trajectory (trajectories instance) (values))
    (play trajectory)))


(defmethod pause ((instance structure))
  (dolist (trajectory (trajectories instance) (values))
    (pause trajectory)))


(defmethod stop ((instance structure))
  (dolist (trajectory (trajectories instance) (values))
    (stop trajectory)))    


(defun update-position (instance coordinates)
  (jupyter-widgets:send-custom instance
                               (jupyter:json-new-obj
                                 ("do" "update_position"))
                               (list coordinates))
  (values))


(defclass shape (component)
  ((primitives
    :accessor primitives
    :initarg :primitives
    :initform nil
    :trait :plist-list-snake-case))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "ShapeModel"
    :%view-name "ShapeView"
    :representations (list (make-instance 'buffer-representation))))

