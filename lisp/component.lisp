(in-package #:ngl)


(jupyter-widgets:defwidget component (jupyter-widgets:widget)
  ((name
     :accessor name
     :initarg :name
     :trait :string)
   (position
     :accessor position
     :initarg :position
     :initform #(0s0 0s0 0s0)
     :trait :float-vector)
   (auto-view-duration
     :accessor auto-view-duration
     :initarg :auto-view-duration
     :initform nil
     :trait :int)
   (annotations
     :accessor annotations
     :initarg :annotations
     :initform nil
     :trait :widget-list)
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
  (:documentation "")
  (:default-initargs
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-module +module-name+
    :%view-module-version +module-version+))


(jupyter-widgets:defwidget structure (component)
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
  (:documentation "")
  (:default-initargs
    :%model-name "StructureModel"
    :%view-name "StructureView"
    :representations (list (make-instance 'cartoon)
                           (make-instance 'base)
                           (make-instance 'ball-and-stick :sele "ligand"))))


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
                               (list :object-plist
                                     "do" "update_position")
                               (list coordinates))
  (values))


(defun remove-all-measurements (instance)
  (jupyter-widgets:send-custom instance
                               (list :object-plist
                                     "do" "remove_all_measurements"))
  (values))


(defun remove-measurement (instance atoms)
  (jupyter-widgets:send-custom instance
                               (list :object-plist
                                     "do" "remove_measurement"
                                     "atoms" (or atoms :empty-array)))
  (values))


(defun add-measurement (instance atoms)
  (jupyter-widgets:send-custom instance
                               (list :object-plist
                                     "do" "add_measurement"
                                     "atoms" (or atoms :empty-array)))
  (values))


(jupyter-widgets:defwidget shape (component)
  ((primitives
    :accessor primitives
    :initarg :primitives
    :initform nil
    :trait :plist-list-snake-case))
  (:documentation "")
  (:default-initargs
    :%model-name "ShapeModel"
    :%view-name "ShapeView"
    :representations (list (make-instance 'buffer-representation))))

