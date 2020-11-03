(in-package #:ngl)

(defclass component (jupyter-widgets:widget)
  ((uuid
     :accessor uuid
     :initarg :uuid
     :initform (jupyter:make-uuid)
     :trait :string)
   (representations
     :accessor representations
     :initarg :representations
     :initform nil
     :trait :widget-list)
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
     :trait :json))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "StructureModel"
    :%view-name "StructureView"
    :representations (list (make-instance 'cartoon)
                           (make-instance 'base)
                           (make-instance 'ball-and-stick :sele "ligand"))))

(jupyter-widgets:register-widget %structure)
