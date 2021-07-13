(in-package #:ngl)


(jupyter/widgets:defwidget annotation (jupyter/widgets:widget)
  ((position
     :accessor position
     :initarg :position
     :initform #(0s0 0s0 0s0)
     :trait :float-vector
     :documentation "position in 3d")
   (content
     :accessor content
     :initarg :content
     :initform ""
     :trait :string
     :documentation "HTML content")
   (offset
     :accessor offset
     :initarg :offset
     :initform #(0s0 0s0)
     :trait :float-vector
     :documentation "2d offset")
   (visible
     :accessor visible
     :initarg :visible
     :initform t
     :trait :bool
     :documentation "visibility flag"))
  (:documentation "Annotation HTML element floating on top of a position rendered in 3d")
  (:default-initargs
    :%model-name "AnnotationModel"
    :%view-name "AnnotationView"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-module +module-name+
    :%view-module-version +module-version+))

