(in-package #:ngl)

(defclass representation (jupyter-widgets:widget)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "RepresentationView"
    :%view-module +module-name+
    :%view-module-version +module-version+))


(defclass structure-representation (representation)
  ((sele
     :accessor sele
     :initarg :sele
     :initform ""
     :trait :string))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation ""))


(defclass cartoon (structure-representation)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "CartoonModel"))

(jupyter-widgets:register-widget cartoon)


(defclass ball-and-stick (structure-representation)
  ((sphere-detail
     :accessor sphere-detail
     :initarg :sphere-detail
     :initform 2
     :trait :int))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "BallAndStickModel"))

(jupyter-widgets:register-widget ball-and-stick)
