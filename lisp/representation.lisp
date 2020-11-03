(in-package #:ngl)

(defclass representation (jupyter-widgets:widget)
  ((color-scheme
     :accessor color-scheme
     :initarg :color-scheme
     :initform "chainname"
     :trait :string)
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
    :color-scheme "element"
    :%model-name "BallAndStickModel"))

(jupyter-widgets:register-widget ball-and-stick)


(defclass base (ball-and-stick)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "BaseModel"))

(jupyter-widgets:register-widget base)


(defclass measurement (representation)
  ((label-size
     :accessor label-size
     :initarg :label-size
     :initform 2d0
     :trait :float)
   (label-color
     :accessor label-color
     :initarg :label-color
     :initform "white"
     :trait :color)
   (label-visible
     :accessor label-visible
     :initarg :label-visible
     :initform t
     :trait :bool)
   (label-z-offset
     :accessor label-z-offset
     :initarg :label-z-offset
     :initform 0.5d0
     :trait :float))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation ""))


(defclass dihedral (measurement)
  ((atom-quad
     :accessor atom-quad
     :initarg :atom-quad
     :initform nil
     :trait :list))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation ""))

(jupyter-widgets:register-widget dihedral)

