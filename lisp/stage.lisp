(in-package #:ngl)

(defclass stage (jupyter-widgets:dom-widget)
  ((impostor
     :accessor impostor
     :initarg :imposter
     :initform t
     :trait :bool)
   (quality
     :accessor quality
     :initarg :quality
     :initform "medium"
     :trait :string)
   (worker-default
     :accessor worker-default
     :initarg :worker-default
     :initform t
     :trait :bool)
   (sample-level
     :accessor sample-level
     :initarg :sample-level
     :initform 0
     :trait :int)
   (background-color
     :accessor background-color
     :initarg :background-color
     :initform "black"
     :trait :color)
   (rotate-speed
     :accessor rotate-speed
     :initarg :rotate-speed
     :initform 2.0d0
     :trait :float)
   (zoom-speed
     :accessor zoom-speed
     :initarg :zoom-speed
     :initform 1.2d0
     :trait :float)
   (pan-speed
     :accessor pan-speed
     :initarg :pan-speed
     :initform 1.0d0
     :trait :float)
   (clip-near
     :accessor clip-near
     :initarg :clip-near
     :initform 0d0
     :trait :float)
   (clip-far
     :accessor clip-far
     :initarg :clip-far
     :initform 100d0
     :trait :float)
   (clip-dist
     :accessor clip-dist
     :initarg :clip-dist
     :initform 10d0
     :trait :float)
   (clip-mode
     :accessor clip-mode
     :initarg :clip-mode
     :initform "scene"
     :trait :string)
   (clip-scale
     :accessor clip-scale
     :initarg :clip-scale
     :initform "relative"
     :trait :string)
   (fog-near
     :accessor fog-near
     :initarg :fog-near
     :initform 50d0
     :trait :float)
   (fog-far
     :accessor fog-far
     :initarg :fog-far
     :initform 100d0
     :trait :float)
   (camera-fov
     :accessor camera-fov
     :initarg :camera-fov
     :initform 40d0
     :trait :float)
   (camera-eye-sep
     :accessor camera-eye-sep
     :initarg :camera-eye-sep
     :initform 0.3d0
     :trait :float)
   (camera-type
     :accessor camera-type
     :initarg :camera-type
     :initform "perspective"
     :trait :string)
   (light-color
     :accessor light-color
     :initarg :light-color
     :initform "#dddddd"
     :trait :color)
   (light-intensity
     :accessor light-intensity
     :initarg :light-intensity
     :initform 1.0d0
     :trait :float)
   (ambient-color
     :accessor ambient-color
     :initarg :ambient-color
     :initform "#dddddd"
     :trait :color)
   (ambient-intensity
     :accessor ambient-intensity
     :initarg :ambient-intensity
     :initform 0.2d0
     :trait :float)
   (hover-timeout
     :accessor hover-timeout
     :initarg :hover-timeout
     :initform 0d0
     :trait :float)
   (tooltip
     :accessor tooltip
     :initarg :tooltip
     :initform t
     :trait :bool)
   (mouse-preset
     :accessor mouse-preset
     :initarg :mouse-preset
     :initform "default"
     :trait :string)
   (components
     :accessor components
     :initarg :components
     :initform nil
     :trait :widget-list))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "StageModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "StageView"
    :%view-module +module-name+
    :%view-module-version +module-version+))

(jupyter-widgets:register-widget stage)

