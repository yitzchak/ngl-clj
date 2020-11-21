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
     :trait :widget-list)
   (rock
     :accessor rock
     :initarg :rock
     :initform nil
     :trait :bool)
   (spin
     :accessor spin
     :initarg :spin
     :initform nil
     :trait :bool)
   (fullscreen
     :accessor fullscreen
     :initarg :fullscreen
     :initform nil
     :trait :bool)
   (pick-filter
     :accessor pick-filter
     :initarg :pick-filter
     :initform (list "click")
     :trait :json)
   (on-pick
     :accessor on-pick
     :initarg :on-pick
     :initform nil)
   (callbacks-lock
     :accessor callbacks-lock)
   (callbacks
     :reader callbacks
     :initform (make-hash-table :test #'equal)))
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


(defmethod initialize-instance :after ((instance stage) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (callbacks-lock instance) (bordeaux-threads:make-lock (jupyter:comm-id instance))))


(defun make-image (instance callback &key antialias (factor 1) transparent trim)
  (bordeaux-threads:with-lock-held ((callbacks-lock instance))
    (let ((uuid (jupyter:make-uuid)))
      (setf (gethash uuid (callbacks instance)) callback)
      (jupyter-widgets:send-custom instance
        (jupyter:json-new-obj
          ("do" "make-image")
          ("uuid" uuid)
          ("factor" factor)
          ("antialias" (if antialias :true :false))
          ("transparent" (if transparent :true :false))
          ("trim" (if trim :true :false)))))))


(defmethod jupyter-widgets:on-custom-message ((instance stage) content buffers)
  (alexandria:switch ((jupyter:json-getf content "event") :test #'string=)
    ("image"
      (bordeaux-threads:with-lock-held ((callbacks-lock instance))
        (funcall (gethash (jupyter:json-getf content "uuid") (callbacks instance))
                 (first buffers)
                 (jupyter:json-getf content "type"))
        (remhash (jupyter:json-getf content "uuid") (callbacks instance))))
    ("pick"
      (let ((data (jupyter:json-to-nested-plist (jupyter:json-getf content "data") :symbol-case :snake)))
        (dolist (handler (on-pick instance))
                ()
          (funcall handler instance data))))
    (otherwise
      (call-next-method))))


(defmethod play ((instance stage))
  (dolist (component (components instance) (values))
    (play component)))


(defmethod pause ((instance stage))
  (dolist (component (components instance) (values))
    (pause component)))


(defmethod stop ((instance stage))
  (dolist (component (components instance) (values))
    (stop component)))    


(defun on-stage-pick (instance handler)
  (push handler (on-pick instance)))

