(in-package #:ngl)

(jupyter/widgets:defwidget stage (jupyter/widgets:dom-widget)
  ((impostor
     :accessor impostor
     :initarg :imposter
     :initform t
     :trait :bool
     :documentation "")
   (quality
     :accessor quality
     :initarg :quality
     :initform "medium"
     :trait :string
     :documentation "")
   (worker-default
     :accessor worker-default
     :initarg :worker-default
     :initform t
     :trait :bool
     :documentation "default value for useWorker parameter of representations")
   (sample-level
     :accessor sample-level
     :initarg :sample-level
     :initform 0
     :trait :int
     :documentation "sampling level for antialiasing, between -1 and 5; -1: no sampling, 0: only sampling when not moving")
   (background-color
     :accessor background-color
     :initarg :background-color
     :initform "black"
     :trait :color
     :documentation "background color")
   (rotate-speed
     :accessor rotate-speed
     :initarg :rotate-speed
     :initform 2.0d0
     :trait :float
     :documentation "camera-controls rotation speed, between 0 and 10")
   (zoom-speed
     :accessor zoom-speed
     :initarg :zoom-speed
     :initform 1.2d0
     :trait :float
     :documentation "camera-controls rotation speed, between 0 and 10")
   (pan-speed
     :accessor pan-speed
     :initarg :pan-speed
     :initform 1.0d0
     :trait :float
     :documentation "camera-controls pan speed, between 0 and 10")
   (clip-near
     :accessor clip-near
     :initarg :clip-near
     :initform 0d0
     :trait :float
     :documentation "position of camera near/front clipping plane in percent of scene bounding box")
   (clip-far
     :accessor clip-far
     :initarg :clip-far
     :initform 100d0
     :trait :float
     :documentation "position of camera far/back clipping plane in percent of scene bounding box")
   (clip-dist
     :accessor clip-dist
     :initarg :clip-dist
     :initform 10d0
     :trait :float
     :documentation "camera clipping distance in Angstrom")
   (clip-mode
     :accessor clip-mode
     :initarg :clip-mode
     :initform "scene"
     :trait :string
     :documentation "how to interpret clipNear/Far and fogNear/Far values: \"scene\" for scene-relative, \"camera\" for camera-relative")
   (clip-scale
     :accessor clip-scale
     :initarg :clip-scale
     :initform "relative"
     :trait :string
     :documentation "\"relative\" or \"absolute\": interpret clipNear/Far and fogNear/Far as percentage of bounding box or absolute Angstroms (ignored when clipMode==camera)")
   (fog-near
     :accessor fog-near
     :initarg :fog-near
     :initform 50d0
     :trait :float
     :documentation "position of the start of the fog effect in percent of scene bounding box")
   (fog-far
     :accessor fog-far
     :initarg :fog-far
     :initform 100d0
     :trait :float
     :documentation "position where the fog is in full effect in percent of scene bounding box")
   (camera-fov
     :accessor camera-fov
     :initarg :camera-fov
     :initform 40d0
     :trait :float
     :documentation "perspective camera field of view in degree, between 15 and 120")
   (camera-eye-sep
     :accessor camera-eye-sep
     :initarg :camera-eye-sep
     :initform 0.3d0
     :trait :float
     :documentation "stereo camera eye seperation")
   (camera-type
     :accessor camera-type
     :initarg :camera-type
     :initform "perspective"
     :trait :string
     :documentation "type of camera, either 'persepective' or 'orthographic'")
   (light-color
     :accessor light-color
     :initarg :light-color
     :initform "#dddddd"
     :trait :color
     :documentation "point light color")
   (light-intensity
     :accessor light-intensity
     :initarg :light-intensity
     :initform 1.0d0
     :trait :float
     :documentation "point light intensity")
   (ambient-color
     :accessor ambient-color
     :initarg :ambient-color
     :initform "#dddddd"
     :trait :color
     :documentation "ambient light color")
   (ambient-intensity
     :accessor ambient-intensity
     :initarg :ambient-intensity
     :initform 0.2d0
     :trait :float
     :documentation "ambient light intensity")
   (hover-timeout
     :accessor hover-timeout
     :initarg :hover-timeout
     :initform 0d0
     :trait :float
     :documentation "timeout for hovering")
   (tooltip
     :accessor tooltip
     :initarg :tooltip
     :initform t
     :trait :bool
     :documentation "")
   (mouse-preset
     :accessor mouse-preset
     :initarg :mouse-preset
     :initform "default"
     :trait :string
     :documentation "")
   (components
     :accessor components
     :initarg :components
     :initform nil
     :trait :widget-list
     :documentation "")
   (rock
     :accessor rock
     :initarg :rock
     :initform nil
     :trait :bool
     :documentation "")
   (spin
     :accessor spin
     :initarg :spin
     :initform nil
     :trait :bool
     :documentation "")
   (fullscreen
     :accessor fullscreen
     :initarg :fullscreen
     :initform nil
     :trait :bool
     :documentation "")
   (pick-filter
     :accessor pick-filter
     :initarg :pick-filter
     :initform (list "click")
     :trait :json
     :documentation "")
   (on-pick
     :accessor on-pick
     :initarg :on-pick
     :initform nil)
   (on-select
     :accessor on-select
     :initarg :on-select
     :initform nil)
   (on-remove
     :accessor on-remove
     :initarg :on-remove
     :initform nil)
   (callbacks-lock
     :accessor callbacks-lock)
   (callbacks
     :reader callbacks
     :initform (make-hash-table :test #'equal)))
  (:documentation "")
  (:default-initargs
    :%model-name "StageModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "StageView"
    :%view-module +module-name+
    :%view-module-version +module-version+))


(defmethod initialize-instance :after ((instance stage) &rest initargs &key &allow-other-keys)
  (declare (ignore initargs))
  (setf (callbacks-lock instance) (bordeaux-threads:make-lock (jupyter:comm-id instance))))


(defun make-image (instance callback &key antialias (factor 1) transparent trim)
  (bordeaux-threads:with-lock-held ((callbacks-lock instance))
    (let ((uuid (jupyter:make-uuid)))
      (setf (gethash uuid (callbacks instance)) callback)
      (jupyter/widgets:send-custom instance
        `(:object-alist
           ("do" . "make_image")
           ("uuid" . ,uuid)
           ("factor" . ,factor)
           ("antialias" . ,(if antialias :true :false))
           ("transparent" . ,(if transparent :true :false))
           ("trim" . ,(if trim :true :false)))))))


(defmethod jupyter/widgets:on-custom-message ((instance stage) content buffers)
  (alexandria:switch ((gethash "event" content) :test #'string=)
    ("image"
      (bordeaux-threads:with-lock-held ((callbacks-lock instance))
        (funcall (gethash (gethash "uuid" content) (callbacks instance))
                 (first buffers)
                 (gethash "type" content))
        (remhash (gethash "uuid" content) (callbacks instance))))
    ("pick"
      (let ((data (gethash "data" content)))
        (dolist (handler (on-pick instance))
          (funcall handler instance data))))
    ("select"
      (let ((data (gethash "data" content)))
        (dolist (handler (on-select instance))
          (funcall handler instance data))))
    ("remove"
      (dolist (handler (on-remove instance))
        (funcall handler instance)))
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


(defun on-stage-select (instance handler)
  (push handler (on-select instance)))


