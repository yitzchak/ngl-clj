(in-package #:ngl)


(defclass representation (jupyter-widgets:widget)
  ((clip-center
     :accessor clip-center
     :initarg :clip-center
     :initform #(0d0 0d0 0d0)
     :trait :flaot-vector
     :documentation "position of for spherical clipping")
   (clip-near
     :accessor clip-near
     :initarg :clip-near
     :initform 0d0
     :trait :float
     :documentation "position of camera near/front clipping plane in percent of scene bounding box")
   (clip-radius
     :accessor clip-radius
     :initarg :clip-radius
     :initform 0d0
     :trait :float
     :documentation "radius of clipping sphere")
   (color-domain
     :accessor color-domain
     :initarg :color-domain
     :initform nil
     :trait :float-vector
     :documentation "scale value range")
   (color-mode
     :accessor color-mode
     :initarg :color-mode
     :initform "hcl"
     :trait :string
     :documentation "color mode, one of rgb, hsv, hsl, hsi, lab, hcl")
   (color-reverse
     :accessor color-reverse
     :initarg :color-reverse
     :initform nil
     :trait :bool
     :documentation "reverse color scale")
   (color-scale
     :accessor color-scale
     :initarg :color-scale
     :initform ""
     :trait :json
     :documentation "color scale, either a string for a predefined scale or an array of colors to be used as the scale")
   (color-scheme
     :accessor color-scheme
     :initarg :color-scheme
     :initform "chainname"
     :trait :string
     :documentation "color scheme")
   (color-value
     :accessor color-value
     :initarg :color-value
     :initform "#909090"
     :trait :color
     :documentation "color value")
   (depth-write
     :accessor depth-write
     :initarg :depth-write
     :initform t
     :trait :bool
     :documentation "depth write")
   (diffuse
     :accessor diffuse
     :initarg :diffuse
     :initform "#ffffff"
     :trait :color
     :documentation "diffuse color for lighting")
   (diffuse-interior
     :accessor diffuse-interior
     :initarg :diffuse-interior
     :initform nil
     :trait :bool
     :documentation "diffuse interior, i.e. ignore normal")
   (disable-impostor
     :accessor disable-imposter
     :initarg :disable-imposter
     :initform nil
     :trait :bool
     :documentation "")
   (disable-picking
     :accessor disable-picking
     :initarg :disable-picking
     :initform nil
     :trait :bool
     :documentation "disable picking")
   (flat-shaded
     :accessor flat-shaded
     :initarg :flat-shaded
     :initform nil
     :trait :bool
     :documentation "render flat shaded")
   (interior-color
     :accessor interior-color
     :initarg :interior-color
     :initform "#222222"
     :trait :color
     :documentation "")
   (interior-darkening
     :accessor interior-darkening
     :initarg :interior-darkening
     :initform 0
     :trait :float
     :documentation "interior darkening: 0 no darking, 1 fully darkened")
   (lazy
     :accessor lazy
     :initarg :lazy
     :initform nil
     :trait :bool
     :documentation "only build & update the representation when visible otherwise defer changes until set visible again")
   (matrix
     :accessor matrix
     :initarg :matrix
     :initform `((1d0 0d0 0d0 0d0)
                 (0d0 1d0 0d0 0d0)
                 (0d0 0d0 1d0 0d0)
                 (0d0 0d0 0d0 1d0))
     :trait :json
     :documentation "")
   (metalness
     :accessor metalness
     :initarg :metalness
     :initform 0d0
     :trait :float
     :documentation "how metallic the material is, between 0 and 1")
   (name
     :accessor name
     :initarg :name
     :trait :string
     :documentation "")
   (opacity
     :accessor opacity
     :initarg :opacity
     :initform 1d0
     :trait :float
     :documentation "translucency: 1 is fully opaque, 0 is fully transparent")
   (open-ended
     :accessor open-ended
     :initarg :open-ended
     :initform t
     :trait :bool
     :documentation "")
   (quality
     :accessor quality
     :initarg :quality
     :initform nil
     :trait :string
     :documentation "")
   (radial-segments
     :accessor radial-segments
     :initarg :radial-segments
     :initform 10
     :trait :int
     :documentation "")
   (roughness
     :accessor roughness
     :initarg :roughness
     :initform 0.4d0
     :trait :float
     :documentation "how rough the material is, between 0 and 1")
   (side
     :accessor side
     :initarg :side
     :initform "double"
     :trait :string
     :documentation "which triangle sides to render, \"front\" front-side, \"back\" back-side, \"double\" front- and back-side")
   (sphere-detail
     :accessor sphere-detail
     :initarg :sphere-detail
     :initform 1
     :trait :int
     :documentation "")
   (use-interior-color
     :accessor use-interior-color
     :initarg :use-interior-color
     :initform nil
     :trait :bool
     :documentation "use interior color")
   (visible
     :accessor visible
     :initarg :visible
     :initform t
     :trait :bool
     :documentation "")
   (wireframe
     :accessor wireframe
     :initarg :wireframe
     :initform nil
     :trait :bool
     :documentation "ender as wireframe"))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "RepresentationView"
    :%view-module +module-name+
    :%view-module-version +module-version+))


(defclass buffer-representation (representation)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "BufferRepresentationModel"))


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


(defclass backbone (ball-and-stick)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "BackboneModel"))

(jupyter-widgets:register-widget backbone)


(defclass base (ball-and-stick)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "BaseModel"))

(jupyter-widgets:register-widget base)


(defclass licorice (ball-and-stick)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "LicoriceModel"))

(jupyter-widgets:register-widget licorice)


(defclass line (structure-representation)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :color-scheme "element"
    :%model-name "LineModel"))

(jupyter-widgets:register-widget line)


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
  (:documentation "")
  (:default-initargs
    :%model-name "DihedralModel"))

(jupyter-widgets:register-widget dihedral)


(defclass ribbon (structure-representation)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "RibbonModel"))

(jupyter-widgets:register-widget ribbon)


(defclass spacefill (structure-representation)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :color-scheme "element"
    :%model-name "SpacefillModel"))

(jupyter-widgets:register-widget spacefill)


(defclass surface (representation)
  ((use-worker
     :accessor use-worker
     :initarg :use-worker
     :initform nil
     :trait :bool))
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "SurfaceModel"))

(jupyter-widgets:register-widget surface)

