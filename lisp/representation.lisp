(in-package #:ngl)


(jupyter/widgets:defwidget representation (jupyter/widgets:widget)
  ((clip-center
     :accessor clip-center
     :initarg :clip-center
     :initform #(0d0 0d0 0d0)
     :trait :float-vector
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
   #+(or)(matrix
     :accessor matrix
     :initarg :matrix
     :initform #2A((1d0 0d0 0d0 0d0)
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
     :documentation "Render as wireframe"))
  (:documentation "")
  (:default-initargs
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "RepresentationView"
    :%view-module +module-name+
    :%view-module-version +module-version+))


(jupyter/widgets:defwidget buffer-representation (representation)
  ((buffer
     :accessor buffer
     :initarg :buffer
     :initform nil
     :trait :plist-camel-case))
  (:documentation "")
  (:default-initargs
    :%model-name "BufferRepresentationModel"
    :%view-name "BufferRepresentationView"))


(jupyter/widgets:defwidget structure-representation (representation)
  ((assembly
     :accessor assembly
     :initarg :assembly
     :initform "default"
     :trait :string)
   (radius-scale
     :accessor radius-scale
     :initarg :radius-scale
     :initform 1s0
     :trait :float
     :documentation "")
   (sele
     :accessor sele
     :initarg :sele
     :initform ""
     :trait :string))
  (:documentation ""))


(jupyter/widgets:defwidget cartoon (structure-representation)
  ((aspect-ratio
     :accessor aspect-ratio
     :initarg :aspect-ratio
     :initform 5s0
     :trait :float)
   (subdiv
     :accessor subdiv
     :initarg :subdiv
     :initform 12s0
     :trait :float)
   (radial-segments
     :accessor radial-segments
     :initarg :radial-segments
     :initform 10
     :trait :int)
   (tension
     :accessor tension
     :initarg :tension
     :initform nil
     :trait :float)
   (capped
     :accessor capped
     :initarg :capped
     :initform t
     :trait :boolean)
   (smooth-sheet
     :accessor smooth-sheet
     :initarg :smooth-sheet
     :initform nil
     :trait :boolean))
  (:documentation "")
  (:default-initargs
    :%model-name "CartoonModel"))


(jupyter/widgets:defwidget ball-and-stick (structure-representation)
  (#+(or)(aspect-ratio
     :accessor aspect-ratio
     :initarg :aspect-ratio
     :initform 2s0
     :trait :float)
   #+(or)(bond-scale
     :accessor bond-scale
     :initarg :bond-scale
     :initform 0.4s0
     :trait :float)
   #+(or)(bond-spacing
     :accessor bond-spacing
     :initarg :bond-spacing
     :initform 1s0
     :trait :float)
   #+(or)(cylinder-only
     :accessor cylinder-only
     :initarg :cylinder-only
     :initform nil
     :trait :boolean)
   #+(or)(disable-impostor
     :accessor disable-imposter
     :initarg :disable-imposter
     :initform t
     :trait :bool
     :documentation "")
   #+(or)(line-only
     :accessor line-only
     :initarg :line-only
     :initform nil
     :trait :boolean)
   #+(or)(linewidth
     :accessor linewidth
     :initarg :linewidth
     :initform 2s0
     :trait :float)
   (multiple-bond
     :accessor multiple-bond
     :initarg :multiple-bond
     :initform "off"
     :trait :string)
   #+(or)(open-ended
     :accessor open-ended
     :initarg :open-ended
     :initform t
     :trait :boolean)
   #+(or)(radial-segments
     :accessor radial-segments
     :initarg :radial-segments
     :initform t
     :trait :float)
   #+(or)(sphere-detail
     :accessor sphere-detail
     :initarg :sphere-detail
     :initform 2
     :trait :int))
  (:documentation "")
  (:default-initargs
    :color-scheme "element"
    :%model-name "BallAndStickModel"))


(jupyter/widgets:defwidget backbone (ball-and-stick)
  ()
  (:documentation "")
  (:default-initargs
    :%model-name "BackboneModel"))


(jupyter/widgets:defwidget base (ball-and-stick)
  ()
  (:documentation "")
  (:default-initargs
    :%model-name "BaseModel"))


(jupyter/widgets:defwidget licorice (ball-and-stick)
  ()
  (:documentation "")
  (:default-initargs
    :%model-name "LicoriceModel"))


(jupyter/widgets:defwidget line (structure-representation)
  ()
  (:documentation "")
  (:default-initargs
    :color-scheme "element"
    :flat-shaded :null
    :%model-name "LineModel"))


(jupyter/widgets:defwidget measurement (representation)
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
  (:documentation "")
  (:default-initargs
    :flat-shaded :null))


(jupyter/widgets:defwidget dihedral (measurement)
  ((atom-quad
     :accessor atom-quad
     :initarg :atom-quad
     :initform nil
     :trait :list))
  (:documentation "")
  (:default-initargs
    :%model-name "DihedralModel"))


(jupyter/widgets:defwidget ribbon (structure-representation)
  ()
  (:documentation "")
  (:default-initargs
    :%model-name "RibbonModel"))


(jupyter/widgets:defwidget spacefill (structure-representation)
  ()
  (:documentation "")
  (:default-initargs
    :color-scheme "element"
    :%model-name "SpacefillModel"))


(jupyter/widgets:defwidget surface (structure-representation)
  ((background
     :accessor background
     :initarg :background
     :initform nil
     :trait :bool
     :documentation "Render the surface in the background, unlit.")
   (color-volume
     :accessor color-volume
     :initarg :color-volume
     :initform nil
     :trait :json
     :documentation "parameter for surface representation of volume data")
   (contour
     :accessor contour
     :initarg :contour
     :initform nil
     :trait :bool
     :documentation "")
   (cutoff
     :accessor cutoff
     :initarg :cutoff
     :initform 0s0
     :trait :float
     :documentation "")
   (filter-sele
     :accessor filter-sele
     :initarg :filter-sele
     :initform ""
     :trait :string
     :documentation "")
   (opaque-back
     :accessor opaque-back
     :initarg :opaque-back
     :initform t
     :trait :bool
     :documentation "Render the back-faces (where normals point away from the camera) of the surface opaque, ignoring the transparency parameter.")
   (probe-radius
     :accessor probe-radius
     :initarg :probe-radius
     :initform 1.4s0
     :trait :float
     :documentation "")
   (scale-factor
     :accessor scale-factor
     :initarg :scale-factor
     :initform 2s0
     :trait :float
     :documentation "")
   (smooth
     :accessor smooth
     :initarg :smooth
     :initform 2
     :trait :int
     :documentation "How many iterations of laplacian smoothing after surface triangulation. For volume data only.")
   (surface-type
     :accessor surface-type
     :initarg :surface-type
     :initform "ms"
     :trait :string
     :documentation "")
   (use-worker
     :accessor use-worker
     :initarg :use-worker
     :initform t
     :trait :bool
     :documentation "Weather or not to triangulate the volume asynchronously in a Web Worker. For volume data only."))
  (:documentation "")
  (:default-initargs
    :%model-name "SurfaceModel"))


(jupyter/widgets:defwidget unitcell (structure-representation)
  ((radius-size
     :accessor radius-size
     :initarg :radius-size
     :initform 0.5s0
     :trait :float
     :documentation ""))
  (:documentation "")
  (:default-initargs
    :%model-name "UnitcellModel"))



