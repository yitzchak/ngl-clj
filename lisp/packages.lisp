(defpackage #:ngl
  (:use #:common-lisp)
  (:shadow #:structure)
  (:export
    #:ambient-color
    #:ambient-intensity
    #:atom-quad
    #:auto-view
    #:backbone
    #:background-color
    #:ball-and-stick
    #:base
    #:buffer
    #:buffer-representation
    #:camera-eye-sep
    #:camera-fov
    #:camera-type
    #:cartoon
    #:clip-center
    #:clip-dist
    #:clip-far
    #:clip-mode
    #:clip-near
    #:clip-radius
    #:clip-scale
    #:color-domain
    #:color-mode
    #:color-reverse
    #:color-scale
    #:color-scheme
    #:color-value
    #:component
    #:components
    #:depth-write
    #:diffuse
    #:diffuse-interior
    #:dihedral
    #:direction
    #:disable-impostor
    #:disable-picking
    #:ext
    #:flat-shaded
    #:fog-far
    #:fog-near
    #:frame
    #:fullscreen
    #:hover-timeout
    #:impostor
    #:interior-color
    #:interior-darkening
    #:interpolate-step
    #:interpolate-type
    #:is-running
    #:label-color
    #:label-size
    #:label-visible
    #:label-z-offset
    #:lazy
    #:licorice
    #:light-color
    #:light-intensity
    #:line
    #:matrix
    #:measurement
    #:metalness
    #:mode
    #:mouse-preset
    #:name
    #:on-pick
    #:on-stage-pick
    #:opacity
    #:open-ended
    #:pan-speed
    #:pause
    #:pick-filter
    #:play
    #:position
    #:positions
    #:primitives
    #:quality
    #:quaternion
    #:radial-segments
    #:representation
    #:representations
    #:ribbon
    #:rock
    #:rotate-speed
    #:roughness
    #:sample-level
    #:scale
    #:sele
    #:shape
    #:side
    #:spacefill
    #:sphere-detail
    #:spin
    #:stage
    #:%step
    #:stop
    #:structure
    #:structure-representation
    #:surface
    #:timeout
    #:tooltip
    #:trajectories
    #:trajectory
    #:update-position
    #:use-interior-color
    #:use-worker
    #:uuid
    #:value
    #:value
    #:visible
    #:wireframe
    #:worker-default
    #:zoom-speed))

