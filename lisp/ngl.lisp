(in-package #:ngl)

(defclass ngl (jupyter-widgets:dom-widget)
  ()
  (:metaclass jupyter-widgets:trait-metaclass)
  (:documentation "")
  (:default-initargs
    :%model-name "NGLModel"
    :%model-module +module-name+
    :%model-module-version +module-version+
    :%view-name "NGLView"
    :%view-module +module-name+
    :%view-module-version +module-version+))

(jupyter-widgets:register-widget ngl)

