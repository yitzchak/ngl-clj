(asdf:defsystem #:ngl-clj
  :description "A nglview widget for Common Lisp Jupyter."
  :version "0.8.0"
  :author "Tarn W. Burton"
  :license "MIT"
  :depends-on
    (:common-lisp-jupyter)
  :components
    ((:module lisp
      :serial t
      :components
        ((:file "packages")
         (:file "version")
         (:file "interface")
         (:file "annotation")
         (:file "representation")
         (:file "trajectory")
         (:file "component")
         (:file "stage")))))
