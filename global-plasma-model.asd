;;;; global-plasma-model.asd

(asdf:defsystem #:global-plasma-model
  :serial t
  :depends-on (#:mv-gnuplot
               #:collision-cross-sections-and-rates
               #:mv-grid-utils
               #:gsll
               #:physics-constants
               #:alexandria)
  :components ((:file "package")
               (:file "global-plasma-model")))

