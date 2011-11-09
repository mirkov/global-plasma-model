;;;; global-plasma-model.asd

(asdf:defsystem :global-plasma-model
  :serial t
  :depends-on (:mv-gnuplot
               :collision-cross-sections-and-rates
               :mv-grid-utils
               :gsll
               :physics-constants
               :alexandria
	       :lisp-unit)
  :components ((:file "global-plasma-model-package")
	       (:file "gpm-setup")
	       (:file "plasma-parameters")
	       (:file "geometric-effects")
	       (:file "particle-balance")
	       (:file "particle-balance-new")
	       (:file "energy-balance")
	       (:file "two-ion-species-sheath")
	       (:file "example1")))

