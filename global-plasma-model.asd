;;;; global-plasma-model.asd

(asdf:defsystem :global-plasma-model
  :serial t
  :depends-on (:mv-gnuplot
               :collision-cross-sections-and-rates
               :mv-grid-utils
               :gsll
               :physics-constants
               :alexandria
	       :lisp-unit
	       :two-ion-species-sheath
	       :mobility-data)
  :components ((:file "global-plasma-model-package")
	       (:file "gpm-setup")
	       (:file "plasma-parameters")
	       (:file "geometric-effects")
	       (:file "iisi-coupling-effects")
	       (:file "particle-balance")
	       (:file "energy-balance")
	       #|(:file "example1")
	       (:file "plots")|#
	       #|(:file "fields&mfpaths")|#))

