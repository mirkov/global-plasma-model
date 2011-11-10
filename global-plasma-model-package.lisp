;;;; package.lisp

(defpackage :global-plasma-model
  (:nicknames :gpm)
  (:use #:cl :alexandria :gsll :grid
	:collision-cross-sections-and-rates
	:mv-gnuplot :mv-grid :lisp-unit
	:two-ion-species-sheath)
  (:shadow :lisp-unit
	   :norm)
  (:shadow :lisp-unit
	   :set-equal)
  (:shadow :alexandria
	   :standard-deviation :mean :variance :factorial :median)
  (:shadow :gsll :row :column)
  (:export :calc-ne :calc-ne0
	   :Ei :Ee :energy/Ar+ :energy/Xe+ :Vs
	   :Xi :XI-Ar0 :XI-Xe0
	   :Te-equation0 :Te-equation% :calc-te0
	   :particle-balance-calculation0 :print-Te-calc-results0
	   :*tolerance* :*max-iter* :*iisi-coupling
	   :*Ub-Ar* :*Ub-Xe* :*XI-Ar* :*XI-Xe*
	   :Te-equation :Te-equation% :calc-xi&ub :calc-te0
	   :particle-balance-calculation :print-Te-calc-results))





(defpackage :global-plasma-model-user
    (:nicknames :gpm-user)
    (:use #:cl :alexandria :gsll :grid
	  :collision-cross-sections-and-rates
	  :mv-gnuplot :mv-grid :lisp-unit
	  :global-plasma-model
	  :two-ion-species-sheath)
    (:shadow :lisp-unit
	     :norm)
    (:shadow :lisp-unit
	     :set-equal)
    (:shadow :alexandria
	     :standard-deviation :mean :variance :factorial :median)
    (:shadow :gsll :row :column))

