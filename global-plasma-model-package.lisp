;;;; package.lisp

(defpackage #:global-plasma-model
  (:nicknames :gpm)
  (:use #:cl :alexandria :gsll :grid
	:collision-cross-sections-and-rates
	:mv-gnuplot :mv-grid :lisp-unit)
  (:shadow :lisp-unit
	   :norm)
  (:shadow :lisp-unit
	   :set-equal)
  (:shadow :alexandria
	   :standard-deviation :mean :variance :factorial :median)
  (:shadow :gsll :row :column))

