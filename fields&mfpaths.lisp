(in-package :gpm)

(defun E/N-edge (Te ng-deff h &optional (edge/average-ratio 2d0))
  "Calculate the plasma edge E/N as function of `Te',
`ng-deff' and `h'

The value of E/N is in Townsend 10^{-21} V m^2

`edge/average-ratio' is the ratio of edge to average electric
field (default 2)"
  (/ (* edge/average-ratio (/ Te ng-deff) (log h))
     1d-21))


(defun K-Ar/Ar (E/N)
  (K-E/N E/N *ar+/ar-interpolations*))

(defun K-Ar/Xe (E/N)
  "Ar+/Xe mobility is not known.  I calculate it using Xenon/argon,
and scaling by polarizabilities.  This could be total nonsense"
  (* (/ (rel-polarizability :Ar)
	(rel-polarizability :Xe))
     (K-E/N E/N *xe+/ar-interpolations*)))



(defun K-Xe/Ar (E/N)
  (K-E/N E/N *xe+/ar-interpolations*))

(defun K-Xe/Xe (E/N)
  (K-E/N E/N *xe+p12/xe-interpolations*))

(defun K-eff-Ar (E/N X-Ar X-Xe)
  (/ (+ (/ X-Ar (K-Ar/Ar E/N))
	(/ X-Xe (K-Ar/Xe E/N)))))

(defun K-eff-Xe (E/N X-Ar X-Xe)
  (/ (+ (/ X-Ar (K-Xe/Ar-A E/N))
	(/ X-Xe (K-Xe/Xe E/N)))))


(defun lambda-N (M K0 E/N)
  "Product lambda N

`M' -- molecular mass in amu
`K0' -- Mobility at 273.16K and 10132.5Pa
`E/N' -- reduced field

I do not take into account the temperature effects, as I do not know
how to do it yet "
  (* (/ (* +amu+ M)
	+q+)
     (expt (* 1d-4 +N0+ K0) 2)
     (* E/N-edge 1e-21)))