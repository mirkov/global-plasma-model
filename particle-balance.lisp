;; Mirko Vukovic
;; Time-stamp: <2011-11-18 21:43:16 particle-balance.lisp>
;; 
;; Copyright 2011 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package :gpm)

(Defun xi1 (ng-deff X K u)
  "Ion fraction as function of total gas density `ng', `deff', molar
gas fraction X, ionization rate K and bohm velocity u"
  (* ng-deff X K (/ u)))

(defgeneric Xi (species ng-deff X Te)
  (:documentation 
   "`species' ion fraction as function of total gas density `ng' and
`deff' product, species molar gas fraction `X' and temperature Te

`species' is a keyword, :Ar, or :Xe
`X' is a number between 0 and 1 (inclusive)
`Te' is the electron temperature in eV")
  (:method ((species (eql :Ar)) ng-deff X Te)
    (Xi1 ng-deff X (K-Ar+e->ion :phelps-maxwell Te) (Ub +m-Ar+ Te)))
  (:method ((species (eql :Xe)) ng-deff X Te)
    (Xi1 ng-deff X (K-Xe+e->ion :phelps-maxwell (coerce Te 'double-float))
	 (Ub +m-Xe+ Te))))

(defun particle-balance-defect (Te ng-deff X-Ar X-Xe &optional Ub-Ar Ub-Xe Ti)
  "Calculate the difference between Xi_Ar + Xi_Xe and unity: (Xi_Ar + Xi_Xe -1)
as function of Te, ng-deff, X-Ar and X-Xe.
"
  (let (Xi-Ar Xi-Xe)
    (if *iisi-coupling*
	(let ((*max-iter* 100))
	  (destructuring-bind (i XI-Ar% XI-Xe% Ub-Ar Ub-Xe)
	      (calc-xi&ub Ng-deff X-Ar X-Xe Te Ti Ub-Ar Ub-Xe nil nil)
	    (declare (ignore i))
	    (setf *Ub-Ar* Ub-Ar
		  *Ub-Xe* Ub-Xe
		  *XI-Ar* Xi-Ar
		  *XI-Xe* Xi-Xe
		  Xi-Ar Xi-Ar%
		  Xi-Xe Xi-Xe%)))
	(setf XI-Ar (Xi :Ar ng-deff X-Ar Te)
	      XI-Xe (Xi :Xe ng-deff X-Xe Te)))
    (+ -1d0 XI-Ar XI-Xe)))

(let (ng-deff% X-Ar% X-Xe%)
  (defun Te-equation0% (Te%)
    (particle-balance-defect Te% ng-deff% X-Ar% X-Xe%))
  (defun calc-te (ng-deff x-ar x-xe &optional print-steps)

    (setf ng-deff% ng-deff
	  X-Ar% X-Ar
	  X-Xe% X-Xe)
    (let ((max-iter 50)
	  (*iisi-coupling* nil) ;; this does not seem to have an effect!!
	  (solver
	   (make-one-dimensional-root-solver-f +brent-fsolver+ 'Te-Equation0%
					       *Te-min* *Te-max*)))
      (when print-steps
	(format t "iter ~6t   [lower ~24tupper] ~36troot ~44terr ~54terr(est)~&"))
      (loop for iter from 0
	 for root = (solution solver)
	 for lower = (fsolver-lower solver)
	 for upper = (fsolver-upper solver)
	 do (iterate solver)
	 while  (and (< iter max-iter)
		     (not (root-test-interval lower upper 0.0d0 1d-6)))
	 do
	 (when print-steps
	   (format t "~d~6t~10,6f~18t~10,6f~28t~12,9f ~44t~10,4g ~10,4g~&"
		   iter lower upper
		   root (- root (sqrt 5.0d0))
		   (- upper lower)))
	 finally (progn
		   (setf *te-root-delta* (- root *te-root*)
			 *Te-root* root)
		   (return root))))))

(defun particle-balance-calculation0 (ng-deff X-Xe)
  "Perform a particle balance calculation using the standard model,
returning a list
'(Te Xi-Ar Xi-Xe)"
  (let ((X-Ar (- 1d0 X-Xe))) 
    (let* ((te (calc-te0 ng-deff X-Ar X-Xe)))
      (list Te (XI-Ar0 ng-deff X-Ar Te)
	    (Xi-Xe0 ng-deff X-Xe Te)))))

(defun print-Te-calc-results0 (ng-deff X-Xe &optional (stream t))
  (destructuring-bind (Te Xi-Ar Xi-Xe)
      (particle-balance-calculation0 ng-deff X-Xe)
    (format stream "ng deff: ~15t~a~%" ng-deff)
    (format stream "X_Xe: ~15t~5,3f~%" X-Xe)
    (format stream "Te: ~15t~5,3f~%" te)
    (format stream "Xi_Ar:~14t~6,4f~%" Xi-Ar)
    (format stream "Xi_Xe:~14t~6,4f~%" Xi-Xe)
    (format stream "Xi_Ar+Xi_Xe:~14t~6,4f~%" (+ Xi-Ar Xi-Xe))))

