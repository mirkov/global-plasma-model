;; Mirko Vukovic
;; Time-stamp: <2011-11-09 13:47:54 particle-balance-new.lisp>
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
;; provide methods to calculate the particle blance using Baalrud's
;; theory for the Bohm criterion

(defparameter *tolerance* 1e-6
  "Tolerance for numeric iterative solutions")
(defparameter *max-iter* 50
  "Maximum number of iterations")
(defparameter *IISI-coupling* t
  "Turn on ion-ion coupling model")

(defvar *Ub-Ar* nil
  "Internal variable for storing the calculated Argon Bohm velocity across
functions")

(defvar *Ub-Xe* nil
  "Internal variable for storing the calculated Xenon Bohm velocity across
functions")

(defvar *XI-Ar* nil
  "Internal variable for storing the calculated Argon ion fractional
density across functions")

(defvar *XI-Xe* nil
  "Internal variable for storing the calculated Xenon ion factional
density across functions")

(defun calc-xi&ub (ng-deff X-ar X-Xe Te Ti Ub-Ar Ub-Xe
		   &optional print-steps (warn-iteration-count t))
  "Iterate until convergence over the XI-Ar, XI-Xe, and V1/2
calculation for a given ng-deff, X-Ar,X-Xe, Te and Ti.

At convergence, the sum of Xi-Ar and Xi-Xe need not equal unity.  It
is unity when Te is correct.

Returns a list contentaining: iteration count, XI-Ar, XI-Xe, Ub-Ar, Ub-Xe"
  (when print-steps
    (format t "Iteration~12tXI-Ar~22tXI-Xe~33TUb-Ar~43tUB-Xe~%"))
  (let* (#|(Ub-Ar (if Ub-Ar Ub-Ar (gpm::ub 40 Te)))
	 (Ub-Xe (if Ub-Xe Ub-Xe (gpm::ub 132 Te)))|#
	 XI-Ar Xi-Xe
	 (res
	  (do ((ref-i-1 1d0 ref-i)
	       (ref-i Ub-Ar Ub-Ar)
	       (i 0 (1+ i)))
	      ((or
		(< (/ (abs (- ref-i ref-i-1))
		      ref-i-1)
		   *tolerance*)
		(> i *max-iter*))
	       (list i Xi-Ar Xi-Xe Ub-Ar Ub-Xe))
	    (setf XI-Ar (XI ng-deff X-Ar (K-Ar+e->ion :phelps-maxwell Te) Ub-Ar)
		  XI-Xe (XI ng-deff X-Xe (K-Xe+e->ion :phelps-maxwell Te) Ub-Xe))
	    (destructuring-bind
		  (Ub-Ar% Ub-Xe%)
		(if *iisi-coupling*
		    (v1/2 xi-ar xi-xe 40 132 Te Ti)
		    (let ((Ub-Ar (gpm::Ub 40d0 Te))
			  (Ub-Xe (gpm::Ub 131.29d0 Te)))
		      (list Ub-Ar Ub-Xe)))
	      (setf Ub-Ar Ub-Ar%
		    Ub-Xe Ub-Xe%))
	    (when print-steps
	      (format t "~a: ~12t~5,3f~22t~5,3f~32t~5,3e~42t~5,3e~%"
		      i XI-Ar XI-Xe Ub-Ar Ub-Xe)))))
    (when warn-iteration-count
      (assert (<= (first res) *max-iter*)
	      ()
	      "Exceeded maximum interation count.  Values are ~a" res))
    res))

(defun Te-equation (Te ng-deff X-Ar X-Xe Ub-Ar Ub-Xe Ti)
  "Equation for electron temperature.  It returns zero when Te equals
the electron temperature

We calculate ion fractions using calc-xi&ub, and return 1-(XI-Ar + XI-Xe)
"
  (- 1d0
     (let ((*max-iter* 1))
     (destructuring-bind (i XI-Ar XI-Xe Ub-Ar Ub-Xe)
	 (calc-xi&ub Ng-deff X-Ar X-Xe Te Ti Ub-Ar Ub-Xe nil nil)
       (declare (ignore i))
       (setf *Ub-Ar* Ub-Ar
	     *Ub-Xe* Ub-Xe
	     *XI-Ar* Xi-Ar
	     *XI-Xe* Xi-Xe)
       (+ XI-Ar XI-Xe)))))


(let (ng-deff% X-Ar% X-Xe% Ti%)
  (defun Te-equation% (Te)
    (Te-equation Te ng-deff% x-ar% x-xe% *Ub-Ar* *Ub-Xe* Ti% ))
  (defun calc-te (ng-deff x-ar x-xe Ti &optional print-steps)
    (setf ng-deff% ng-deff
	  X-Ar% X-Ar
	  X-Xe% X-Xe
	  Ti% Ti
	  *Ub-Ar* (gpm::ub 3d0 40)
	  *Ub-Xe* (gpm::ub 3d0 132d0))
    (let ((max-iter 50)
	  (solver
	   (make-one-dimensional-root-solver-f +brent-fsolver+ 'Te-Equation%
					       0.10001d0 10.0d0)))
      (when print-steps
	(format t "iter ~6t   [lower ~24tupper] ~36troot ~44terr ~54terr(est)~&"))
      (loop for iter from 0
	 for root = (solution solver)
	 for lower = (fsolver-lower solver)
	 for upper = (fsolver-upper solver)
	 do (iterate solver)
	 while  (and (< iter max-iter)
		     (not (root-test-interval lower upper 0.0d0 0.001d0)))
	 do
	 (when print-steps
	   (format t "~d~6t~10,6f~18t~10,6f~28t~12,9f ~44t~10,4g ~10,4g~&"
		   iter lower upper
		   root (- root (sqrt 5.0d0))
		   (- upper lower)))
	 finally (return root)))))


(defun particle-balance-calculation (ng-deff X-Ar Ti)
  "Perform a particle balance calculation, returning a list
'(Te Xi-Ar Xi-Xe)"
  (let ((X-Xe (- 1d0 X-Ar))) 
    (let* ((te (calc-te ng-deff X-Ar X-Xe Ti)))
      (list Te *XI-Ar* *Xi-Xe*))))

(defun print-Te-calc-results (ng-deff X-Ar Ti &optional (stream t))
  (destructuring-bind (Te Xi-Ar Xi-Xe)
      (particle-balance-calculation ng-deff X-Ar Ti)
    (format stream "ng deff: ~15t~a~%" ng-deff)
    (format stream "X_Ar: ~15t~5,3f~%" X-ar)
    (format stream "Te: ~15t~5,3f~%" te)
    (format stream "Xi_Ar:~14t~6,4f~%" Xi-Ar)
    (format stream "Xi_Xe:~14t~6,4f~%" Xi-Xe)
    (format stream "Xi_Ar+Xi_Xe:~14t~6,4f~%" (+ *Xi-Ar* *Xi-Xe*))))
