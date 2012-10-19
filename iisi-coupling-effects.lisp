;; Mirko Vukovic
;; Time-stamp: <2011-11-20 11:32:12 iisi-coupling-effects.lisp>
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


(let (ng-deff% Te% Ti%
	       X-Ar% X-Xe%
	       XI-Ar% XI-Xe%
	       Ub-Ar% Ub-Xe%)
  (defun setup-XI-Ar-iteration (ng-deff Te Ti
				X-Ar X-Xe
				XI-Ar XI-Xe
				Ub-Ar Ub-Xe)
    (setf ng-deff% ng-deff
	  Te% Te
	  Ti% Ti
	  X-Ar% X-Ar
	  X-Xe% X-Xe
	  XI-Ar% XI-Ar
	  XI-Xe% XI-Xe
	  Ub-Ar% Ub-Ar
	  Ub-Xe% Ub-Xe))
  (defun XI-Ar-iteration-values ()
    (values XI-Ar% XI-Xe% Ub-Ar% Ub-Xe%))
  (defun iterate-XI-Ar (XI-Ar)
    "Using `XI-Ar', and externally bound ng-deff%, X-Ar%, X-Xe%, XI-Xe%, 
Ub-Ar%, Ub-Xe%, ng-deff%,Te%, and Ti%, do:
1) calculate Ub-Ar% and Ub-Xe% using the iisi model (function `V1/2')
2) recalculate XI-Ar and XI-Xe using the particle balance equation (function
   `XI1')
3) Store the calculated values of XI-Ar% XI-Xe% Ub-Ar% Ub-Xe% and return 
   the value of XI-Ar%

At convergence, the returned value of XI-Ar% should equal the argument
XI-Ar

The function `V1/2' uses the value of *iisi-coupling* to determine
whether to use the ion-ion-sheath instability method to calculate the
ion velocities, or just return the individual species Bohm velocities.

This can feature can be used in the testing of `iterate-XI-Ar' by
running the results of the classic model in `iterate-Xi-Ar'.  It
should return the value of the argument.
"
    (destructuring-bind
	  (Ub-Ar Ub-Xe)
	(v1/2 XI-Ar XI-Xe% +m-ar+ +m-xe+ Te% Ti%
	      :classic-p (not *iisi-coupling*))
      (setf Ub-Ar% Ub-Ar
	    Ub-Xe% Ub-Xe
	    XI-Ar% (XI1 ng-deff% X-Ar% (K-Ar+e->ion :phelps-maxwell
						    (coerce Te% 'double-float))
			Ub-Ar%)
	    XI-Xe% (XI1 ng-deff% X-Xe% (K-Xe+e->ion :phelps-maxwell
						    (coerce Te% 'double-float))
			Ub-Xe%))
      Xi-Ar%)))

(define-test iterate-XI-Ar
  "Check the function for the case of classic sheath"
  (let* ((*iisi-coupling* nil)
	 (ng-deff 1e20)
	 (X-Ar 0.9)
	 (X-Xe 0.1)
	 (Ti nil)
	 (Te (calc-te ng-deff X-Ar X-Xe))
	 (Xi-Ar (XI :Ar ng-deff X-Ar Te))
	 (Xi-Xe (XI :Xe ng-deff X-Xe Te))
	 (Ub-Ar (Ub +m-Ar+ Te))
	 (Ub-Xe (Ub +m-Xe+ Te)))
    (setup-xi-ar-iteration ng-deff te ti X-Ar X-Xe Xi-Ar Xi-Xe Ub-Ar Ub-Xe)
    (assert-number-equal Xi-Ar (iterate-XI-Ar Xi-Ar))))


(defun calc-xi&ub (ng-deff X-ar X-Xe Te Ti Ub-Ar Ub-Xe
		   &optional print-steps (warn-iteration-count t))
  "Iterate until convergence over the XI-Ar, XI-Xe, and V1/2
calculation for a given ng-deff, X-Ar,X-Xe, Te and Ti.

At convergence, the sum of Xi-Ar and Xi-Xe need not equal unity.  It
is unity when Te is correct.

Returns a list contentaining: iteration count, XI-Ar, XI-Xe, Ub-Ar, Ub-Xe"
  (let ((XI-Ar (XI :Ar ng-deff X-Ar Te))
	(Xi-Xe (XI :Xe ng-deff X-Ar Te)))
    (setup-xi-ar-iteration ng-deff te ti X-Ar X-Xe
			   XI-Ar XI-Xe
			   Ub-Ar Ub-Xe)
    (multiple-value-bind (XI-Ar-res iter-count delta)
	(converge #'iterate-XI-Ar XI-Ar
		  :max-iter *max-iter* :trace print-steps :tol *tolerance*
		  :warn-exceeded-max-iter warn-iteration-count)
      (multiple-value-bind
	    (XI-Ar% XI-Xe% Ub-Ar% Ub-Xe% )
	  (xi-ar-iteration-values)
	(declare (ignore Xi-Ar%))
	(list iter-count XI-Ar-res XI-Xe% Ub-Ar% Ub-Xe% delta)))))

(define-test calc-xi&ub
  "Check that calc-xi&ub converges within the prescribed tolerance"
  (let* ((*iisi-coupling* nil)
	 (ng-deff 1e20)
	 (X-Ar 0.9)
	 (X-Xe 0.1)
	 (Te (calc-te ng-deff X-Ar X-Xe)))
    (let ((*iisi-coupling* t)
	  (Ti 0.04)
	  (Xi-Ar (XI :Ar ng-deff X-Ar Te))
	  (Xi-Xe (XI :Xe ng-deff X-Xe Te))
	  (Ub-Ar (Ub +m-Ar+ Te))
	  (Ub-Xe (Ub +m-Xe+ Te)))
      (destructuring-bind (iter-count% XI-Ar% XI-Xe% Ub-Ar% Ub-Xe% delta)
	  (calc-xi&ub ng-deff X-Ar X-Xe Te Ti Ub-Ar Ub-Xe)
	(declare (ignore iter-count% XI-Ar% XI-Xe% Ub-Ar% Ub-Xe%))
	(assert-true (< delta *tolerance*))))))


  
#|(defun calc-xi&ub (ng-deff X-ar X-Xe Te Ti Ub-Ar Ub-Xe
		   &optional print-steps (warn-iteration-count t))
  "Iterate until convergence over the XI-Ar, XI-Xe, and V1/2
calculation for a given ng-deff, X-Ar,X-Xe, Te and Ti.

At convergence, the sum of Xi-Ar and Xi-Xe need not equal unity.  It
is unity when Te is correct.

Returns a list contentaining: iteration count, XI-Ar, XI-Xe, Ub-Ar, Ub-Xe"
  (when print-steps
    (format t "Iteration~12tXI-Ar~22tXI-Xe~33TUb-Ar~43tUB-Xe~%"))
  (let* (XI-Ar Xi-Xe
	 (res
	  (do ((ref-i-1 1d0 ref-i)
	       (ref-i Ub-Ar Ub-Ar)
	       (i 0 (1+ i)))
	      ((or
		(< (/ (abs (- ref-i ref-i-1))
		      ref-i)
		   *tolerance*)
		(> i *max-iter*))
	       (list i Xi-Ar Xi-Xe Ub-Ar Ub-Xe))
	    (setf XI-Ar (XI :Ar ng-deff X-Ar Te)
		  XI-Xe (XI :Xe ng-deff X-Xe Te))
	    (destructuring-bind
		  (Ub-Ar% Ub-Xe%)
		(mapcar #'(lambda (Ub-cm/s)
			    (* 1e-2 Ub-cm/s))
		   (v1/2 xi-ar xi-xe +m-ar+ +m-xe+ Te Ti
			 :classic-p (not *iisi-coupling*)))
	      (setf Ub-Ar Ub-Ar%
		    Ub-Xe Ub-Xe%))
	    (when print-steps
	      (format t "~a: ~12t~5,3f~22t~5,3f~32t~5,3e~42t~5,3e~%"
		      i XI-Ar XI-Xe Ub-Ar Ub-Xe)))))
    (when warn-iteration-count
      (assert (<= (first res) *max-iter*)
	      ()
	      "Exceeded maximum interation count.  Values are ~a" res))
    res))|#




(let (ng-deff% X-Ar% X-Xe% Ti%)
  (defun Te-equation-iisi% (Te)
    (Te-equation Te ng-deff% x-ar% x-xe% *Ub-Ar* *Ub-Xe* Ti% ))
  (defun calc-te-iisi (ng-deff x-ar x-xe Ti &optional print-steps)
    (setf ng-deff% ng-deff
	  X-Ar% X-Ar
	  X-Xe% X-Xe
	  Ti% Ti
	  *Ub-Ar* (gpm::ub 3d0 40)
	  *Ub-Xe* (gpm::ub 3d0 132d0))
    (let ((max-iter 50)
	  (solver
	   (make-one-dimensional-root-solver-f +brent-fsolver+
					       'Te-Equation-iisi%
					       *te-min* *te-max*)))
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


(defun particle-balance-calculation-iisi (ng-deff X-Xe Ti)
  "Perform a particle balance calculation, returning a list
'(Te Xi-Ar Xi-Xe)"
  (let ((X-Ar (- 1d0 X-Xe))) 
    (let* ((te (calc-te ng-deff X-Ar X-Xe Ti)))
      (list Te *XI-Ar* *Xi-Xe*))))

(defun print-Te-calc-results-iisi (ng-deff X-Xe Ti &optional (stream t))
  (destructuring-bind (Te Xi-Ar Xi-Xe)
      (particle-balance-calculation ng-deff X-Xe Ti)
    (format stream "ng deff: ~15t~a~%" ng-deff)
    (format stream "X_Xe: ~15t~5,3f~%" X-Xe)
    (format stream "Te: ~15t~5,3f~%" te)
    (format stream "Xi_Ar:~14t~6,4f~%" Xi-Ar)
    (format stream "Xi_Xe:~14t~6,4f~%" Xi-Xe)
    (format stream "Xi_Ar+Xi_Xe:~14t~6,4f~%" (+ *Xi-Ar* *Xi-Xe*))))
