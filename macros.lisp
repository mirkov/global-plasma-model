;; Mirko Vukovic
;; Time-stamp: <2011-11-20 11:32:38 macros.lisp>
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

(defun converge (fun arg0 &key (max-iter 50)
		 (tol 1e-3) trace
		 (warn-exceeded-max-iter t))
  "Execute function on its result until the relative changes converge
within tol

Return as multiple values, the converged result, the iteration count,
and fractional change at last iteration"
  (let* ((val0 (funcall fun arg0))
	 (val val0)
	 change
	 (count 0))
    (symbol-macrolet ((trace-step
		       (when trace
			 (format t "~a:~10t~a~20t~a~30t~a~%" count val val0 change))))
      (when trace
	(format t "Iter ~11tval_i~21tval_i-1~31tDelta~%"))
      (tagbody loop-top
	 (when trace
	   (format t "~a:~10t~a~20t~a~30t~a~%" count val val0 change))
	 (setf val0 val
	       val (funcall fun val)
	       change (abs (/ (- val0 val)
			      val)))
	 (incf count)
	 (when (or (>= count max-iter)
		   (< change tol))
	   (when trace
	     (format t "~a:~10t~a~20t~a~30t~a~%" count val val0 change))
	   (go loop-exit))
	 (go loop-top)
       loop-exit)
      (assert (not (and warn-exceeded-max-iter
			(> count max-iter)))
	      ()
	    "Convergence exceeded maximum iteration count")
      (return-from converge (values val count change)))))


(defun sqrt-convergence (x)
 (sqrt x))

(defun log-convergence (x)
  (log x))

#| (converge #'log-convergence
20 :trace nil :tol 1e-7 :max-iter 10 :warn-exceeded-max-iter nil)

(converge #'sqrt-convergence
20d0 :trace t :tol 1e-7 :max-iter 200)|#