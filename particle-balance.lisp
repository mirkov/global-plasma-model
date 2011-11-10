;; Mirko Vukovic
;; Time-stamp: <2011-11-09 16:58:15 particle-balance.lisp>
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

(defun XI (ng-deff X K u)
  "Ion fraction as function of total gas density `ng', `deff', molar
gas fraction X, ionization rate K and bohm velocity u"
  (* ng-deff X K (/ u)))

(defun XI-Ar0 (ng-deff X Te)
  "Argon ion fraction as function of total gas density `ng', `deff',
Argon molar gas fraction X and temperature Te"
  (Xi ng-deff X (K-Ar+e->ion :phelps-maxwell Te) (Ub 40 Te)))


(defun XI-Xe0 (ng-deff X Te)
  "Xenon ion fraction as function of total gas density `ng', `deff',
Xenon molar gas fraction X and temperature Te"
  (Xi ng-deff X (K-Xe+e->ion :phelps-maxwell (coerce Te 'double-float))
      (Ub 131.29 Te)))

(defun Te-equation0 (Te ng-deff X-Ar X-Xe)
  "Equation for electron temperature. It returns zero when the sum of
ion fractions equals zero"
  (let ((XI-Ar (Xi-Ar0 ng-deff X-Ar Te))
	(XI-Xe (Xi-Xe0 ng-deff X-Xe Te)))
    (+ -1d0 XI-Ar XI-Xe)))

(let (ng-deff% X-Ar% X-Xe%)
  (defun Te-equation0% (Te%)
      (Te-equation0 Te% ng-deff% X-Ar% X-Xe%))
  (defun calc-te0 (ng-deff x-ar x-xe &optional print-steps)
    (setf ng-deff% ng-deff
	  X-Ar% X-Ar
	  X-Xe% X-Xe)
    (let ((max-iter 50)
	  (solver
	   (make-one-dimensional-root-solver-f +brent-fsolver+ 'Te-Equation0%
					       0.10001d0 90.0d0)))
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
