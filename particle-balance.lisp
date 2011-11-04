;; Mirko Vukovic
;; Time-stamp: <2011-11-04 16:08:50EDT ion-fractions.lisp>
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

(defun XI-Ar (ng-deff X Te)
  "Argon ion fraction as function of total gas density `ng', `deff',
Argon molar gas fraction X and temperature Te"
  (Xi ng-deff X (K-Ar+e->ion :phelps-maxwell Te) (Ub 40 Te)))


(defun XI-Xe (ng-deff X Te)
  "Xenon ion fraction as function of total gas density `ng', `deff',
Xenon molar gas fraction X and temperature Te"
  (Xi ng-deff X (K-Xe+e->ion :phelps-maxwell (coerce Te 'double-float))
      (Ub 131.29 Te)))

(defun Te-equation (Te ng-deff X-ar X-xe)
  "Equation for electron temperature.  It returns zero when Te equals
the electron temperature"
  (- 1d0
     (* ng-deff (+ (/ (* X-Ar (K-Ar+e->ion :phelps-maxwell Te))
		      (Ub 40d0 Te))
		   (/ (* X-Xe (K-Xe+e->ion :phelps-maxwell Te))
		      (Ub 131.29d0 Te))))))

(let (ng-deff% X-Ar% X-Xe%)
  (defun Te-equation% (Te)
    (Te-equation Te ng-deff% x-ar% x-xe%))
  (defun calc-te (ng-deff x-ar x-xe &optional print-steps)
    (setf ng-deff% ng-deff
	  X-Ar% X-Ar
	  X-Xe% X-Xe)
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


(defun print-Te-calc-results (ng-deff X-Ar &optional (stream t))
  (let ((X-Xe (- 1d0 X-Ar))) 
    (let* ((te (calc-te ng-deff X-Ar X-Xe))
	   (Xi-Ar (Xi-Ar ng-deff X-Ar te))
	   (Xi-Xe (Xi-Xe ng-deff X-Xe te)))
      (format stream "ng deff: ~15t~a~%" ng-deff)
      (format stream "X_Ar: ~15t~5,3f~%" X-ar)
      (format stream "Te: ~15t~5,3f~%" te)
      (format stream "Xi_Ar:~14t~6,4f~%" Xi-Ar)
      (format stream "Xi_Xe:~14t~6,4f~%" Xi-Xe)
      (format stream "Xi_Ar+Xi_Xe:~14t~6,4f~%" (+ Xi-Ar Xi-Xe)))))
