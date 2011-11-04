;; Mirko Vukovic
;; Time-stamp: <2011-11-04 17:54:41EDT example1.lisp>
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

;; provide the three examples from L&L Section 10.2

(defun example1 (&optional (stream t))
  "Walk through Example 1 of the global plasma model

L&L p. 335"
  (let ((R 0.15)
	(L 0.3)
	(ng 3.3e19)
	(hl 0.31)
	(hr 0.27)
	(Pabs 800))
    (format stream "Problem parameters:~%")
    (format stream "R: ~15t~5,3f~%" R)
    (format stream "L: ~15t~5,3f~%" L)
    (format stream "ng: ~15t~5,3e~%" ng)
    (format stream "pabs: ~15t~5,3e~%" Pabs)
    (format stream "hl: ~15t~5,3f~%" hl)
    (format stream "hr: ~15t~5,3f~%" hr)
    (let* ((deff (deff r l :hr hr :hl hl))
	   (Te (calc-Te (* ng deff) 1.0 0.0)))
      (format stream "deff: ~15t~5,3f~%" deff)
      (format stream "Te: ~15t~5,3f~%" Te)
      (let ((Vs (Vs Te 40)))
	(format stream "Vs: ~15t~5,3f~%" Vs)
      (let ((Ei (Ei Vs Te))
	    (Ee (Ee Te))
	    (Ec (energy-loss/Ar+ Te))
	    (Et (energy/Ar+ Vs Te)))
	(format stream "Ei: ~15t~5,3f~%" Ei)
	(format stream "Ee: ~15t~5,3f~%" Ee)
	(format stream "Ec: ~15t~5,3f~%" Ec)
	(format stream "Et: ~15t~5,3f~%" Et)
	(let ((Aeff (Aeff R L :hr hr :hl hl))
	      (ub (ub 40 Te)))
	  (format stream "Aeff: ~15t~5,3f~%" Aeff)
	  (format stream "ub: ~15t~5,3e~%" ub)
	  (let ((ne (calc-ne Pabs Aeff Vs 1.0 0.0 Te)))
	    (format stream "ne: ~15t~5,3e~%" ne))))))))

(defun example2 (&optional (stream t))
  "Walk through Example 1 of the global plasma model

L&L p. 335"
  (let ((R 0.15)
	(L 0.3)
	(ng 3.3e19)
	(hl 0.31)
	(hr 0.0)
	(Pabs 800))
    (format stream "Problem parameters:~%")
    (format stream "R: ~15t~5,3f~%" R)
    (format stream "L: ~15t~5,3f~%" L)
    (format stream "ng: ~15t~5,3e~%" ng)
    (format stream "pabs: ~15t~5,3e~%" Pabs)
    (format stream "hl: ~15t~5,3f~%" hl)
    (format stream "hr: ~15t~5,3f~%" hr)
    (let* ((deff (deff r l :hr hr :hl hl))
	   (Te (calc-Te (* ng deff) 1.0 0.0)))
      (format stream "deff: ~15t~5,3f~%" deff)
      (format stream "Te: ~15t~5,3f~%" Te)
      (let ((Vs (Vs Te 40)))
	(format stream "Vs: ~15t~5,3f~%" Vs)
      (let ((Ei (Ei Vs Te))
	    (Ee (Ee Te))
	    (Ec (energy-loss/Ar+ Te))
	    (Et (energy/Ar+ Vs Te)))
	(format stream "Ei: ~15t~5,3f~%" Ei)
	(format stream "Ee: ~15t~5,3f~%" Ee)
	(format stream "Ec: ~15t~5,3f~%" Ec)
	(format stream "Et: ~15t~5,3f~%" Et)
	(let ((Aeff (Aeff R L :hr hr :hl hl))
	      (ub (ub 40 Te)))
	  (format stream "Aeff: ~15t~5,3f~%" Aeff)
	  (format stream "ub: ~15t~5,3e~%" ub)
	  (let ((ne (calc-ne Pabs Aeff Vs 1.0 0.0 Te)))
	    (format stream "ne: ~15t~5,3e~%" ne))))))))

(defun example3 (&optional (stream t))
  "Walk through Example 1 of the global plasma model

L&L p. 335"
  (let ((R 0.15)
	(L 0.3)
	(ng 3.3e19)
	(hl 0.31)
	(hr 1.0)
	(Pabs 800)
	(Vs 500))
    (format stream "Problem parameters:~%")
    (format stream "R: ~15t~5,3f~%" R)
    (format stream "L: ~15t~5,3f~%" L)
    (format stream "ng: ~15t~5,3e~%" ng)
    (format stream "pabs: ~15t~5,3e~%" Pabs)
    (format stream "hl: ~15t~5,3f~%" hl)
    (format stream "hr: ~15t~5,3f~%" hr)
    (format stream "Vs: ~15t~5,3f~%" Vs)
    (let* ((deff (deff r l :hr hr :hl hl))
	   (Te (calc-Te (* ng deff) 1.0 0.0)))
      (format stream "deff: ~15t~5,3f~%" deff)
      (format stream "Te: ~15t~5,3f~%" Te)
      (let ((Ei (Ei Vs Te))
	    (Ee (Ee Te))
	    (Ec (energy-loss/Ar+ Te))
	    (Et (energy/Ar+ Vs Te)))
	(format stream "Ei: ~15t~5,3f~%" Ei)
	(format stream "Ee: ~15t~5,3f~%" Ee)
	(format stream "Ec: ~15t~5,3f~%" Ec)
	(format stream "Et: ~15t~5,3f~%" Et)
	(let ((Aeff (Aeff R L :hr hr :hl hl))
	      (ub (ub 40 Te)))
	  (format stream "Aeff: ~15t~5,3f~%" Aeff)
	  (format stream "ub: ~15t~5,3e~%" ub)
	  (let ((ne (calc-ne Pabs Aeff Vs 1.0 0.0 Te)))
	    (format stream "ne: ~15t~5,3e~%" ne)))))))

