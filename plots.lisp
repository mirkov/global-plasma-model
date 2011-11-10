;; Mirko Vukovic
;; Time-stamp: <2011-11-10 11:00:05 plots.lisp>
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

(in-package :gpm-user)

(defmacro plot-vs-X-Xe ((ng-deff Ti) &body body)
  `(let* ((ng-deff ,ng-deff)
	  (Ti ,Ti)
	  (X-Xe (gseq 1d-3 0.999d0))
	  (X-Ar (gmap #'(lambda (X-Xe) (- 1d0 X-Xe)) X-Xe)))
     (grid-bind (Te0 XI-Ar0 Xi-Xe0)
	 (gpmap (particle-balance-calculation0 ng-deff @!Xi-Xe) X-Xe)
       (grid-bind (Te XI-Ar Xi-Xe)
	   (gpmap (particle-balance-calculation ng-deff @!Xi-Xe Ti) X-Xe)
	 (plot-xys X-Xe ,@body)))))

#|

(progn
  (init-gnuplot)
  (start-gnuplot))

(plot-vs-x-xe (1e20 .04)
  (list (list Te0 :title "Classic")
	(list Te :title "New")))


(set-to ((title "Ar+/ne and Xe+/ne fraction as function of Xe fraction")
	 (xlabel "Xe fraction"))
  (plot-vs-x-xe (1e20 0.04)
    (let ((XI-Xe0 (gpmap (XI-Ar0 ng-deff @!X-Ar @!Te) X-Ar Te0))
	  (XI-Ar0 (gpmap (XI-Xe0 ng-deff @!X-Xe @!Te) X-Xe Te0)))
      (grid-bind (i Xi-Ar Xi-Xe Ub-Ar Ub-Xe)
	  (gpmap (calc-xi&ub ng-deff @!X-Ar @!X-Xe @!Te Ti 1e5 1e5)
		 X-Ar X-Xe Te)
	(list (list Xi-Xe0 :title "Xe-classic")
	      (list Xi-Ar0 :title "Ar-classic")
	      (list Xi-xe :title "Xe-new")
	      (list Xi-Ar :title "Ar-new"))))))


(set-to ((title "Bohm velocities as function of Xe fraction")
	 (ylabel "[m/s]")
	 (xlabel "Xe fraction"))
  (plot-vs-x-xe (1e20 0.04)
    (let ((UXe0 (gpmap (gpm::ub 132 @!Te0) Te0))
	  (UAr0 (gpmap (gpm::ub 40 @!Te0) Te0)))
      (grid-bind (UAr UXe)
	  (gpmap (v1/2 @!XI-Ar @!XI-Xe 40 132 @!Te 0.04)
		 XI-Ar XI-Xe Te)
	(list (list UXe0 :title "Xe-classic")
	      (list UAr0 :title "Ar-classic")
	      (list Uxe :title "Xe-new")
	      (list UAr :title "Ar-new"))))))

(set-to ((title "Plasma density as function of Xe fraction")
	 (ylabel "n_e [m^-3]")
	 (xlabel "Xenon Fraction")) 
  (plot-vs-x-xe (1e21 0.04)
    (let* ((XI-Ar0 (gpmap (XI-Ar0 ng-deff @!X-Ar @!Te) X-Ar Te0))
	   (XI-Xe0 (gpmap (XI-Xe0 ng-deff @!X-Xe @!Te) X-Xe Te0))
	   (ne0 (gpmap (calc-ne0 1e3 2e-2 10.0 @!Xi-Ar0 @!Xi-Xe0 @!Te0)
		       Xi-Ar0 Xi-Xe0 Te0)))
      (let ((ne (gpmap (calc-ne ng-deff @!X-Ar @!X-Xe @!Te Ti 1e3 2e-2 10.0) 
		       X-Ar X-Xe Te)))
	(list (list ne0 :title "classic")
	      (list ne :title "new"))))))





|#