;; Mirko Vukovic
;; Time-stamp: <2011-11-04 17:31:31EDT energy-balance.lisp>
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


(defun Ei (Vs Te)
  "Kinetic energy of ions hitting the surface

L&L 10.2.8"
  (+ Vs (* 0.5d0 Te)))

(defun Ee (Te)
  "Energy of electron hitting a surface

L&L p. 332"
  (* 2d0 Te))

(defun energy/Ar+ (Vs Te)
  "Energy lost from the system per each Argon ion/electron pair

L&L (10.2.9)"
  (+ (energy-loss/Ar+ Te)
     (Ee Te)
     (Ei Vs Te)))

(defun Vs (Te M)
  "Floating sheath potential

L&L 10.2.4"
  (* Te (log (sqrt (/ (* +mksa-mass-proton+ M)
		      (* 2 pi +mksa-mass-electron+))))))

(defun energy/Xe+ (Vs Te)
  "Energy lost from the system per each Xenon ion/electron pair

L&L (10.2.9)"
  (+ (energy-loss/Xe+ Te)
     (Ee Te)
     (Ei Vs Te)))



(defun calc-ne (Pabs A Vs Xi-Ar Xi-Xe Te)
  "Calculate the electron density

L&L (!10.2.15)"
  (let ((u-Ar (ub 40 Te))
	(u-Xe (ub 131.29 Te))
	(E-Ar (energy/Ar+ Vs Te))
	(E-Xe (energy/Xe+ Vs Te)))
    (/ Pabs
       (* +mksa-electron-charge+ A
	  (+ (* Xi-Ar u-Ar E-Ar)
	     (* Xi-Xe u-Xe E-Xe))))))