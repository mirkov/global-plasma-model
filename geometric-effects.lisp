;; Mirko Vukovic
;; Time-stamp: <2011-11-08 13:22:26 geometric-effects.lisp>
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

(defun deff (R l &key (hr 0.3d0) (hl 0.3d0))
  "Effective length (10.2.13)"
  (* 0.5 (/ (+ (/ hl l)
	       (/ hr R)))))

(define-test deff
  (let ((lisp-unit:*epsilon* 1e-5))
    (assert-number-equal (* 0.5 (/ 0.6))
			 (deff 1 1))
    (assert-number-equal (* 0.5 2 (/ 0.9))
			 (deff 2 1))
    (assert-number-equal (* 0.5 2 (/ 0.9))
			 (deff 1 2))
    (assert-number-equal (* 0.5 2 (/ 1.1))
			 (deff 2 1 :hr 0.5))
    (assert-number-equal (* 0.5 2 (/ 1.1))
			 (deff 1 2 :hl 0.5))))
    
    
(defun Aeff (R l &key (hr 0.3d0) (hl 0.3d0))
  "Effective area

L&L (10.2.11)"
  (* 2 +pi+ R
     (+ (* R hl)
	(* l hr))))