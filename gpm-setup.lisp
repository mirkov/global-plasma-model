;; Mirko Vukovic
;; Time-stamp: <2011-11-18 19:48:20 gpm-setup.lisp>
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

(defconstant +pi+ (coerce pi 'double-float)
  "pi to double precision")

(define-symbol-macro +q+ physics-constants:+elementary-charge-sp+)
(define-symbol-macro +amu+ physics-constants:+proton-mass-sp+)

(defconstant +M-Ar+ 40.0 "Argon Mass")
(defconstant +M-Xe+ 131.29 "Xenon Mass")

(defparameter *Te-min* 1.00001d-1
  "Default minimum electron temperature used for solving for Te")

(defparameter *Te-max* 99.999d-1
  "Default maximum electron temperature used for solving for Te")

(defparameter *Te-root* nil
  "Last root of the equation for Te found by the root finder")

(defparameter *Te-root-delta* nil
  "Difference between the previous found root and the new one")

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
