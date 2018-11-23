(defpackage :bricabrac.internal-time
  (:use :cl)
  (:export #:duration))

(in-package :bricabrac.internal-time)

(defun seconds (seconds)
  (/ seconds internal-time-units-per-second))

