(defpackage :bricabrac.debug
  (:use :cl)
  (:export #:print-all))

(in-package :bricabrac.debug)

(defun print-all (&rest args)
  (print args))
