(defpackage :bricabrac.internal-time
  (:use :cl)
  (:export #:duration))

(in-package :bricabrac.internal-time)

(defun internal- (amount &key (unit :seconds)))
