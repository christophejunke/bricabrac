(defpackage :bricabrac.utils
  (:use :cl)
  (:export #:with-path
           #:nmapcar))

(in-package :bricabrac.utils)

(defmacro with-path (path &body body)
  `(let ((*default-pathname-defaults* (merge-pathnames ,path)))
     ,@body))

(defun nmapcar (function seq &rest more-seqs)
  (apply #'map-into seq function seq more-seqs))
