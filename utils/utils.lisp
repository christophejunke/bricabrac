(defpackage :bricabrac.utils
  (:use :cl)
  (:export #:with-path
           #:nmapcar
           #:define-local-keyword))

(in-package :bricabrac.utils)

(defmacro with-path (path &body body)
  `(let ((*default-pathname-defaults* (merge-pathnames ,path)))
     ,@body))

(defun nmapcar (function seq &rest more-seqs)
  (apply #'map-into seq function seq more-seqs))

(defmacro define-local-keyword (symbol &key (documentation "") data)
  (check-type symbol symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (prog1 ',symbol
       (defparameter ,symbol ',symbol ,documentation)
       (setf (get ',symbol 'local-keyword-data) ,data))))
