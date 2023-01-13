(defpackage :bricabrac.mixins
  (:use :cl)
  (:import-from #:alexandria
                alexandria:with-gensyms)
  (:export #:has-name
           #:name

           #:has-parent
           #:parent
           #:do-walk-ancestry-up

           #:has-children
           #:children
           #:find-child-by-name))

(in-package :bricabrac.mixins)

(defclass has-name ()
  ((name
    :accessor name
    :initarg :name
    :initform (gensym #.(string :name)))))

(defmethod name ((s symbol)) s)
(defmethod name ((c class)) (class-name c))
(defmethod name ((p package)) (intern (package-name p) "KEYWORD"))

(defmethod print-object ((object has-name) stream)
  (print-unreadable-object (object stream)
    (format stream "[~a]" (name object))))

(defclass has-parent ()
  ((parent
    :accessor parent
    :initarg :parent
    :initform nil)))

(defmacro do-walk-ancestry-up ((node leaf &optional result) &body body)
  `(do ((,node ,leaf (parent ,node)))
       ((null ,node) ,result)
     ,@body))

(defclass has-children ()
  ((children
    :type vector
    :accessor children
    :initarg :children
    :initform (make-array 0
                          :adjustable t
                          :fill-pointer 0))))

(defun find-child-by-name (root child-name)
  (find child-name (children root) :key #'name))
