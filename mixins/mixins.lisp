(defpackage :bricabrac.mixins
  (:use :cl)
  (:export #:has-name
           #:name

           #:has-parent
           #:parent

           #:has-children
           #:children))

(in-package :bricabrac.mixins)

(defclass has-name ()
  ((name
    :accessor name
    :initarg :name
    :initform (gensym #.(string :name)))))

(defmethod name ((s symbol)) s)
(defmethod name ((c class)) (class-name c))
(defmethod name ((p package)) (intern (package-name p) "KEYWORD"))

(defclass has-parent ()
  ((parent
    :accessor parent
    :initarg :parent
    :initform nil)))

(defclass has-children ()
  ((children
    :type vector
    :accessor children
    :initarg :children
    :initform (make-array 0
                          :adjustable t
                          :fill-pointer 0))))
