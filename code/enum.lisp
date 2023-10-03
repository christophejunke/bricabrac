(defpackage :enum (:use))
(defpackage :enum.impl (:use :cl :enum))

(in-package :enum.impl)

(defclass literal ()
  ((enum :accessor enum :initarg :enum)
   (name :accessor name :initarg :name)
   (value :accessor value :initarg :value)))

(defgeneric make-literal-value (enum value)
  (:method (_ v) v))

(defgeneric make-literal (enum name value)
  (:method (enum name value)
    (make-instance 'literal
                   :name name
                   :value (make-literal-value enum value)
                   :enum enum)))

(defclass enum ()
  ((name :accessor name :initarg :name)
   (literals :accessor literals :initarg :literals)
   (constructor :accessor constructor
                :initarg :constructor
                :initform #'make-literal)))

(let ((e (allocate-instance (find-class 'enum))))
  (initialize-instance e :name 'test)
  (let ((a (make-literal e :a 0))
        (b (make-literal e :b 1)))
    (initialize-instance e :literals (vector a b))))
