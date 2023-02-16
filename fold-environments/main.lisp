(in-package #:bricabrac.fold-environments.private)

(defvar *fold-mapping* nil
  "Object that maps keys to folder functions.

If bound to a list, it is assumed to be a property list and subject to
*FOLD-MAPPING-INTERPRETATION-MODE*")

(defvar *fold-mapping-interpretation-mode*
  .simple
  "See INTERPRET-FOLD-FUNCTION and BRICABRAC.FOLD-ENVIRONMENTS.MODES package")

(defgeneric fold-for-key (key old new)
  (:method (_ old new)
    "Default method: NEW shadows OLD"
    new))

(defgeneric interpret-fold-function (mode function old new)
  (:method ((_ null) input old new)
    (error "Invalid input ~s" input)))

(define-local-keyword .fold-mapping)

(defun find-fold-mapping (mapping env key)
  (or (and env
           (find-fold-mapping (resolve .fold-mapping env)
                              nil
                              key))
      (resolve key (fold-environments* mapping nil))))

(defmethod fold-for-key ((_ (eql .fold-mapping)) old new)
  (fold-environments old new))

(defgeneric generic-fold (mapping env key old new)
  (:method ((mapping list) env key old new)
    (let ((function (find-fold-mapping mapping env key)))
      (let ((*environment* env))
        (typecase function
          (null (fold-for-key key old new))
          ((or function symbol)
           (funcall function old new))
          (t (interpret-fold-function *fold-mapping-interpretation-mode*
                                      function
                                      old
                                      new)))))))

(defun ensure-flat-environment (env)
  (if (consp (first env)) (reduce #'append env) env))

(defun fold-environments% (old new &optional (mapping *fold-mapping*))
  "OLD must be a flat property list"
  (loop
    :for env := old :then next
    :for (key new-val) :on new by #'cddr
    :for old-val := (resolve key env)
    :for fold := (generic-fold mapping env key old-val new-val)
    :for next := (augment env key fold)
    :finally (return env)))

(defun ensure-list-of-environments (env)
  (and env (if (consp (first env)) env (list env))))

(defun fold-environments* (environments &optional (mapping *fold-mapping*) initial)
  (flet ((fold (o n) (fold-environments% o n mapping)))
    (reduce #'fold
            (ensure-list-of-environments environments)
            :initial-value initial)))

(defun fold-environments (old new &optional (mapping *fold-mapping*))
  (fold-environments* new mapping (ensure-flat-environment old)))

(defmacro foldf (place &rest env)
  `(setf ,place (fold-environments ,place (list ,@env))))
