(in-package #:bricabrac.fold-environments.private)

(defvar *fold-mapping* nil
  "Object that maps keys to folder functions.

If bound to a list, it is assumed to be a property list and subject to
*FOLD-MAPPING-INTERPRETATION-MODE*")

(defvar *fold-mapping-interpretation-mode* nil
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
  (let ((mapping (fold-environments* mapping nil)))
    (or (resolve key mapping)
        (and env (find-fold-mapping (resolve .fold-mapping env)
                                    nil
                                    key)))))

(defmethod fold-for-key ((_ (eql .fold-mapping)) old new)
  (fold-environments old new))

(defgeneric generic-fold (mapping env key old new)
  (:method ((mapping list) env key old new)
    (let ((function (find-fold-mapping mapping env key)))
      (let ((*environment* (let* ((parent *environment*)
                                  (*environment* nil))
                             (fold-environments% parent env))))
        (typecase function
          (null (fold-for-key key old new))
          ((or function symbol)
           (funcall function old new))
          (t (interpret-fold-function *fold-mapping-interpretation-mode*
                                      function
                                      old
                                      new)))))))

(defun fold-environments% (old new &optional (mapping *fold-mapping*))
  "OLD must be a flat property list"
  (loop
    :for env := (copy-tree old) :then next
    :for (key new-val) :on new by #'cddr
    :for old-val := (resolve key env)
    :for fold := (generic-fold mapping env key old-val new-val)
    :for next := (augment env key fold)
    :finally (return env)))

(defun ensure-list-of-environments (env)
  (and env (if (consp (first env)) env (list env))))

(defun fold-environments* (environments &optional (mapping *fold-mapping*))
  (flet ((fold (o n) (fold-environments% o n mapping)))
    (reduce #'fold
            (ensure-list-of-environments environments)
            :initial-value nil)))

(defun fold-environments (old new &optional (mapping *fold-mapping*))
  (fold-environments* (append (ensure-list-of-environments old)
                              (ensure-list-of-environments new))
                      mapping))

;; (with-environment ((.fold-mapping '(:type (cons #:new #:old))))
;;            (with-environment ((:type '(:a integer))
;;                               (:type '(:b float)))
;;              (resolve :type)))
;; ((:B FLOAT) (:A INTEGER))
