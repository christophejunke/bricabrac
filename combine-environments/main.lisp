(in-package #:bricabrac.combine-environments.private)

(defvar *combinator* nil
  "Object that maps keys to combinator functions.

If bound to a list, it is assumed to be a property list.")

(defun expand-environmen-bind% (keys env body)
  (loop
    with environment = (gensym "ENV")
    for entry in keys
    for (sym k%) = (if (consp entry) entry (list entry))
    for key = (or k%
                  (if (local-keyword-p sym)
                      sym
                      (intern (string sym) "KEYWORD")))
    collect `(,sym (resolve ,environment ,key)) into bindings
    finally (return `(let ((,environment ,env)) (let ,bindings ,@body)))))

(defmacro environment-bind ((&rest keys) env &body body)
  (expand-environmen-bind% keys env body))

(defgeneric combine-for-key (key old new)
  (:method (_ old new)
    "Default method: NEW shadows OLD"
    new))

(defun without (environment key)
  (let ((entry (member key environment)))
    (if entry
        (append (ldiff environment entry)
                (without (cddr entry) key))
        environment)))

(defun augment (env key value)
  (if value
      (list* key value (without env key))
      (without env key)))

(defun resolve (environment key)
  (getf environment key))

(defvar *environment*)

(defgeneric combine (combinator env key old new)
  (:method ((combinators list) env key old new)
    (let ((function (resolve combinators key)))
      (let ((*environment* env))
        (if function
            (etypecase function
              ;; CALL FUNCTION
              (function (funcall function old new))
              ;; Interpret (INIT I F) forms
              ((cons (member init :init) cons)
               (destructuring-bind (_ I F) function
                 (declare (ignore _))
                 (funcall F (or old I) new))))
            (combine-for-key key old new))))))

(defun combine-two-environments (old new &optional (combinator *combinator*))
  (loop
    :for env := (copy-tree old) :then next
    :for (key new-val) :on new by #'cddr
    :for old-val := (resolve env key)
    :for combined := (combine combinator env key old-val new-val)
    :for next := (augment env key combined)
    :finally (return env)))

(defun ensure-list-of-environments (env)
  (and env (if (consp (first env)) env (list env))))

(defun combine-environments (environments &optional (combinator *combinator*))
  (flet ((fold (o n) (combine-two-environments o n combinator)))
    (reduce #'fold
            (ensure-list-of-environments environments)
            :initial-value nil)))

(defun init (initial-value function)
  (lambda (old new)
    (funcall function (or old initial-value) new)))
