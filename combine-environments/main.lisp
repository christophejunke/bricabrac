(in-package #:bricabrac.combine-environments.private)

(defvar *combinator* nil
  "Object that maps keys to combinator functions.

If bound to a list, it is assumed to be a property list.")

(defmacro environment-bind ((&rest keys) env &body body)
  (let ((e (gensym)))
    `(let ((,e ,env))
       (let ,(mapcar (lambda (k) `(resolve ,e ,k)) keys)
         ,@body))))

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

(defgeneric combine (combinator env key old new)
  (:method ((combinators list) env key old new)
    (let ((function (resolve combinators key)))
      (if function
          (funcall function old new)
          (combine-for-key key old new)))))

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
