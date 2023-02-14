(in-package #:bricabrac.fold-environments.private)


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

(defun resolve (key &optional (environment *environment*))
  (getf environment key))

(defvar *environment* nil
  "Current environment, bound during the extent of a folding function.
See RESOLVE")

(defun expand-environment-bind% (keys env body)
  (loop
    with environment = (gensym "ENV")
    for entry in keys
    for (sym k%) = (if (consp entry) entry (list entry))
    for key = (or k%
                  (if (local-keyword-p sym)
                      sym
                      (intern (string sym) "KEYWORD")))
    collect `(,sym (resolve ,key ,environment)) into bindings
    finally (return `(let ((,environment ,env)) (let ,bindings ,@body)))))

(defmacro environment-bind ((&rest keys) env &body body)
  (expand-environment-bind% keys env body))

(defmacro with-environment ((&rest env) &body body)
  (flet ((expand (env)
           `(let ((*environment*
                    (fold-environments% *environment* (list ,@env))))
              ,@body)))
    (cond
      ((not env) `(progn ,@body))
      ((consp (first env))
       (loop for (k v) in env append (list k v) into list
             finally (return (expand list))))
      (t (expand env)))))
