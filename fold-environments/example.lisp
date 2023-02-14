(defpackage :test
  (:use :cl
        #:bricabrac.fold-environments.environment
        #:bricabrac.fold-environments
        #:bricabrac.local-keywords))

(in-package :test)

(define-local-keyword .type)

(defun flat-nest (op old new)
  (cond
    ((and (consp old) (eq op (car old)))
     (list* op new (cdr old)))
    (old (list op new old))
    (t new)))

(defun fold-type (old new)
  (destructuring-bind (var type) new
    (augment (without old var)
             var
             (flat-nest 'and (resolve var old) type))))

(with-environment (env nil) (.fold-mapping '((.type fold-type)))
  (with-environment env ((.type '(a integer))
                         (.type '(b float)))
    (with-environment env ((.type '(a fixnum)))
      (resolve 'b (resolve .type env)))))
