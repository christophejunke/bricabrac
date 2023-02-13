(in-package #:bricabrac.fold-environments.private)

(defvar *environment*)

(defvar *fold-mapping-interpretation-mode* nil
  "See INTERPRET-FOLD-FUNCTION and BRICABRAC.FOLD-ENVIRONMENTS.MODES package")

(defvar *fold-mapping* nil
  "Object that maps keys to folder functions.

If bound to a list, it is assumed to be a property list and subject to
*FOLD-MAPPING-INTERPRETATION-MODE*")

(define-local-keyword bricabrac.fold-environments.modes:.simple
    "Interpret (F) and (I F) quoted forms.

  - (F) stands for variadic F, if OLD is NIL, only call F with NEW
  - (I F) is the variant where F is called with (OR OLD I) and NEW

For example:

    (FOLD-ENVIRONMENTS* '(:A 10 :A 20) '(:A +))
    !! #<TYPE-ERROR expected-type: NUMBER datum: NIL>

    (FOLD-ENVIRONMENTS* '(:A 10 :A 20) '(:A (+)))
    => (:A 30)

    (FOLD-ENVIRONMENTS* '(:A 10 :A 20) '(:A (100 +)))
    => (:A 130)
")

(define-local-keyword bricabrac.fold-environments.modes:.eval
    "Interpret CONS forms as expressions.

All occurences of uninterned symbols #:NEW an #:OLD (case-insensitive
comparison) are replaced by their respective QUOTED values at time of
evaluation.

    (FOLD-ENVIRONMENTS* '(:A 10 :A 20) 
                        '(:A (CONS #:NEW #:OLD)))
    => (:A (20 10))

    (FOLD-ENVIRONMENTS* '(:A 10 :A 20 :A 10)
                        '(:A (UNION #:OLD (LIST #:NEW))))
    => (:A (20 10))
")

(define-local-keyword bricabrac.fold-environments.modes:.simple/eval
    "Apply mode .SIMPLE if possible, or .EVAL")

(defgeneric fold-for-key (key old new)
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

(defun eval-code% (input old new)
    (labels ((s= (str) 
               (lambda (s)
                 (and (symbolp s)
                      (null (symbol-package s))
                      (string-equal (symbol-name s) str))))
             (rewrite (v s n) (subst-if (list 'quote v) (s= s) n)))
      (eval (rewrite new "new" (rewrite old "old" input)))))

(defgeneric interpret-fold-function (mode function old new)
  (:method ((_ null) input old new)
    (error "Invalid input ~s" input))
  (:method ((_ (eql .simple)) (input cons) old new)
    (if (rest input)
        #1=(destructuring-bind (I F) input
             (funcall F (or old I) new))
        #2=(destructuring-bind (F) input
             (if old (funcall F old new) (funcall f new)))))
  (:method ((_ (eql .eval)) input old new)
    (eval-code% input old new))
  (:method ((_ (eql .simple/eval)) input old new)
    (typecase input
      ((cons t (cons symbol null)) #1#)
      ((cons symbol null) #2#)
      (t (eval-code% input old new)))))

(defgeneric generic-fold (mapping env key old new)
  (:method ((mapping list) env key old new)
    (let ((function (resolve mapping key)))
      (let ((*environment* env))
        (typecase function
          (null (fold-for-key key old new))
          ((or function symbol)
           (funcall function old new))
          (t (interpret-fold-function *fold-mapping-interpretation-mode*
                                      function
                                      old
                                      new)))))))

(defun fold-environments (old new &optional (mapping *fold-mapping*))
  (loop
    :for env := (copy-tree old) :then next
    :for (key new-val) :on new by #'cddr
    :for old-val := (resolve env key)
    :for fold := (generic-fold mapping env key old-val new-val)
    :for next := (augment env key fold)
    :finally (return env)))

(defun ensure-list-of-environments (env)
  (and env (if (consp (first env)) env (list env))))

(defun fold-environments* (environments &optional (mapping *fold-mapping*))
  (flet ((fold (o n) (fold-environments o n mapping)))
    (reduce #'fold
            (ensure-list-of-environments environments)
            :initial-value nil)))

(defun expand-environment-bind% (keys env body)
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
  (expand-environment-bind% keys env body))
