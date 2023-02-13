(in-package #:bricabrac.fold-environments.private)

(defvar *fold-mapping* nil
  "Object that maps keys to folder functions.

If bound to a list, it is assumed to be a property list and subject to
*FOLD-MAPPING-INTERPRETATION-MODE*")

(defvar *fold-mapping-interpretation-mode* nil
  "See INTERPRET-FOLD-FUNCTION and BRICABRAC.FOLD-ENVIRONMENTS.MODES package")

(defvar *environment* nil
  "Current environment, bound during the extent of a folding function.
See RESOLVE")

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

"Interpret CONS forms as expressions.

All occurences of uninterned symbols #:NEW an #:OLD (case-insensitive
comparison) are replaced by their respective QUOTED values at time of
evaluation. Likewise, #:GET is a shorthand for RESOLVE. This is
especially useful when prototyping but this calls EVAL.

    (FOLD-ENVIRONMENTS* '(:A 10 :A 20) 
                        '(:A (CONS #:NEW #:OLD)))
    => (:A (20 10))

    (FOLD-ENVIRONMENTS* '(:A 10 :A 20 :A 10)
                        '(:A (UNION #:OLD (LIST #:NEW))))
    => (:A (20 10))

    (FOLD-ENVIRONMENTS* '((:SCALE 2 :X 1 :Y 1)
                          (:SCALE 3 :X 10 :Y 20)
                          (:SCALE 1/6 :X 10 :Y 30))

                        '((:SCALE (*))
                          (:X (CONS (* #:NEW (#:GET :SCALE)) #:OLD))
                          (:Y (CONS (* #:NEW (#:GET :SCALE)) #:OLD))))
    => (:Y (30 120 2) :X (10 60 2) :SCALE 1)

    (FOLD-ENVIRONMENTS* '((:SCALE 1 :TRANSLATE 0) ;; init
                          (:SCALE 3)              ;; scale (x3)
                          (:TRANSLATE #C(1 -1))   ;; move to #C(3 -3)
                          (:POINT #C(0 0))        ;; compute points ..
                          (:POINT #C(1 0))        ;; relative to ..
                          (:POINT #C(1 1)))       ;; transform

                        ;; TRANSFORMATION RULES
                        '((:SCALE (*) :TRANSLATE (+))
                          (:POINT (CONS (* (+ #:NEW (#:GET :TRANSLATE))
                                           (#:GET :SCALE))
                                        #:OLD))))
    => (:POINT (6 #C(6 -3) #C(3 -3)) :TRANSLATE #C(1 -1) :SCALE 3)
"

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

(defun resolve (key &optional (environment *environment*))
  (getf environment key))

(defun eval-code% (input old new)
  (labels ((q (v) (list 'quote v))
           (s= (str) 
             (lambda (s)
               (and (symbolp s)
                    (null (symbol-package s))
                    (string-equal (symbol-name s) str))))
           (rewrite (v s n) (subst-if v (s= s) n)))
    (eval (rewrite (q new) "new"
                   (rewrite (q old) "old" 
                            (rewrite 'resolve "get" input))))))

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
    (let ((mapping (fold-environments* mapping nil)))
      (let ((function (resolve key mapping)))
        (let ((*environment* env))
          (typecase function
            (null (fold-for-key key old new))
            ((or function symbol)
             (funcall function old new))
            (t (interpret-fold-function *fold-mapping-interpretation-mode*
                                        function
                                        old
                                        new))))))))

(defun fold-environments (old new &optional (mapping *fold-mapping*))
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
    collect `(,sym (resolve ,key ,environment)) into bindings
    finally (return `(let ((,environment ,env)) (let ,bindings ,@body)))))

(defmacro environment-bind ((&rest keys) env &body body)
  (expand-environment-bind% keys env body))
