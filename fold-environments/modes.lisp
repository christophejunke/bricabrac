(in-package #:bricabrac.fold-environments.private)

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

(declaim (inline init-function% variadic-function%))

(defun init-function% (input old new)
  (destructuring-bind (I F) input
    (funcall F (or old I) new)))

(defun variadic-function% (input old new)
  (destructuring-bind (F) input
    (if old (funcall F old new) (funcall f new))))

(defmethod interpret-fold-function ((_ (eql .simple)) (input cons) old new)
  (if (rest input)
      (init-function% input old new)
      (variadic-function% input old new)))

(define-local-keyword bricabrac.fold-environments.modes:.eval
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
")

(declaim (inline eval-code%))

(defun uninterned= (&rest args)
  (labels ((fn/2 (sym str)
             (and (symbolp sym)
                  (null (symbol-package sym))
                  (string-equal (symbol-name sym) str)))
           (fn/1 (str)
             (check-type str string)
             (lambda (sym) (fn/2 sym str))))
    (if (rest args)
        (apply #'fn/2 args)
        (apply #'fn/1 args))))

(define-local-keyword <NEW>)
(define-local-keyword <OLD>)

(defun transform-code (code old new)
  (flet ((rewrite (form pair)
           (destructuring-bind (replace . search) pair
             (subst-if replace (uninterned= search) form))))
    (reduce #'rewrite
            (list (cons new "new")
                  (cons old "old")
                  (cons 'resolve "get"))
            :initial-value (sublis `((<NEW> . ,new)
                                     (<OLD> . ,old))
                                   code))))

(defun as-function (code)
  (let ((old (gensym "OLD"))
        (new (gensym "NEW")))
    (coerce `(lambda (,old ,new) ,(transform-code code old new))
            'function)))

(defun eval-code% (input old new)
  (flet ((q (v) (list 'quote v)))
    (eval (transform-code input (q old) (q new)))))

(defmethod interpret-fold-function  ((_ (eql .eval)) input old new)
  (eval-code% input old new))

(define-local-keyword bricabrac.fold-environments.modes:.simple/eval
    "Apply mode .SIMPLE if possible, or .EVAL")

(defmethod interpret-fold-function  ((_ (eql .simple/eval)) input old new)
  (typecase input
    ((cons t (cons symbol null))
     (init-function% input old new))
    ((cons symbol null)
     (variadic-function% input old new))
    (t
     (eval-code% input old new))))
