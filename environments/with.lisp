(defpackage :bricabrac.with
  (:use :cl)
  (:export #:with
           #:within
           #:without

           ;; generic functions
           #:expand-context
           #:make-wind-unwind-pair
           #:call-with-context
           #:vars-delimiter-p
           
           ;; utilities
           #:expand
           #:list-all-methods
           #:list-contexts
           #:print-contexts))

(in-package :bricabrac.with)

;; OVERVIEW
;; ========================================
;;
;; Many libraries define their own WITH- macro in the spirit
;; of WITH-OPEN-FILE, WITH-STANDARD-IO-SYNTAX, etc. to
;; temporarily establish a context that is cleaned up when
;; leaving it.
;;
;; The WITH macro offers a unified syntax for that kind of
;; behavior: using CLOS to dispatch on the context, it is
;; possible to define custom context managers with only two
;; functions (see MAKE-WIND-UNWIND-PAIR), by specializing
;; the more general CALL-WITH-CONTEXT function, or by
;; specializing the EXPAND-CONTEXT function called
;; during macroexpansion.
;;
;; Syntax:
;;
;;     (<WITH> <CTX> <AS> <VARS> <BODY>)
;;     (<WITH> <CTX> <BODY>)
;;
;; Where:
;;
;;     <WITH> :: :WITH | :WITHOUT | :WITHIN
;;               (but can be customized)
;;     <CTX> ::= <TYPE> | (<TYPE> &rest args)
;;     <TYPE> ::= SYMBOL
;;     <AS> is a SYMBOL <S> such that 
;;          (VARS-DELIMITER-P <WITH> <TYPE> <S>)
;;     <VARS> ::= SYMBOL | (&rest symbols)
;;
;; NB. WITHIN is an alias for WITH
;;
;; GENERIC FUNCTIONS
;; ========================================

(defgeneric make-wind-unwind-pair (with type arguments)
  (:documentation
   "Return two functions for context TYPE and ARGUMENTS list.

    Assuming the two functions returned are named WIND and
    UNWIND, and that there is an invocation of WITH made as
    follows:

      (with (type . arguments) :as (v1 .. vn)
        ...)

    Then a call to WIND with no arguments must return N values.
    The UNWIND function must accept N values.

    WIND is called first to establish bindings. UNWIND is
    called in an UNWIND-PROTECT form to exit the context
    properly.

    For example:

      (defmethod make-context-pair ((type (eql 'open)) args)
        (values #'open #'close))

    The above is sufficient to execute:

      (with (open \"/tmp/test.data\" :direction :output) :as o
        (write-line \"test\" o))

    NB. The two returned functions might be closures sharing
    data not visible to the user."))

(defgeneric call-with-context (with type syms fn arguments)
  (:documentation "Call FN in a WITH context identified by TYPE and ARGUMENTS")
  (:method (with type syms fun args)
    "Call the WIND/UNWIND pair of functions associated with this
     TYPE and the given arguments ARGS. See MAKE-WIND-UNWIND-PAIR."
    (multiple-value-bind (wind unwind) (make-wind-unwind-pair with type args)
      (let ((values (multiple-value-list (apply wind args))))
        (unwind-protect (apply fun values)
          (apply unwind values))))))

(defgeneric expand-context (with type args vars body)
  (:documentation "Expansion function for the WITH macro.")
  (:method (with type arguments variables body)
    "Wrap BODY in a function of VARS and delegate to CALL-WITH-CONTEXT."
    `(call-with-context ',with
                        ',type
                        ',variables
                        (lambda ,variables ,@body)
                        (list ,@arguments))))

(defgeneric vars-delimiter-p (with type symbol)
  (:method (_with _type (as symbol))
    (member as '(:as :of =) :test #'string=)))

;; MACRO
;; ========================================

(defun parse-context (ctx)
  "Return the type of context and a list of arguments"
  (typecase ctx
    ;; complex form with optional arguments
    (cons (values (car ctx) (cdr ctx)))
    ;; a single value
    (t (values ctx nil))))

(defun parse-with-body (with type body)
  "Return a list of symbols (variables) and the actual body"
  (typecase body
    ;; (with <ctx> :as <vars> <body>)
    ((cons symbol list)
     (cond
       ((vars-delimiter-p with type (first body))
        (pop body) ;; :as, ...
        (destructuring-bind (var &rest body) body
          (typecase var
            ;; no vars actuall
            (null (values nil body))
            ;; vars in a literal list
            (cons (values var body))
            ;; a single var
            (t (values (list var) body)))))
       (t
        (warn "Unknown delimiter ~a for context ~a" (first body) with)
        (values nil body))))
    ;; (with <ctx> <body>)
    (t (values nil body))))

(defun expand (with context body)
  "Parse CONTEXT/BODY and call EXPAND-CONTEXT for the WITH context.

   If BODY starts with :AS, then the following form is a designator
   for a list of variables to be bound by the context (ie. a list or a
   single symbol). The rest is then the actual body.

   CONTEXT is either an atom or a literal list (ATOM &REST ARGS).

   WITH is a way to group contexts into distinct dictionaries: the
   existing macros WITH/WITHIN/WITHOUT use respectively the keywords
   :WITH, :WITHIN and :WITHOUT.  You can register your own contexts in
   distinct namespaces. WITH is typically a symbol but is not required
   to be (see package-context.lisp for an approach that dispatches on
   WITH being a struct type)."
  (multiple-value-bind (type args) (parse-context context)
    (multiple-value-bind (vars body) (parse-with-body with type body)
      (expand-context with type args vars body))))

(defmacro with (context &body body)
  "Execute BODY in CONTEXT for keyword :WITH."
  (expand :with context body))

(defmacro within (context &body body)
  "Execute BODY in CONTEXT for keyword :WITHIN."
  (expand :within context body))

(defmacro without (context &body body)
  "Execute BODY in CONTEXT for keyword :WITHOUT."
  (expand :without context body))

(defun list-all-methods ()
  (append
   (closer-mop:generic-function-methods #'expand-context)
   (closer-mop:generic-function-methods #'call-with-context)
   (closer-mop:generic-function-methods #'make-wind-unwind-pair)))

(defun simplify-specializer (specializer)
  (typecase specializer
    (class `(class , (class-name specializer)))
    (sb-mop:eql-specializer
     (sb-mop:eql-specializer-object specializer))
    (t specializer)))

(defun list-contexts ()
  (delete-duplicates
   (mapcan
    (lambda (m)
      (destructuring-bind (with type . rest) (sb-mop:method-specializers m)
        (declare (ignore rest))
        (let ((entry (list (simplify-specializer with)
                           (simplify-specializer type))))
          (unless (equalp entry '((class t) (class t)))
            (list entry)))))
    (list-all-methods))
   :test #'equalp))

(defun print-contexts ()
  (fresh-line)
  (dolist (ctx (list-contexts))
    (print ctx))
  (values))

(defmethod expand-context ((_ (eql :within)) (path pathname) args vars body)
  (destructuring-bind (&key ensure-exists) args
    (when ensure-exists
      (setf path
            `(ensure-directories-exist 
              ,path
              ,@(and (consp ensure-exists) ensure-exists))))
    (destructuring-bind (&optional (var nil var-p)) vars
      `(let ((*default-pathname-defaults* ,path))
         ,@(if (and var var-p) 
               `((let ((,var *default-pathname-defaults*)) ,@body))
               body)))))

