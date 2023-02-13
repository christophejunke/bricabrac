(defpackage :bricabrac.with
  (:use :cl)
  (:export #:with
           #:expand-with
           #:make-wind-unwind-pair
           #:call-with-context))

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
;; specializing the even more EXPAND-WITH function called
;; during macroexpansion.
;;
;; Syntax:
;;
;;     (with <CTX> :as <VARS> <BODY>)
;;     (with <CTX> <BODY>)
;;
;; Where:
;;
;;     <CTX> ::= ATOM | (T &rest args)
;;     <VARS> ::= SYMBOL | (&rest symbols)

;; GENERIC FUNCTIONS
;; ========================================

(defgeneric make-wind-unwind-pair (type arguments)
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

(defgeneric call-with-context (type fn &rest arguments)
  (:documentation "Call FN in a context identified by TYPE and ARGUMENTS")
  (:method (type fn &rest args)
    "Call the WIND/UNWIND pair of functions associated with this
     TYPE and the given arguments ARGS. See MAKE-WIND-UNWIND-PAIR."
    (multiple-value-bind (wind unwind) (make-wind-unwind-pair type args)
      (let ((values (multiple-value-list (apply wind args))))
        (unwind-protect (apply fn values)
          (apply unwind values))))))

(defgeneric expand-with (type args vars body)
  (:documentation "Expansion function for the WITH macro.")
  (:method (type args vars body)
    "Wrap BODY in a function of VARS and delegate to CALL-WITH-CONTEXT."
    `(call-with-context ',type (lambda ,vars ,@body) ,@args)))

;; MACRO
;; ========================================

(defun parse-context (ctx)
  "Return the type of context and a list of arguments"
  (typecase ctx
    ;; complex form with optional arguments
    (cons (values (car ctx) (cdr ctx)))
    ;; a single value
    (t (values ctx nil))))

(defun parse-with-body (body)
  "Return a list of symbols (variables) and the actual body"
  (typecase body
    ;; (with <ctx> :as <vars> <body>)
    ((cons (eql :as) list)
     (pop body) ;; :as
     (destructuring-bind (var &rest body) body
       (typecase var
         ;; vars in a literal list
         (cons (values var body))
         ;; a single var
         (t (values (list var) body)))))
    ;; (with <ctx> <body>)
    (t (values nil body))))

(defmacro with (context &body body)
  (multiple-value-bind (type args) (parse-context context)
    (multiple-value-bind (vars body) (parse-with-body body)
      (expand-with type args vars body))))
