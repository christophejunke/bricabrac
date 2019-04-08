(defpackage :bricabrac.code.cee
  (:use :cl :alexandria)
  (:export #:*c-environment*
           #:*c-indent*
           #:*c-right-margin*
           #:with-c
           #:with-logical-block
           #:with-indent
           #:code-dispatch
           #:c/block
           #:c/block*
           #:c/switch
           #:c/struct
           #:c/case
           #:c/function
           #:nl
           #:declarationp
           #:call-with-multiple-c-contexts
           #:with-multiple-c-contexts))

(in-package :bricabrac.code.cee)

;; newline convention:
;;
;; Newlines and indentations can be confusing and give inconsistent
;; results.
;;
;; To counter that, there is a set of conventions in place for all the
;; following code emitters. When we print a THING made of COMPONENTS,
;; it is assumed that the stream is already placed exactly where THING
;; should be printed, which means the enclosing scope made the
;; necessary adjustemnts for it.  The printer for THING is responsible
;; for adding newlines and logical groups for underlying
;; COMPONENTS. At the end of each THING, no trailing newline should be
;; added: since newlines are a way to change indentation, this is the
;; job of the caller to dedent and newline.
;;

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *c-environment* :definition)
  (defvar *c-indent* 4)
  (defvar *c-right-margin* 80)
  (defparameter *c-signature-formats*
    (list (cons :filled "~:[~;static ~]~a ~a(~@<~{~a~^, ~:_~}~:>)")
          (cons :linear "~:[~;static ~]~a ~a(~@<~{~a~^, ~_~}~:>)")
          (cons :linear/linear
                "~:[~;static ~]~a ~_~a(~@<~{~a~^, ~_~}~:>)"))))

(defmacro with-multiple-c-contexts ((&key &allow-other-keys)
                                    &body body)
  `(call-with-multiple-c-contexts (lambda () ,@body)))

(defun call-with-multiple-c-contexts (function)
  (dolist (*c-environment* '(:declaration :definition))
    (funcall function)))

(defmacro with-c (&body body)
  `(let ((*print-right-margin* *c-right-margin*))
     ,@body))

(defmacro with-logical-block (&body body)
  `(pprint-logical-block (*standard-output* ())
     ,@body))

(defun nl (&optional (kind :mandatory))
  (pprint-newline kind))

(defmacro with-indent ((&optional (indent '*c-indent*)) &body body)
  (with-gensyms (indent%)
    `(let ((,indent% ,indent))
       (pprint-indent :block ,indent%)
       ,@body
       (pprint-indent :block 0))))

(defmacro c/block (&body body)
  `(with-logical-block
     (princ "{")
     (with-indent ()
       (nl)
       ,@body)
     (nl)
     (princ "}")))

(defmacro c/block* (&body body)
  `(progn
     (princ "{")
     (with-indent ()
       (nl)
       ,@body)
     (nl)
     (princ "}")))

(defmacro c/switch (test &body clauses)
  `(with-logical-block
     (format t "switch (~a) {" ,test)
     (nl)
     ,@(loop
         for clause in clauses
         collect clause
         collect '(nl))
     (princ "}")))

(defmacro c/case ((test &key (blockp t) (breakp t)) &body body)
  (let ((body (if breakp `(,@body (nl) (princ "break;")) body)))
    `(progn
       ,(if (eq test :default)
            `(princ "default:")
            `(format t "case ~a:" ,test))
       ,@(if blockp
             `((nl)
               (c/block ,@body))
             `((pprint-indent :block *c-indent*)
               (nl)
               (with-logical-block
                 ,@body)
               (pprint-indent :block 0))))))

(defun declarationp ()
  (eq *c-environment* :declaration))

(defmacro code-dispatch (&body clauses)
  (flet ((only (x)
           (mapcan #'cdr
                   (remove x clauses :test-not #'eq :key #'first))))
    `(cond
       ((declarationp)
        ,@(only :header))
       (t ,@(only :source)))))

(defmacro c/function ((name &key
                            (staticp nil)
                            (format :linear)
                            (ret "void")
                            (parameters '("void")))
                      &body body)
  (with-gensyms (scope)
    (once-only (staticp ret parameters)
      `(block ,scope
         (with-logical-block
           (format t
                   ,(or (cdr (assoc format *c-signature-formats*))
                        (error "Unknown format ~s" format))
                   ,staticp
                   ,ret
                   ,name
                   ,parameters)
           (when (eq *c-environment* :declaration)
             (princ ";")
             (pprint-exit-if-list-exhausted))
           (nl)
           (c/block ,@body))))))

(defmacro c/struct ((name &key
                          (staticp nil)
                          (declaration '(declarationp)))
                    &body body)
  `(with-logical-block
     (format t "~:[~;static ~]struct ~a" ,staticp ,name)
     (when ,declaration
       (princ ";")
       (pprint-exit-if-list-exhausted))
     (princ " ")
     (c/block* ,@body)
     (princ ";")))

(defun test ()
  (dolist (*c-environment* '(:declaration :definition))
    (with-c
      (fresh-line)
      (c/function ("test"
                   :staticp t
                   :format :linear
                   :parameters '("int var"))
        (princ "int ret;")
        (nl)
        (c/switch "var"
          (c/case (0 :blockp t :breakp t))
          (c/case (1 :blockp nil :breakp t))
          (c/case (2 :blockp t :breakp nil))
          (c/case (3 :blockp nil :breakp nil))
          (c/case (:default :blockp nil :breakp t)
            (princ "/* NO */")))))
    (with-c
      (fresh-line)
      (c/function ("maybe_do_something"
                   :ret "int"
                   :format :linear
                   :parameters '("int *a"
                                 "int *b"
                                 "char **c"
                                 "float *very_long_name"
                                 "double *very_long_name_twice_as_long"))
        (princ "return 0;")))))
