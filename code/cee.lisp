(defpackage :bricabrac.code.cee
  (:use :cl :alexandria))

(in-package :bricabrac.code.cee)

;; newline convention:
;;
;; whether a form should output a newline or not at some points might lead to
;; confusion and inconsistent results.
;;
;; to counter that, there is a printing convention in place for all the
;; following helprs. When we print a THING made of COMPONENTS, it is assumed
;; that the stream is already placed exactly where THING should be printed,
;; which means the enclosing scope made the necessary adjustemnts for it.
;; The printer for THING is responsible for adding newlines and logical groups
;; for underlying COMPONENTS. At the end of each THING, no trailing newline
;; should be added: since newlines are a way to change indentation, this is the
;; job the enclosing scope to dedent and newline.
;;

(defvar *c-environment* :definition)
(defvar *c-indent* 4)
(defvar *c-right-margin* 80)

(defvar *c-signature-formats*
  (list (cons :filled "~:[~;static ~]~a ~a(~@<~{~a~^, ~:_~}~:>)")
        (cons :linear "~:[~;static ~]~a ~a(~@<~{~a~^, ~_~}~:>)")))

(defmacro with-c (&body body)
  `(let ((*print-right-margin* ,*c-right-margin*))
     ,@body))

(defmacro with-logical-block (&body body)
  `(pprint-logical-block (*standard-output* ())
     ,@body))

;; (defun nl (&optional (kind :mandatory))
;;   (pprint-newline kind))

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

(defmacro c/function ((&key
                         name
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
             (write-line ";")
             (return-from ,scope))
           (nl)
           (c/block ,@body))))))

(dolist (*c-environment* '(:declaration :definition))
  (with-c
    (fresh-line)
    (c/function (:name "test"
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
    (c/function (:name "maybe_do_something"
                 :ret "int"
                 :format :linear
                 :parameters '("int *a"
                               "int *b"
                               "char **c"
                               "float *very_long_name"
                               "double *very_long_name_twice_as_long"))
      (princ "return 0;"))))


