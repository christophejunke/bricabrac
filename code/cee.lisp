(defpackage :bricabrac.code.cee
  (:use :cl :alexandria))

(in-package :bricabrac.code.cee)

(defvar *c-environment* :definition)
(defvar *c-indent* 4)
(defvar *c-right-margin* 80)

(defmacro with-c (&body body)
  `(let ((*print-right-margin* ,*c-right-margin*))
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
       (nl)
       ,@body
       (pprint-indent :block 0)
       (nl))))

(defmacro c/block (&body body)
  `(with-logical-block
     (princ "{")
     (with-indent () ,@body)
     (princ "}")))

(defmacro c/switch (test &body body)
  `(with-logical-block
     (format t "switch (~a) {" ,test)
     ,@body
     (nl)
     (princ "}")))

(defmacro c/case ((test &key (blockp t) (breakp t)) &body body)
  (let ((body (if breakp `(,@body (princ "break;")) body)))
    `(progn
       ,(if (eq test :default)
            `(princ "default:")
            `(format t "~@:_case ~a:" ,test))
       ,@(if blockp
             `((nl)
               (c/block ,@body))
             `((pprint-indent :block *c-indent*)
               (nl)
               (with-logical-block
                 ,@body)
               (pprint-indent :block 0))))))

(defmacro c/function ((&key
                         name
                         (staticp nil)
                         (ret "void")
                         (parameters '("void")))
                      &body body)
  (with-gensyms (scope)
    `(block ,scope
       (with-logical-block
         (format t
                 "~:[~;static ~]~a ~a(~@<~{~a~^, ~_~}~:>)"
                 ,staticp
                 ,ret
                 ,name
                 ,parameters)
         (when (eq *c-environment* :declaration)
           (write-line ";")
           (return-from ,scope))
         (nl)
         (with-c-block ,@body)))))

(with-c
  (fresh-line)
  (c/function (:name "test"
               :staticp t
               :parameters '("const int v"))
    (princ "int ret;")
    (nl)
    (c/switch "var"
      (c/case (0 :blockp t :breakp t))
      (c/case (1 :blockp nil :breakp t))
      (c/case (2 :blockp t :breakp nil))
      (c/case (3 :blockp nil :breakp nil)))))

(with-c
  (fresh-line)
  (c/function (:name "maybe_do_something"
               :ret "int"
               :parameters '("int *a"
                             "int *b"
                             "char **c"
                             "float *very_long_name"
                             "double *very_long_name_twice_as_long"))
    (princ "return 0;")))

