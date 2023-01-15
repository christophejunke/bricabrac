(defpackage :with-example.using 
  (:use :cl :bricabrac.with)
  (:export #:using))
(in-package :with-example.using)

;;; Example of how you can organize all your expanders in a package,
;;; and how WITH can be a struct and not just a keyword when
;;; specializing methods.

;; this is where all my expander functions are stored
(defpackage :my-expanders (:use :cl))

;; ctx holds the keyword (using/introducing) of the macro
(defstruct (ctx (:constructor ctx (keyword))) keyword)

(defmethod make-load-form ((context ctx) &optional environment)
  (make-load-form-saving-slots context
                               :slot-names '(ctx-keyword) 
                               :environment environment))

(defun resolve (name)
  (let ((symbol (find-symbol (string name) :my-expanders)))
    (assert symbol)
    (symbol-value symbol)))

;; find symbol with same name call the function associated with it
(defmethod call-with-context ((_ ctx) type syms fn &rest args)
  (check-type type symbol)
  (destructuring-bind (function . names) (resolve type)
    (declare (ignore names))
    (funcall function fn syms args)))

;; in variable namespace, the symbols holds a list of allowed keywords
;; for introducing bindings
(defmethod vars-delimiter-p ((w ctx) type symbol)
  (destructuring-bind (_ . names) (resolve type)
    (declare (ignore _))
    (or (loop for (k . s) in names
                thereis (and (string= (ctx-keyword w) k)
                             (find symbol s :test #'string=)))
        (error "Unknown delimiter"))))

;; user-facing macros for my repository of expanders

(defmacro using (context &body body)
  (expand (ctx 'using) context body))

(defmacro introducing (context &body body)
  (expand (ctx 'introducing) context body))

(defmacro inside (context &body body)
  (expand (ctx 'inside) context body))

;; example expanders

(defparameter my-expanders::tempdir
  '(my-expanders::expand-tempdir (:inside) (:in)))

(defparameter my-expanders::tmp ;; alias
  my-expanders::tempdir)

(defun my-expanders::expand-tempdir (fn syms args)
  (assert (null syms))
  (assert (null args))
  (let ((*default-pathname-defaults* #P"/tmp/"))
    (funcall fn)))

(defparameter my-expanders::symbols
  '(my-expanders::expand-symbols 
    (:using :like)
    (:introducing :named)))

(defparameter my-expanders::fresh-symbols
  '(my-expanders::expand-symbols
    (:introducing :as)))

(defun my-expanders::expand-symbols (fn syms args)
  (assert (null args))
  (apply fn (loop for s in syms collect (copy-symbol s))))

;; example usage

(inside :tempdir
  (probe-file "."))
;; #P"/tmp/"

;; control precisely what combinations of keywords are allowed

(using :symbols like (a b c)
  (list a b c))

(introducing symbols named (a b c)
  (list a b c))

(introducing fresh-symbols as (a b c)
  (list a b c))

