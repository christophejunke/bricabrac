(defpackage :events.impl (:use :cl :uv))
(in-package :events.impl)

(defstruct evb ptr clr)

(ql:quickload :trivial-garbage)

(defun make-event-base ()
  (let ((ptr (cffi:foreign-alloc '(:struct uv:uv-loop-s))))
    (warn "init: ~d" (uv-loop-init ptr))
    (flet ((fin ()
             (warn "finalizer")
             (unless (cffi:null-pointer-p ptr)
               (warn "close ~a: ~d" ptr (uv-loop-close ptr))
               (cffi:foreign-free ptr)))
           (clr () (setf ptr (cffi:null-pointer))))
      (let ((obj (make-evb :ptr ptr :clr #'clr)))
        (prog1 obj
          (tg:finalize obj #'fin))))))

(defmacro check-success! (expr)
  (let ((status (gensym)))
    `(let ((,status ,expr))
       (assert (= ,expr 0) () "Non-zero return status ~a: ~a" ',expr ,status))))

(defmacro check-success? (expr)
  (let ((status (gensym)))
    `(let ((,status ,expr))
       (unless (= ,status 0)
         (warn "Non-zero return status ~a: ~a" ',expr ,status)))))

(defun call-with-uv-loop (fn)
  (cffi:with-foreign-object (uv-loop '(:struct uv:uv-loop-s))
    (check-success! (uv-loop-init uv-loop))
    (unwind-protect (funcall fn uv-loop)
      (check-success? (uv-loop-close uv-loop)))))
