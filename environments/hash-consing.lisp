(defpackage :bricabrac.environments.hash-consing
  (:use :cl :alexandria)
  (:export #:with-hash-consing))

(in-package :bricabrac.environments.hash-consing)

(defmacro with-hash-consing ((hash &key 
                                     (using)
                                     (test '#'equalp)) &body body)
  (with-gensyms (value hidden-hash)
    (let* ((hash-symbol (or using hidden-hash))
           (form `(flet ((,hash (,value)
                           (or (gethash ,value ,hash-symbol)
                               (setf (gethash ,value ,hash-symbol) ,value))))
                    ,@body)))
      (if using
          form
          `(let ((,hash-symbol (make-hash-table :test ,test)))
             (unwind-protect ,form
               (clrhash ,hash-symbol)))))))
