(defpackage :bricabrac.environments.hash-consing
  (:use :cl :alexandria)
  (:export #:with-hash-consing))

(in-package :bricabrac.environments.hash-consing)

(defmacro with-hash-consing ((hash &key (test #'equalp)) &body body)
  (with-gensyms (value hidden-hash)
    `(let* ((,hidden-hash (make-hash-table :test ,test)))
       (unwind-protect 
            (flet ((,hash (,value)
                     (or (gethash ,value ,hidden-hash)
                         (setf (gethash ,value ,hidden-hash) ,value))))
              ,@body)
         (clrhash ,hidden-hash)))))
