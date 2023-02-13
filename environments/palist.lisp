(defpackage :pal
  (:use :cl
        :bricabrac.with 
        :bricabrac.environments.hash-consing)
  (:export #:map
           #:fold
           #:do
           #:alist
           #:plist
           #:hash
           #:collecting
           #:pah))

(in-package :pal)

(defvar *hash* (make-hash-table :test #'equalp))

(defun normalize-environment (raw &optional (hash *hash*))
  (with-hash-consing (H :using hash)
    (with :list-collector :as collect
      (flet ((collect (k v) (when v (collect (H (cons k v))))))
        (typecase raw
          (null nil)
          (hash-table 
           (maphash (lambda (k v) (collect k v)) raw))
          (cons
           (loop
             (unless raw (return))
             (let ((head (pop raw)))
               (etypecase head
                 (symbol
                  (assert raw () "Missing value for key ~s" head)
                  (collect head (pop raw)))
                 (cons (collect (car head) (cdr head)))))))))
      (collect))))

(progn
  (fresh-line)
  (write (list
          (normalize-environment '(:a 3 :b 2 (:dir 0 a b c) :stuff nil :stuff 5))
          (normalize-environment '(:a + :b 2 :stuff nil :stuff 10)))
         :circle t))


(defmacro do-plist ((key val list-form &optional result) &body body)
  (with :symbols :named (list)
    `(let ((,list ,list-form))
       (loop
         (unless ,list (return ,result))
         (let ((,key (pop ,list)))
           (assert ,list () "Missing last value (odd sized list)")
           (let ((,val (pop ,list)))
             ,@body))))))

(defmacro do-ap-list ((key val list-form &optional result) &body body)
  (with :symbols :named (list)
    `(let ((,list ,list-form))
       (loop
         (unless ,list (return ,result))
         (let ((,key (pop ,list)))
           (multiple-value-bind (,key ,val)
               (if (consp ,key)
                   (values (car ,key) (cdr ,key))
                   (progn
                     (assert ,list () "Missing last value (odd sized list)")
                     (values ,key (pop ,list))))
             ,@body))))))

(defmacro do-alist ((key val list-form &optional result) &body body)
  (with :symbols :named (list entry)
    `(let ((,list ,list-form))
       (dolist (,entry ,list ,result)
         (destructuring-bind (,key . ,val) ,entry
           ,@body)))))
