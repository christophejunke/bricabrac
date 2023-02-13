(defpackage :bricabrac.with.builtins 
  (:use :cl :bricabrac.with)
  (:import-from :osicat
                osicat:native-namestring
                osicat:*temporary-directory*)
  (:import-from :osicat-posix
                osicat-posix:mkdtemp))

(in-package :bricabrac.with.builtins)

(defparameter +no-bindings+ "This context does not establish bindings")
(defparameter +no-arguments+ "No arguments expected for this context")

(defmethod expand-context
    ((_with (eql :within))
     (path pathname)
     args
     vars
     body)
  (assert (not args) () +no-arguments+)
  (assert (not vars) () +no-bindings+)
  `(let ((*default-pathname-defaults* ,path))
     ,@body))

(defmethod expand-pathname% (path args vars body)
  (assert (not args) () +no-arguments+)
  (assert (not vars) () +no-bindings+)
  `(let ((*default-pathname-defaults* (merge-pathnames ,path)))
     ,@body))

(defmethod expand-context ((_ (eql :with)) (p pathname) a v b)
  (expand-pathname% p a v b))

(defmethod expand-context ((_ (eql :within)) (p pathname) a v b)
  (expand-pathname% p a v b))


(defun make-temporary-directory (&key prefix root)
  (truename
   (mkdtemp
    (native-namestring
     (make-pathname :name prefix :defaults (or root *temporary-directory*))))))

(defmethod call-with-context
    ((_with (eql :within))
     (_type (eql :temporary-directory))
     symbols
     function
     arguments)
  (assert (not symbols) () +no-bindings+)
  (destructuring-bind (&key prefix root) arguments
    (let ((D (make-temporary-directory :prefix prefix :root root)))
      (unwind-protect (let ((*default-pathname-defaults* D)) (funcall function))
        (osicat:delete-directory-and-files D)))))

(defmethod make-wind-unwind-pair ((_w (eql :with)) (_t (eql 'open)) args)
  (values #'open #'close))

(defmethod expand-context ((_w (eql :with)) (_t (eql :output)) args vars body)
  (destructuring-bind (&optional file (if-exists :error)) args
    `(with (open (or ,file *default-pathname-defaults*)
                 :if-exists ,if-exists
                 :direction :output) :as ,vars
       ,@body)))

(defmethod expand-context ((_w (eql :with)) (_t (eql :input)) args vars body)
  (destructuring-bind (&optional file (if-does-not-exists :error)) args
    `(with (open (or ,file *default-pathname-defaults*)
                 :if-does-not-exist ,if-does-not-exists
                 :direction :input) :as ,vars
       ,@body)))

(defmethod vars-delimiter-p ((_w (eql :with))
                             (_t (eql :symbols))
                             symbol)
  (or (string= symbol :named) (call-next-method)))

(defmethod call-with-context ((_w (eql :with))
                              (_t (eql :symbols))
                              symbols
                              function
                              arguments)
  (assert (not arguments) () +no-arguments+)
  (apply function (mapcar #'copy-symbol symbols)))

(defun any-or-selection% (types default)
  (if types
      `(or ,@(loop 
               for e in types
               do (check-type e symbol)
               collect e))
      default))

(defmethod expand-context
    ((_w (eql :without))
     (_t (eql :conditions))
     arguments
     variables
     body)
  (assert (not variables) () +no-bindings+)
  (with :symbols :named (condition)
    `(handler-case (progn ,@body)
       (,(any-or-selection% arguments t)
         (,condition)
         (values nil ,condition)))))

(defmethod expand-context
    ((_w (eql :with))
     (_t (eql :muffled-warnings))
     arguments
     symbols
     body)
  (assert (not symbols) () +no-bindings+)
  `(handler-bind ((,(any-or-selection% arguments 'warning)
                    #'muffle-warning))
     ,@body))

(defmethod call-with-context
    ((_w (eql :within))
     (_t (eql :directory))
     symbols
     function
     arguments)
  (assert (not symbols) () +no-bindings+)
  (when (stringp (first arguments))
    (push :relative arguments))
  (setf (rest arguments)
        (mapcar #'princ-to-string (rest arguments)))
  (let ((*default-pathname-defaults*
          (merge-pathnames
           (make-pathname :directory arguments))))
    (funcall function)))

(declaim (inline enqueue% make-queue% head%))

(defun head% (queue)
  (cdar queue))

(defun make-queue% ()
  (let ((q (cons nil nil))) (cons q q)))

(defun enqueue% (queue values)
  (setf (cdr queue) (last (nconc (cdr queue) (copy-list values)))))

(defmethod expand-context
    ((_w (eql :with))
     (_t (eql :list-collector))
     arguments
     symbols
     body)
  (assert (not arguments) () +no-arguments+)
  (destructuring-bind (collector) symbols
    (with :symbols named (queue values)
      `(let ((,queue (make-queue%)))
         (flet ((,collector (&rest ,values)
                  (enqueue% ,queue ,values)
                  (head% ,queue)))
           ,@body)))))

(with :list-collector :as collect
  (collect 1 2 3)
  (collect :a :b))

(within :temporary-directory
  (within (:directory "test" (random 10))
    (merge-pathnames "something.data")))

(within :temporary-directory 
  (with #P"test.data"
    (with :output :as os
      (write '(hello world) :stream os))
    (with :input :as in 
      (read in))))

(without (:conditions warning error)
  (warn "danger ahead")
  (error "oh no"))

(without :conditions
  (with :muffled-warnings
    (warn "danger ahead")
    (error "oh no")))



