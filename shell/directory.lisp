(in-package #:bricabrac.shell)

(declaim
 (inline call-within-temporary-directory
         call-within-temporary-directory/thread-if-wait))

(defun randletter (&rest args)
  (declare (ignore args))
  (random-elt
   #.(coerce (loop
                for c from (char-code #\a) upto (char-code #\z)
                for char = (code-char c)
                collect char
                collect (char-upcase char))
             'string)))

(defvar *tmpdir-name* nil)

(defun tmpdir (&optional (name *tmpdir-name*))
  (truename
   (osicat-posix:mkdtemp
    (osicat:native-namestring
     (make-pathname :name name
                    :defaults osicat:*temporary-directory*)))))

(defun call-within-temporary-directory (function)
  (let ((directory (tmpdir)))
    (unwind-protect (let ((*default-pathname-defaults* directory))
                      (funcall function))
      (osicat:delete-directory-and-files directory))))

(defun call-within-temporary-directory/thread-if-wait (function &rest args)
  (if (option :wait)
      (apply #'call-within-temporary-directory function args)
      (bt:make-thread (lambda ()
                        (apply #'call-within-temporary-directory
                               (lambda ()
                                 (with-terminal-options ((:wait t))
                                   (funcall function)))
                               args)))))

(defmacro within-temporary-directory ((&key (if-wait :thread)
                                            (prefix nil pp))
                                       &body body)
  (let ((fn `(lambda () ,@body)))
    (flet ((wrap (body)
             (if pp
                 `((let ((*tmpdir-name* ,prefix))
                     ,@body))
                 body)))
      `(progn
        ,@(case if-wait
          (:thread (wrap
                    `((call-within-temporary-directory/thread-if-wait ,fn))))
          ((nil) (wrap
                  `((call-within-temporary-directory ,fn))))
          (t (with-gensyms ($fn)
               `((flet ((,$fn () ,@body))
                    (case ,if-wait
                      (:thread
                       ,@(wrap
                          `((call-within-temporary-directory/thread-if-wait
                             #',$fn))))
                      ((nil)
                       ,@(wrap
                          `((call-within-temporary-directory
                             #',$fn))))))))))))))
