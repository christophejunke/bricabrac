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

(defmacro with-temporary-file ((stream name) &body body)
  `(call-within-temporary-directory
    (lambda ()
      (with-open-file (,stream ,name :direction :output :if-exists :error)
        ,@body))))

(defvar *from-path*)
(defvar *socket-file*)
(defvar *script-permissions* #o500)
(defvar *script-name* "script.sh")

(defmacro with-open-task-socket ((socket
                                &key
                                  (socket-name "io.sock")
                                  (file '*socket-file*))
                               &body body)
  `(call-with-io-socket (lambda (,socket ,file) ,@body)
                        :socket-name ,socket-name))

(defun io-address (&key name)
  (let ((file (osicat:native-namestring (merge-pathnames name))))
    (values (iolib:ensure-address file :family :local) file)))

(defun call-with-io-socket (server &key socket-name)
  (multiple-value-bind (address file) (io-address :name socket-name)
    (iolib:with-open-socket (socket
                             :type :stream
                             :connect :passive
                             :address-family :local)
      (iolib:bind-address socket address)
      (iolib:listen-on socket)
      (funcall server socket file))))

(defmacro with-task-client ((server &key send recv (wait nil wp))
                            &body body)
  (with-gensyms (client %message)
    (check-type send symbol)
    (check-type recv symbol)
    `(iolib:with-accept-connection (,client
                                    ,server
                                    ,@(and wp `(:wait ,wait)))
       (flet (,@(and send
                  `((,send (,%message)
                           (write-line ,%message ,client)
                           (finish-output ,client))))
              ,@(and recv
                  `((,recv () (read-line ,client)))))
         (progn ,@body)))))

(defvar *script-mappings* nil)

(defgeneric write-script-footer (script-footer)
  (:method :before (_)
    (unless (listp _)
      (fresh-line)))
  (:method :after (_)
    (unless (listp _)
      (terpri)))
  (:method ((_ null)))
  (:method ((footers list))
    (dolist (footer footers)
      (write-script-footer footer)))
  (:method ((_ (eql :delimiter)))
    (write-string
     "#>------------------------------------------------------------------------------")))

(defgeneric write-script-header (script-header)
  (:method :before (_)
    (unless (listp _)
      (fresh-line)))
  (:method :after (_)
    (unless (listp _)
      (terpri)))
  (:method ((_ null)))
  (:method ((headers list))
    (dolist (header headers)
      (write-script-header header)))
  (:method ((_ (eql :shebang)))
    (write-string "#!/bin/bash"))
  (:method ((_ (eql :cd-workdir)))
    (format t "cd ~s" (getf *script-mappings* :workdir)))
  (:method ((_ (eql :set-debug)))
    (write-line "cat $0")
    (write-string "set -x"))
  (:method ((_ (eql :set-error)))
    (write-string "set -e"))
  (:method ((_ (eql :define-io-function)))
    (format t "export IO_SOCKET=~s
_io () {
  echo \"$1\" | nc -U \"${IO_SOCKET}\"
}" (getf *script-mappings* :socket)))
  (:method ((_ (eql :trap-cleanup-socket)))
    (format t "export IO_SOCKET=~s
_io () {
  echo \"$1\" | nc -U \"${IO_SOCKET}\"
}
_cleanup () {
    res=$?
    trap '' INT
    echo $(_io $res)
}
trap '_cleanup' EXIT" (getf *script-mappings* :socket)))
  (:method ((_ (eql :delimiter)))
    (write-string
     "#<------------------------------------------------------------------------------")))

(defparameter *script-header*
  '(:SHEBANG
    :TRAP-CLEANUP-SOCKET
    :SET-ERROR
    :SET-DEBUG
    :DELIMITER
    :CD-WORKDIR))

(defparameter *script-footer*
  '(:DELIMITER))

(defun call-with-output-to-script (script-name header footer env function)
  (with-open-file (*standard-output*
                   script-name
                   :direction :output
                   :if-exists :error)
    (let ((truename (osicat:native-namestring (pathname *standard-output*))))
      (prog1 truename
        (osicat-posix:chmod truename *script-permissions*)
        (let ((*script-mappings* (append env *script-mappings*)))
          (write-script-header header)
          (funcall function)
          (write-script-footer footer))))))

(defmacro with-output-to-script ((&key
                                    (vars)
                                    (script-name '*script-name*)
                                    (before '*script-header*)
                                    (after '*script-footer*))
                                 &body body)
  `(call-with-output-to-script ,script-name
                               ,before
                               ,after
                               ,vars
                               (lambda () ,@body)))

(defun %current-path ()
  (osicat:native-namestring *default-pathname-defaults*))

(defun call-within-temporary-directory (fn &key prefix)
  (let ((dir (tmpdir (or prefix *tmpdir-name*))))
    (unwind-protect (let ((*default-pathname-defaults* dir))
                      (with-terminal-options ((:wait t) ((:hold :default) nil))
                        (funcall fn)))
      (warn "Deleting ~a: ~:[error~;done~]"
            dir
            (osicat:delete-directory-and-files dir)))))

(defmacro within-temporary-directory ((&key prefix current) &body body)
  (flet ((body () `(call-within-temporary-directory (lambda () ,@body)
                                                    :prefix ,prefix)))
    (if current
        `(let ((,current (%current-path)))
           ,(body))
        (body))))
