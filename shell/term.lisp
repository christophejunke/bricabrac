(defpackage :scratch
  (:use :cl :foldenv)
  (:local-nicknames (#:posix #:osicat-posix)
                    (#:io #:iolib)
                    (#:os #:osicat)))

(in-package :scratch)

(ql:quickload :iolib/os)

(ql:quickload :iolib)

(defparameter *tmpdir*
  `(:prefix ,(concatenate 'string (lisp-implementation-type) "-")
    :path ,os:*temporary-directory*))

(defun make-temporary-directory ()
  (environment-bind (prefix path) *tmpdir*
    (truename (posix:mkdtemp
               (os:native-namestring
                (make-pathname :name prefix :defaults path))))))

(defun call-within-temporary-directory (function)
  (flet ((call-within (*default-pathname-defaults* f) (funcall f)))
    (let ((tmpdir (make-temporary-directory)))
      (unwind-protect (call-within tmpdir function)
        (os:delete-directory-and-files tmpdir :if-does-not-exist :ignore)))))

(defmacro within-temporary-directory ((&key current) &body body)
  (let ((call `(call-within-temporary-directory (lambda () ,@body))))
    (etypecase current
      (null call)
      (symbol `(let ((,current *default-pathname-defaults*)) ,call)))))

(with-environment *tmpdir* (:prefix "test-")
  (within-temporary-directory (:current foo)
    (list foo (probe-file ""))))

(defparameter *task-options*
  `(:socket-name "io.sock"
    :permission #o500
    :script "run.sh"
    :timeout 5))

(defparameter *task*
  '(:socket-file nil
    :workdir nil
    :task-id nil))

(defun call-with-socket (server-function)
  (environment-bind (socket-name) *task-options*
    (multiple-value-bind (address file) (local-file-address socket-name)
      (io:with-open-socket (socket :connect :passive :address-family :local)
        (io:bind-address socket address)
        (io:listen-on socket)
        (with-environment *task* (:socket-file file)
          (funcall server-function socket file))))))

(defmacro with-socket ((socket file) &body body)
  `(call-with-socket (lambda (,socket ,file) ,@body)))

(defun await-pid (server-socket)
  (environment-bind (timeout) *task-options*
    (io:with-accept-connection (client server-socket :wait timeout)
      (parse-integer (read-line client)))))

(defun await-client (server)
  (io:accept-connection server :wait t))

(defmacro with-open-script ((stream) &body body)
  (alexandria:with-gensyms (perm sh path)
    `(environment-bind ((,perm :permissions) (,sh :script)) *task-options*
       (with-open-file (,stream ,sh :direction :output :if-exists :error)
         (let ((,path (os:native-namestring (pathname ,stream))))
           (posix:chmod ,path ,perm))
         ,@body))))

(defgeneric as-script (program socket-file stream))

(defun render-script (program socket-file)
  (with-open-script (script)
    (as-script program script socket-file)
    (pathname script)))

(defun synchronized-execution (program)
  (within-temporary-directory (:current workdir)
    (with-environment *task* (:workdir workdir)
      (with-socket (server socket-file)
        (let ((script (render-script program socket-file)))
          (spawn script)
          (let ((pid (await-line server)))
            (unwind-protect (await-client server)
              (terminate pid))))))))


(defun local-file-address (name)
  (let ((file (os:native-namestring (merge-pathnames name))))
    (values (io:ensure-address file :family :local) file)))

(defun test (&aux (file "/tmp/io.sock"))
  (io:with-event-base (events)
    (multiple-value-bind (address file) (local-file-address file)
      (write-line file)
      (io:with-open-socket (socket :connect :passive :address-family :local)
        (io:bind-address socket address :reuse-address t)
        (io:listen-on socket)
        (flet ((test-callback (fd event _)
                 (declare (ignore event _))
                 (io:with-accept-connection (client socket)
                   (write-line "ok" client))
                 (delete-file file)
                 (io:exit-event-loop events)))
          (io:set-io-handler events
                             (io:fd-of socket)
                             :read
                             #'test-callback
                             :one-shot t))
        (io:event-dispatch events)))))

(defstruct (circular-buffer
            (:constructor make-circular-buffer
                (capacity
                 &aux
                 (array (make-array capacity
                                    :element-type '(unsigned-byte 8)
                                    :initial-element 0))
                 (start (bt2:make-atomic-integer))
                 (size (bt2:make-atomic-integer)))))
  (capacity 0 :type fixnum :read-only t)
  array
  start
  size)

(defun cb-fill (cb istream &aux w)
  (with-slots (w-mutex read write size) cb
    (let ((limit (rem (1- (bt2:atomic-integer-value read)) size)))

      )
    (bt:with-mutex (w-mutex)
      (setf w write))


    )
  )

(defun cb-flush (cb ostream)
  )

(make-circular-buffer 20)

q
;; terminal

;; program

;; pipeline

