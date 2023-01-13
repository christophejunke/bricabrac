(defpackage :bricabrac.utils
  (:use :cl :alexandria :drakma)
  (:import-from #:uiop
                uiop:copy-stream-to-stream)
  (:export #:with-path
           #:nmapcar
           #:define-local-keyword

           #:request-failed
           #:download-to-file
           #:call-with-http-stream
           #:with-http-stream))

(in-package :bricabrac.utils)

(defmacro with-path (path &body body)
  `(let ((*default-pathname-defaults* (merge-pathnames ,path)))
     ,@body))

(defun nmapcar (function seq &rest more-seqs)
  (apply #'map-into seq function seq more-seqs))

(defun interpol (x xs ys)
  (assert (= (length xs) (length ys)))
  (if-let (p (position-if (lambda (y) (> x y)) xs :from-end t))
    (if (= (1- (length xs)) p)
        (aref ys p)
        (let* ((q (1+ p))
               (r (/ (- x (aref xs p))
                     (- (aref xs q) (aref xs p)))))
          (lerp r (aref ys p) (aref ys q))))
    (aref ys 0)))

(defmacro define-local-keyword (symbol &key (documentation "") data)
  (check-type symbol symbol)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (prog1 ',symbol
       (defparameter ,symbol ',symbol ,documentation)
       (setf (get ',symbol 'local-keyword-data) ,data))))

(defun call-with-http-stream (uri function)
  (multiple-value-bind (body status headers reply stream closep message)
      (http-request uri :force-binary t :want-stream t)
    (declare (ignore body headers reply))
    (unwind-protect (funcall function status stream message)
      (when closep (close stream)))))

(defmacro with-http-stream ((status stream message) uri &body body)
  `(call-with-http-stream ,uri (lambda (,status ,stream ,message) ,@body)))

(define-condition request-failed (error)
  ((status :initarg :status :accessor request-failed/status)
   (message :initarg :message :accessor request-failed/message)
   (uri :initarg :uri :accessor request-failed/uri))
  (:report
   (lambda (condition stream)
     (with-slots (status message uri) condition
       (format stream "request failed (~d): ~a - ~a" status message uri)))))

(defun download-to-file (uri target-file &optional (if-exists :error))
  (let ((type '(unsigned-byte 8)))
    (with-open-file (target (ensure-directories-exist target-file)
                            :element-type type
                            :direction :output
                            :if-exists if-exists)
      (when target
        (prog1 (pathname target)
          (loop
            (file-position target 0)
            (with-simple-restart (retry "Retry downloading")
              (return
                (with-http-stream (status source message) uri
                  (if (= status 200)
                      (copy-stream-to-stream source
                                             target
                                             :element-type type)
                      (error 'request-failed
                             :status status
                             :message message
                             :uri uri)))))))))))
