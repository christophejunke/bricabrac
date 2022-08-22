(defpackage :bricabrac.filters
  (:use :cl))

(in-package :bricabrac.filters)

(defclass port ()
  ((node :accessor port-node :initarg :node)
   (conn :accessor port-connection :initform nil)))

(defclass input-port (port) ())
(defclass output-port (port) ())

(defgeneric make-from-blueprint (blueprint))

(defgeneric output-ports (filter))
(defgeneric input-ports (filter))

(defgeneric make-connection (from-port to-port &key &allow-other-keys)
  (:documentation
   "Create a connection blueprint to transmit data flowing from the FROM-PORT
   to the TO-PORT (with adapters if necessary), or signal an error if this is
   impossible (e.g. connect input to input, etc.). The optional keys can be
   used to select ..."))

(defgeneric filter-open-p (filter))

(defclass open-field-mixin ()
  ((%open :accessor filter-open-p :initform nil)))

(defgeneric open-filter (closed-filter &key &allow-other-keys)
  (:method :before (filter)
    (assert (not (filter-open-p filter)))))

(defgeneric close-filter (open-filter))

(let ((graph (make-instance 'filter-graph)))
  (let ((program
          (make-instance 'program-filter
                         :graph graph
                         :program ""
                         :arguments '()
                         :environment ()
                         :input (make-instance 'program-input-port)
                         :output (make-instance 'program-output-port)
                         :error (make-instance 'program-error-port)))
        (filter
          (make-instance 'thread-filter
                         :graph graph
                         :function #'filter-and-count
                         :input (make-instance 'thread-input-port)
                         :output (make-instance 'thread-output-port))))

    (let ((c (make-connection (output-port! program)
                              (input-port! filter))))

      )
    ))
