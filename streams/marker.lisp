(defpackage :bricabrac.streams.markers
  (:use :cl))

(in-package :bricabrac.streams.markers)

(defclass marker-stream (trivial-gray-streams:fundamental-character-output-stream)
  ((output-stream
    :reader output-stream
    :initarg :stream
    :initform *standard-output*)
   (markers :accessor stream-markers :initform nil))
  (:default-initargs :output-stream *standard-output*))

