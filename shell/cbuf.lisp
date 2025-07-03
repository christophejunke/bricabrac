(defpackage :circular-buffer
  (:use)
  (:export #:make-circular-buffer
           #:fill-from-stream
           #:flush-to-stream))

(defpackage :circular-buffer.impl
  (:use :circular-buffer)
  (:use :bordeaux-threads)
  (:use :cl))

(in-package :circular-buffer.impl)

(defstruct (circular-buffer
            (:constructor make-circular-buffer
                (capacity
                 &aux
                 (array (make-array capacity
                                    :element-type '(unsigned-byte 8)
                                    :initial-element 0))
                 (size capacity))))
  (capacity 0 :type fixnum :read-only t)
  array
  (mutex (make-lock "indices"))
  (state :idle)
  (start 0 :type fixnum)
  (size 0 :type fixnum))

(defun circular-cut (start size capacity)
  ;; assume size <= capacity
  ;;
  ;;        <------- size ------->
  ;;
  ;; =======------------------
  ;; 0      ^                 ^- capacity
  ;;        start
  ;;
  (let ((end (+ start size)))
    (if (> end capacity)
        (values capacity (- end capacity))
        (values end 0))))

(defun read% (array stream start amount &aux (nstart start) (total 0))
  (let ((capacity (array-total-size array)))
    (multiple-value-bind (end extra) (circular-cut start amount capacity)
      (setf nstart (read-sequence array stream :start start :end end))
      (incf total (- nstart start))
      (when (= nstart end)
        (setf nstart (read-sequence array stream :start 0 :end extra))
        (incf total nstart))
      (values nstart total))))

(define-condition enter () ())
(define-condition leave () ())
(define-condition wake () ())
(define-condition wait () ())
(define-condition release () ())

(defmacro on-exit (expr &body body)
  `(unwind-protect (progn ,@body) ,expr))

(defmacro with-mutex (m &body body)
  `(progn
     (signal 'wait)
     (with-lock-held (,m)
       (signal 'wake)
       ,@body
       (signal 'release))))

(defun fill-from-stream (buffer stream &aux state% start% size%)
  (check-type buffer circular-buffer)
  (check-type stream stream)
  (with-slots (mutex capacity start size array state) buffer
    (flet ((read% (start size) (read% array stream start size)))
      (signal 'enter)
      (on-exit (signal 'leave)
        (loop
          (with-mutex mutex
            (setf state% state)
            (setf start% start)
            (setf size% size))
          (case state%
            (:read (return))
            ((:written :idle)
             (let ((space (- capacity size%)))
               (multiple-value-bind (nstart read) (read% start% space)
                 (let ((nstate (cond
                                 ((> read 0) :read)
                                 (t :idle))))
                   (with-mutex mutex
                     (setf start nstart)
                     (setf size (- size read))
                     (setf state nstate))))))))))))

(flex:with-input-from-sequence (v #(10 20 30 50 60 90))
  (let ((b (make-circular-buffer 20)))
    (prog1 b
      (fill-from-stream b v))))

(with-open-file (in "/dev/random" :element-type '(unsigned-byte 8))
  (let ((b (make-circular-buffer 20)))
    (prog1 b
      (fill-from-stream b in))))

(with-input-from-string (in "abcdefg")
  ()
  )

(make-circular-buffer 20)
q
