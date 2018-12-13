(defpackage :bricabrac.pipeline
  (:use :cl :external-program))

(in-package :bricabrac.pipeline)

(let* ((a (start "ls" () :input nil :output :stream))
       (ao (process-output-stream a)))
  (run "wc" '() :input ao :output *standard-output*))

(defun list-mark-first-last (list)
  (loop
    for first = t then nil
    for (a . b) on list
    for last = (null b)
    collect (list a first last)))

(assert (equal (maplist #'list-mark-first-last '(3 2 1 0))
               '(((3 T NIL) (2 NIL NIL) (1 NIL NIL) (0 NIL T))
                 ((2 T NIL) (1 NIL NIL) (0 NIL T))
                 ((1 T NIL) (0 NIL T))
                 ((0 T T)))))

(defstruct (:pipe (:constructor make-pipe%)) in out)

(defun open-pipe ()
  (multiple-value-bind (in out) (osicat-posix:pipe)
    (make-pipe :in in :out out)))

(defun close-pipe (pipe)
  (prog1 pipe
    (osicat-posix:close (shiftf (pipe-in pipe) nil))
    (osicat-posix:close (shiftf (pipe-out pipe) nil))))

(defstruct filter action source sink)

(defun make-simple-forms (filters)
  (loop
    for (filter . rest) on filters
    for past-index = nil then index
    for index from 0
    for first = t then nil
    for last = (null rest)
    collect `(spawn ,filter
                    :source ,(or first past-index)
                    :sink ,(or last index))))

(defun make-pipe-symbols (filters)
  (coerce (loop
            for f in filters
            for first = t then nil    
            unless first
              collect (gensym "PIEP"))
          'simple-vector))

;; (defun pipeline-form (filters :input)
;;   (let ((size (length filters)))
;;     (let ((pipes (loop for))))
;;     )
;;   (loop for input = input))

;; (loop
;;   for (filter first last) (list-mark-first-last '(a b c))
;;   collect (make-filter :action :)
;;   collect (make-connector :source :input
;;                           :sink )
;;       )

(defun connect-filters (filters)
  (loop
    for (action . rest) on filters
    for past = :input then action
    for last = (null rest)
    when first
      collect (make-connector )
    collect (make-filter :source past
                         :action action
                         :sink (if last :output action))
    unless last
      collect (make-connector )))

;; (connect-filters '(a b c))



;; (defmacro with-pipeline ((&key input output error) &body filters)
;;   (let ((filters (list-mark-first-last filters)))
    
;;     )
;;   (loop
;;     for (filter firstp lastp) in (list-mark-first-last filters)
;;     collect `(spawn ,filter :input ,(if firstp ,input
;;                                         ))
;;     )
;;   )


