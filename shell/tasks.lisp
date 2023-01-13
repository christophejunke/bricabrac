(defpackage :bricabrac.tasks
  (:use :cl :alexandria))

(in-package :bricabraac.tasks)

;;; term defs

(list
 :key #'identity
 :execute #'print)

;;; actual options

'(:execute (0 3 2) :key 3)

=> (option :execute) => (print '(0 3 2)) => '(0 3 2)

(defun %run-program (&rest args)
  (apply #'sb-ext:run-program args))

(defun opt-get (options key)
  )

(defun %terminal-arguments (command arguments options)
  (flet ((@ (k) (opt-get options k)))
    (let ((directory  )
          (terminal   (@ :terminal)))
      (values command
              (mappend #'@ (@ :parameters))
              (list* :directory (probe-file (@ :directory))
                     :search t
                     :status-hook (@ :status-hook)
                     :wait (@ :wait)
                     :output (@ :output)
                     :input (@ :input)
                     :error (@ :error)
                     (@ :environment))))))

(defun terminal (command &rest args)

  )


(defgeneric make-task (command options))

(defgeneric run-task (task))
