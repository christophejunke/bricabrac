(defpackage :bricabrac.term
  (:use :cl :alexandria :bricabrac.utils :bricabrac.mixins :bricabrac.environments))

(in-package :bricabrac.term)

(define-local-keyword .environment)
(define-local-keyword .options)
(define-local-keyword .name)

(defclass terminal-configuration (has-parent)
  ((.environment :initarg .environment :reader .environment)
   (.options :initarg .options :reader .options)))

(defclass obsolete-configuration (has-name) ())

(defun make-terminal-configuration (&key parent environment options)
  (make-instance 'terminal-configuration
                 :parent parent
                 .environment environment
                 .options options))

(defmacro define-terminal (name (&optional parent) &key environment options)
  (check-type name symbol)
  (with-gensyms (term-conf)
    `(let ((,term-conf (make-terminal-configuration :parent ,parent
                                                    :environment environment
                                                    )))
       (register-global-terminal ',name ,term-conf))))

(defun sh-escape (args)
  (flet ((esc (a) (shellwords:escape (princ-to-string a))))
    (format nil "~{~a~^ ~}" (mapcar #'esc args))))

(defvar *seen* nil)

(defun find-terminal-configuration (name &key (errorp t))
  (or (get name 'terminal-configuration)
      (and errorp
           (error "terminal configuration not found for ~s" name))))

(defun register-global-terminal (name configuration)
  (prog1 configuration
    (let ((current (find-terminal-configuration name :errorp nil)))
      (setf (get name 'terminal-configuration) configuration)
      (when current
        (change-class current 'obsolete-configuration :name name)))))

(defun unregister-global-terminal (name)
  (let ((current (find-terminal-configuration name :errorp nil)))
    (setf (get name 'terminal-configuration) nil)
    (slot-makunbound (change-class current 'obsolete-configuration)
                     'name)
    (values)))

(defmacro with-circularity-check (o &body body)
  (check-type o symbol)
  `(progn
     (assert (not (member ,o *seen*)))
     (let ((*seen* (list* ,o *seen*)))
       ,@body)))

(defmethod parent :around ((c terminal-configuration))
  (when-let (parent (call-next-method))
    (ensure-configuration parent)))

(defmethod parent ((c obsolete-configuration))
  (with-circularity-check c
    (parent (ensure-configuration (name c)))))

(defgeneric ensure-configuration (cfg)
  (:method ((c terminal-configuration)) c)
  (:method ((_ null)) nil)
  (:method ((s symbol)) 
    (with-circularity-check s
      (ensure-configuration
       (find-terminal-configuration s)))))

(defun prepare (cfg)
  (let ((cfg (ensure-configuration cfg)))
    (with-circularity-check cfg
      (if-let (p (parent cfg))
        (multiple-value-bind (env opt) (prepare p)
          (values (combine-environments env (.environment cfg))
                  (combine-environments opt (.options cfg))))
        (values (.environment cfg)
                (.options cfg))))))

(defmacro define-terminal (name (&optional parent) (&rest env) &body opt)
  `(register-global-terminal ',name
                             (make-terminal-configuration 
                              :parent ',parent
                              :environment (list ,@env)
                              :options (list ,@opt))))

(define-terminal root ()
    (:term 'opt/term
     :directory 'opt/directory
     :colorsp 'opt/colorsp)
  :colorsp nil
  :directory #'user-homedir-pathname)

(define-terminal shell (root)
    (:term "sh"
     :execute (lambda (args)
                (when args
                  `("-c" ,(sh-escape `("exec" ,@args)))))))

(defun opt (option)
  (lambda (value)
    (when value
      (list option))))

(define-terminal x11 (root)
    (:geometry (opt "-geometry")
     :class (opt "-class")
     :execute (lambda (args)
                (when args `("-e" ,@args)))))

(define-terminal :gnome (x11)
    (:term "gnome-terminal"))

(define-terminal :xterm (x11)
    (:term "xterm"
     :hold (opt "-hold")
     :colorsp (opt "-cm")))

(defvar *options* '(:term :xterm))

(defun option (key)
  (let* ((not-found '#.(gensym "NOT FOUND"))
         (result (getf *options* key not-found)))
    (if (eq result not-found)
        (values nil nil)
        (values result t))))

(define-local-keyword .cfg)
(define-local-keyword .env)
(define-local-keyword .opt)

(defclass terminal ()
  ((cfg :initarg .cfg)
   (env :initarg .env)
   (opt :initarg .opt)))

(defun make-terminal (&optional (cfg (option :term)))
  (multiple-value-bind (env opt) (prepare cfg)
    (make-instance 'terminal .env env .opt opt .cfg cfg)))

(defmethod print-object ((term terminal) stream)
  (print-unreadable-object (term stream :type t :identity t)
    (princ (ignore-errors (slot-value term 'cfg)) stream)))

(make-terminal)




