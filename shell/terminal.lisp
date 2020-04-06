(in-package :bricabrac.shell)

;;;; OPTIONS

;; plist of terminal-agnostic options
(defparameter *options*
  '(;; which term to use
    :term :xterm
    ;; enable colors?
    :colorsp t
    ;; hold term open when the command quits?
    :hold nil
    ;; working directory
    :directory t
    ;; should we wait until the terminal closes?
    :wait nil))

;; The translation from the values associated to the above options to
;; the actual arguments that are given to executables that spawns
;; external processes is done through *TERMINAL-OPTIONS*, defined
;; below.
;;
;; *TERMINAL-OPTIONS* is an alist from a terminal name (a list of
;; symbols) to a plist, where each above option is associated with
;; either a constant or a function.

(defun arg (&rest prefix)
  (lambda (v) (and v (append prefix (list v)))))

(defun directory-option (v)
  (etypecase v
    (pathname v)
    (string (pathname v))
    ((eql t) *default-pathname-defaults*)
    ((eql :tmp) (truename #P"/tmp/"))))

(defun terminal-options (tree &aux options)
  (do-property-leaves ((path &rest args) tree :result (reverse options))
    (push (cons (reverse path) args) options)))

(progn
  (defparameter *terminal-options-tree*
    `(_root (:term ,#'string-downcase
             :directory ,#'directory-option
             :colorsp ,(constantly nil))
      (:shell (:term "sh"
               :execute ,(lambda (args)
                          (and args
                               (list "-c" (sh-escape (list* "exec" args)))))))
      (_X11 (:geometry ,(arg "-geometry") :class ,(arg "-class")
             :execute ,(lambda (args)
                        (and args (list* "-e" args))))
       (:xterm ,`(:hold ("-hold")
                  :colorsp ,(lambda (x) (unless x '("-cm")))
                  :term "xterm")
        nil
        ;; example of child terminal, named '(:xterm :mpv), which forces
        ;; specific translations for options.
        (:test ,`(:colorsp ,(constantly '("+cm")) :hold ,(constantly nil)))))))

  ;; translate terminal-agnostic options as actual options
  (defparameter *terminal-options*
    (terminal-options *terminal-options-tree*)))

(defun resolve-terminal (&optional (name (getf *options* :term)))
  (rest (assoc (ensure-list name) *terminal-options* :test #'equalp)))

(defmacro with-terminal-options ((&rest options) &body body)
  (flet ((parse-option (entry)
           (destructuring-bind (x y &key ((:when z) t zp)) entry
             (let ((code `((push ,y *options*) (push ,x *options*))))
               (if zp
                   `((when ,z ,@code))
                   code)))))
    `(let ((*options* *options*))
      ,@(loop for o in options append (parse-option o))
      ,@body)))

(defun option (name &optional (terminal-options (resolve-terminal)))
  (let ((terminal-option (getf terminal-options name)))
    (let ((option (getf *options* name)))
      (typecase terminal-option
        (function (funcall terminal-option option))
        (null option)
        (t (and option terminal-option))))))

(defun terminal% (program
                  &key
                    args
                    (hold (option :hold))
                    (colorp (option :colorsp))
                    (geometry (option :geometry)))
  (with-terminal-options ((:execute (and program (list* program args))))
    (let ((command-line
           (list* (option :term)
                  (append hold
                          (option :class)
                          colorp
                          geometry
                          ;; :execute must be last for xterm
                          (option :execute))))
          (directory (option :directory)))
      (format *debug-io*
              "~&~@[cd ~a && ~]~a~%"
              directory
              (sh-escape command-line))
      (funcall (if (option :wait)
                   #'uiop:run-program
                   #'uiop:launch-program)
               command-line
               :directory directory
               ;; :search t
               ;; :wait *wait*
               ;; :environment (make-environment)
               ;; :status-hook *status-hook*
               :output nil
               :input nil
               :error nil))))

(defun terminal (program &rest args)
  (terminal% program :args (map 'list #'princ-to-string args)))
