(in-package :bricabrac.shell)

;;;; OPTIONS

;; alist of terminal-agnostic options
(defparameter *options* '((:term . :xterm)))

(define-condition not-found (serious-condition) ())

(defun option% (key &key (errorp t) &aux (options *options*))
  (if-let ((entry (assoc key options)))
    (cdr entry)
    (and errorp
         (error "key ~s not found in ~s" key options))))

(defun optionp (key)
  (assoc key *options*))

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
    ((or null (eql :tmp)) (truename #P"/tmp/"))))

(defparameter *terminal-reducers* nil)

(define-local-keyword .reducers
  :documentation "keyword for option reducers")

(define-local-keyword .options
  :documentation "keyword for options passed to the shell/terminal emulator")

(define-local-keyword .defaults
  :documentation "keyword for defaults values")

(define-local-keyword .environment
  :documentation "keyword for environment variables")

(define-local-keyword .screen
  :documentation "keyword for screen arguments")

(defmethod attribute-reducer ((attribute (eql .options)) old new)
  (append new old))

(defmethod attribute-reducer ((attribute (eql .environment)) old new)
  (append new old))

(defmethod attribute-reducer ((attribute (eql .screen)) old new)
  (append new old))

(defmethod attribute-reducer ((attribute (eql .reducers)) old new)
  (append new old))

(defmethod attribute-reducer ((attribute (eql .defaults)) old new)
  (append new old))

;; (defun env-option (v)
;;   (and v (list :environment v)))

(defun normalize-env (env)
  (let ((hash (make-hash-table :test #'equal)))
    (prog1 hash
      (flet ((put (key value)
               (unless (gethash key hash)
                 (setf (gethash key hash) value))))
        (labels ((process (entry)
                   (destructuring-bind (k . v) entry
                     (case k
                       (when (destructuring-bind (condition . couples) v
                               (when (eval condition)
                                 (dolist (c couples)
                                   (process c)))))
                       (:inherit (dolist (u v)
                                   (put u :inherit)))
                       (t (put k v))))))
          (map () #'process env))))))

(defun environment-option (e)
  ;; /!\ option for sb-ext:run-program
  (let ((env (normalize-env e)))
    (labels ((value-of (k)
               (let ((v (gethash k env)))
                 (case v
                   (pending (error "circular search"))
                   (:inherit (osicat-posix:getenv k))
                   (t (optima:match v
                        ((list '= other)
                         (setf (gethash k env) 'pending)
                         (setf (gethash k env)
                               (or (value-of other)
                                   (osicat-posix:getenv other))))
                        (_ v)))))))
      (labels ((fmt* (list)
                 (delete nil (mapcar #'fmt list)))
               (fmt (key)
                 (when-let (value (value-of key))
                   (format nil "~a=~a" key value))))
        (when env
          (list :environment (fmt* (hash-table-keys env))))))))

(defun run-program/env-option (e)
  "combine terminal .environment options with user-supplied :env options"
  (environment-option (append e (option .environment))))

(defun terminal-options (tree &aux options)
  (do-property-leaves ((path &rest args)
                       tree
                       :result (reverse options))
    (push (cons (reverse path) args) options)))

(defparameter *known-classes*
  '((:big . "XTermBIG")
    (:log . :adb/small)
    (:alt . "XTermALT")
    (:adb . "XTermADB")
    (:adb/small . "XTermSmallADB")))

(defun class-option (v)
  (typecase v
    (string (list "-class" v))
    (keyword (class-option
              (cdr (assoc v *known-classes*))))))

(defun hold-option (v)
  (and v '("-hold")))

(defparameter *palette*
  '((:red . (x11 (or "dark red" "brick")))
    (:blue . (x11
              (and "blue"
               (not (/ "^blue$")
                "1" "2" "light" "sky"
                "alice" "medium"  "cadet" "violet" "powder"))))
    (:green . (x11
               (and "green"
                (not (/ "^green$") ;; too light
                 "1" "2" "dark sea" "light" "pale"
                 "yellow" "spring" "lawn"))))
    (:gold .   (x11
                (and "gold" (not "pale" "light" "1" "2"))))
    (:remote . :red)
    (:adb    . :log)
    (:device . :gold)
    (:log    . (exactly "gray10"))
    (:local .  :green)
    (:random . (x11 (or :red :blue :green :gold)))))

(defparameter *test-terms* nil)

(defun colors-file ()
  (first (directory #P"/usr/*/X11/rgb.txt")))

(let (cache)
  (defun x11-colors ()
    (or cache
        (let ((tree (make-instance 'cl-containers:red-black-tree
                                   :sorter #'string-lessp
                                   :test #'string-equal)))
          (prog1 (setf cache tree)
            (with-open-file (in (colors-file))
              (loop
                for line = (read-line in nil nil)
                for trim = (if line (string-trim " " line) "")
                for raw = (ppcre:split "\\s+" trim :sharedp t)
                while line
                unless (find #\! line)
                  do (destructuring-bind (r g b . name) raw
                       (declare (ignore r g b))
                       (let ((color (format nil "~{~a~^ ~}" name)))
                         (cl-containers:insert-item tree color))))))))))

(defun f-or (&rest preds)
  (lambda (v)
    (flet ((test (p) (funcall p v)))
      (some #'test preds))))

(defun f-and (&rest preds)
  (lambda (v)
    (flet ((test (p) (funcall p v)))
      (every #'test preds))))

(defun f-not (&rest preds)
  (lambda (v)
    (flet ((test (p) (funcall p v)))
      (notany #'test preds))))

(defun f-reg (regex)
  (let ((scanner (ppcre:create-scanner regex)))
    (lambda (v)
      (ppcre:scan scanner v))))

(defun has (part)
  (lambda (string)
    (search part string)))

(defun x11-lookup (filter)
  (cl-containers:collect-elements (x11-colors) :filter filter))

(defun lookup-color (color)
  (typecase color
    (null nil)
    (symbol (lookup-color (cdr (assoc color *palette*))))
    (t color)))

(defun parse-background-option (o)
  (flet ((recur (u) (mapcar #'parse-background-option u)))
    (optima:ematch o
      ((list* 'or list)  (apply #'f-or (recur list)))
      ((list* 'and list) (apply #'f-and (recur list)))
      ((list* 'not list) (apply #'f-not (recur list)))
      ((list '/ regex) (f-reg regex))
      ((type string) (has o))
      ((type symbol) (parse-background-option (lookup-color o)))
      ((type function) o))))

(defun query-color (v)
  (labels ((recur (v)
             (typecase v
               (null nil)
               (cons (optima:ematch v
                       ((list 'exactly c) (list c))
                       ((list 'x11 c)
                        (recur (parse-background-option c)))
                       (_
                        ;; assume x11 query
                        (recur (parse-background-option v)))))
               (function (x11-lookup v))
               (string (recur (has v)))
               (symbol (recur (cdr (assoc v *palette*)))))))
    (when v
      (or (recur v)
          (warn "background-option: no match for ~s" v)))))

(defun exactly (c) (list 'exactly c))

(defmacro x11 (quoted)
  `(list 'x11 ',quoted))

(defun background-option (v)
  (when-let (candidates (query-color v))
    (list "-bg" (random-elt candidates))))

(defun no (&rest args)
  (declare (ignore args))
  nil)

(defun toggle (opt)
  (lambda (arg) (and arg (list opt))))

(defun rappend (a b)
  (append b a))

;; (defun tmux-progn (&rest lists)
;;   (loop
;;     for (head . tail) on lists
;;     append head
;;     when tail collect "\\;"))

(defun reflow-options (&aux (hold (option :hold)))
  (declare (ignore hold))
  (when-let (reflow (option :reflow))
    (destructuring-bind (type &optional exec) (option :reflow)
      (flet ((% (e k) (list* (or exec e) k))
             (name () (when-let ((n (option :tmux-name))) `("-n" ,n))))
        (case type
          (:tmux (% "/usr/bin/tmux"
                    (if-let ((session (option :tmux-attach-to)))
                      `("new-window"
                        "-t" ,(format nil "~a:" session)
                        ,@(name)
                        ,@(loop
                            for e in (second (option :env))
                            collect "-e" collect e))
                      `("new"
                        ,@(when-let ((s (option :tmux-session)))
                            `("-s" ,s "-A"))
                        ,@(name)))))
          (:screen (% "/usr/bin/screen" (option .screen)))
          (t (warn "unknown reflow ~a (exec=~a)" type exec)))))))

(defun reduce-set (old new)
  (labels ((analyze (term)
             (etypecase term
               (symbol (analyze (list '+ term)))
               (cons (destructuring-bind (op . syms) term
                       (ecase op
                         (+ (list t   syms))
                         (- (list nil syms))))))))
    (loop
      for term in new
      for (add syms) = (analyze term)
      if add
      nconc syms into union
      else
      nconc syms into subtract
      finally
         (return
           (nunion (nset-difference (remove-duplicates old) subtract)
                   union)))))

(progn
  (defparameter *terminal-options-tree*
    `(_root
      (:term string-downcase
        :directory directory-option
        :ignore-warnings identity
        :status-hook identity
        :wait identity
        :error identity
        :output identity
        :input identity
        .defaults (;; enable colors?
                   (:colors . t)
                   ;; hold term open when the command quits?
                   ;; working directory
                   (:directory . t)
                   ;; should we wait until the terminal closes?
                   (:wait . nil)
                   ;; default geometry
                   (:geometry . "260x20")
                   ;; default tmux-session (nil => create new)
                   (:tmux-session . nil)
                   ;; spawn behaviors
                   (:behavior . nil)
                   ;; default tmux-name
                   (:tmux-name . nil))
        .reducers (:env rappend)
        :behavior identity
        :tmux-session identity
        :tmux-name identity
        :tmux-attach-to identity
        .defaults ((:reflow :tmux) ;; which program to use to reflow lines
                   (.screen "-S" "auto"))                
        :reflow identity
        .reducers (.screen append :behavior reduce-set)
        :execute ,(lambda (args)
                    (list* "-e" (append (reflow-options) args)))
        :env run-program/env-option
        .environment ((:inherit
                       "XDG_RUNTIME_DIR" ; necessary e.g. for alsa
                       "LANG" "SSH_AUTH_SOCK" "DISPLAY" "HOME" "USER" "PATH")))
      (:tmux (:term "tmux"
               :execute ,(lambda (args)
                           (append (rest (reflow-options)) args))
               :options (:no-display)
               :hold ,(constantly ())
               :no-display identity))
      (:shell (:term "sh"
                :execute ,(lambda (args)
                            (and args
                                 (list "-c" (sh-escape (list* "exec" args)))))))
      (_X11 (:geometry ,(arg "-geometry")
             :background background-option
             .options (:background :class :iconic :reverse)
             :class class-option
             :reverse ,(toggle "-rv")
             :iconic ,(toggle "-iconic"))
            (:xterm ,`(:hold hold-option
                       :title ,(arg "-title")
                       .options (:title)
                       .environment ((when (eql :screen (option :reflow))
                                       ("SHELL" . "/usr/bin/screen")))
                       :colors ,(lambda (x) (unless x '("-cm")))
                       :term "xterm")
                    nil
                    ;; example of child terminals which forces specific
                    ;; translations for options.
                    (:uxterm (:term "/usr/local/bin/uxterm"))
                    (:no-color (.defaults ((:colors . nil)
                                           (:background . "black"))))))))

  ;; translate terminal-agnostic options as actual options
  (defparameter *terminal-options*
    (terminal-options *terminal-options-tree*)))

;; (def-terminal :xterm (_x11)
;;   ((:execute (lambda (args) (and args (list* "-e" args))))
;;    (:geometry (arg "-geometry"))
;;    (.options (:background))
;;    (:background background-option)))

(defvar *rec* nil)
(defun options ()
  (append *options*
          (let ((*rec* t))
            (option .defaults))))

(defun resolve-terminal (&optional (name (option% :term)))
  (if-let (entry (assoc (ensure-list name) *terminal-options* :test #'equalp))
    (rest entry)
    (error "Terminal not found: ~s" name)))

(defmacro with-standard-terminal-options ((&rest options) &body body)
  `(let ((*options* *standard-options*))
     (with-terminal-options (,@options)
       ,@body)))

(defmacro with-terminal-options ((&rest options) &body body)
  (flet ((parse-option (env entry)
           (destructuring-bind (x y &key
                                      ((:when w) t wp)
                                      ((:unless u) t up))
               entry
             (multiple-value-bind (x default) (if (consp x)
                                                  (values (first x) (second x))
                                                  (values x nil))
               (assert (member default '(:default nil)))
               (let ((code `((push (cons ,x ,y) ,env))))
                 (when (and wp up)
                   (warn "Both :WHEN and :UNLESS, :WHEN evaluated first"))
                 (when up (setf code `((unless ,u ,@code))))
                 (when wp (setf code `((when ,w ,@code))))
                 (when default (setf code `((unless (optionp ,x) ,@code))))
                 code)))))
    (with-gensyms (env)
      `(let ((,env (list)))
         ,@(loop for o in options append (parse-option env o))
         (let ((*options*
                 (let ((*environment-type* :alist))
                   (combine-environments *options*
                                         ,env
                                         (option .reducers)))))
           ,@body)))))

(defmacro term ((&rest args) &body body)
  `(with-terminal-options ,args
     (terminal ,@body)))

(defun test-terminals (&optional query)
  (setf *test-terms*
        (let (terms)
          (dolist (c (query-color query) terms)
            (with-terminal-options ((:background c) (:title c))
              (push (terminal "bash") terms))))))

(defun option (name &optional (terminal-options (resolve-terminal)))
  (declare (optimize (debug 3)))
  (let* ((not-found (load-time-value (gensym "not-found")))
         (terminal-option (getf terminal-options name not-found)))
    (if (eq terminal-option not-found)
        (unless (option :ignore-warnings)
          (warn "Ignoring option ~s" name))
        (let ((option (assoc name (if *rec* *options* (options)))))
          (typecase terminal-option
            ((or function (and symbol (not null)))
             (funcall terminal-option (cdr option)))
            (t terminal-option))))))

(defun run-program-wrapper (&rest args)
  (apply #'sb-ext:run-program args))

(defun terminal% (program
                  &key
                    args
                    (hold (option :hold))
                    (colorp (option :colors))
                    (geometry (option :geometry)))
  (with-terminal-options ((:execute (and program (list* program args)))
                          ((:title :default) program))
    (let ((directory (probe-file (option :directory)))
          (program (option :term))
          (arguments (append hold
                             colorp
                             geometry
                             (mappend #'option (option .options))
                             ;; :execute must be last for xterm
                             (option :execute))))
      (format *debug-io* "~&;; ~a~%" (sh-escape (list* program arguments)))
      (apply #'run-program-wrapper
             program
             arguments
             :directory directory
             :search t
             :status-hook (option :status-hook)
             :wait (option :wait) 
             :output (option :output)
             :input (option :input)
             :error (option :error)
             (option :env)))))

(defun execute% (program &key args)
  (format *debug-io* "~&~a~%" (sh-escape (list* program args)))
  (apply #'sb-ext:run-program
         program
         args
         :directory (option :directory)
         :search t
         :status-hook (option :status-hook)
         :wait (option :wait)
         :output (option :output)
         :input (option :input)
         :error (option :error)
         (option :env)))

(defun execute (program &rest args)
  (execute% program :args args))

(defun terminal (&optional program &rest args)
  (declare (notinline terminal%))
  (terminal% program :args (map 'list #'princ-to-string (remove nil args))))

(defvar *standard-options* '((:term :xterm)))
