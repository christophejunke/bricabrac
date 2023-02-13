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

(defun arg (option &optional (key nil kp))
  (flet ((opt (v) (and v (list option v))))
    (if kp
        (lambda (v)
          (declare (ignore v))
          (opt (option key)))
        #'opt)))

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

(defun toggle (opt)
  (lambda (arg) (and arg (list opt))))

;; (defun hold-option (v)
;;   (and v '("-hold")))

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

(defun rappend (a b)
  (append b a))

;; (defun tmux-progn (&rest lists)
;;   (loop
;;     for (head . tail) on lists
;;     append head
;;     when tail collect "\\;"))

(defun reflow-options ()
  (when-let (reflow (option :reflow))
    (destructuring-bind (type &optional exec) (option :reflow)
      (flet ((% (e k) (list* (or exec e) k))
             (name () (when-let ((n (option :tmux-name)))
                        `("-n" ,(format nil "[~a]" n))))
             (back () (when (option :background)
                        '("-d"))))
        (case type
          (:tmux 
           (% "/usr/bin/tmux"
              (if-let ((session (option :tmux-attach-to)))
                `("new-window"
                  "-t" ,(format nil "~a:" session)
                  ,@(back)
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

(defun identity-or (another)
  (lambda (v) (or v (option another))))

;;; EXAMPLE
;;
;; saturating list: a list of values or T, T means "everything"
;; folding is the union of values with a special case for T 
 
(defun fold-saturating (old new)
  (cond
    ((or (eq old t)
         (eq new t)))
    (t (union old (remove-duplicates (ensure-list new))))))

(defun list-saturating-option (value)
  ;; returns a function (see option*)
  (if (eq value t)
      ;; (option* key ...) => T
      (constantly t)
      (lambda (&optional (query nil query-p))
        (if query-p
            ;; (option* key v) => v belongs to value list
            (member query value) 
            ;; (option* key) => the whole list
            value))))

(defun option* (key &rest args)
  (apply (option key) args))

;;; -----

(progn
  (defparameter *terminal-options-tree*
    `(_root
      (:term string-downcase
        :debug list-saturating-option
        :final list-saturating-option
        :directory directory-option
        :ignore-warnings identity
        :status-hook identity
        :wait identity
        :error identity
        :output identity
        :input identity
        .defaults (;; enable colors?
                   (:colors . t)
                   ;; debugging (may be a list)
                   (:debug . nil)
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
        .reducers (:env rappend :debug fold-saturating :final fold-saturating)
        :behavior identity
        :tmux-session identity
        :tmux-name ,(identity-or :title)
        :tmux-attach-to identity
        .defaults ((:reflow :tmux) ;; which program to use to reflow lines
                   (.screen "-S" "auto"))                
        :reflow identity
        .reducers (.screen append :behavior reduce-set)
        .options (:execute)
        :execute ,(lambda (args)
                    (list* "-e" (append (reflow-options) args)))
        :env run-program/env-option
        .environment ((:inherit
                       "XDG_RUNTIME_DIR" ; necessary e.g. for alsa
                       "LANG" "SSH_AUTH_SOCK" "DISPLAY" "HOME" "USER" "PATH")))
      (:debug (:term nil))
      (:tmux (:term "tmux"
               :execute ,(lambda (args)
			   (let ((args (ecase (option :hold)
					 ((:error)
					  (warn "not implemented")
					  (append args (list ";" "set" "remain-on-exit" "on")))
					 ((:always t)
					  (append args (list ";" "set" "remain-on-exit" "on")))
					 ((:never nil)
					  args))))
			     (append (rest (reflow-options)) args)))

               ;; tmux connects to a server which spawns a pane,
               ;; making the script always asynchronous but the
               ;; (constantly t) here is to make sure to wait for the
               ;; new pane to be created; FIXME: for tmux there should
               ;; be an option to make the thread wait for completion
               ;; instead so that :wait nil/t makes sense. BUT the
               ;; :wait passed down to run-program must be T always.

               ;; always true, but in two distinct ways
               :wait ,(lambda (v) (if v t :no-wait))

               :background identity
	       :hold identity
	       :title identity
	       :options (:no-display :background)
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
            (:xterm (:hold ,(toggle "-hold")
                     ;; execute must be last
                     .options (:title-arg :hold :geometry :colors)
                     .environment ((when (eql :screen (option :reflow))
                                     ("SHELL" . "/usr/bin/screen")))
                     :colors ,(lambda (x) (unless x '("-cm")))
                     :title identity
                     :title-arg ,(arg "-title" :title)
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
             (multiple-value-bind (x modifier dp) 
                 (if (consp x)
                     (values (first x) (second x) t)
                     (values x nil nil))
	       (when dp
		 (assert #1=(member modifier '(:default :final))
			 ()
			 (if #1#
			     "Modifier keyword should be in second position"
			     "Expected :DEFAULT or :FINAL in second position")))
               (let ((code `((push (cons ,x ,y) ,env))))
                 (when (and wp up)
                   (warn "Both :WHEN and :UNLESS, :WHEN evaluated first"))
                 (when up (setf code `((unless ,u ,@code))))
                 (when wp (setf code `((when ,w ,@code))))
                 (when modifier 
                   (setf code 
                         (ecase modifier
                           (:default `((unless (optionp ,x) ,@code)))
                           (:final `(,@code
                                     (push (cons :final ,x) ,env))))))
                 `((unless (option* :final ,x) ,@code)))))))
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

(defun terminal-arguments% (program args)
  (declare (optimize (debug 3)))
  (with-terminal-options ((:execute (and program (list* program args)))
                          ((:title :default) program))
    (let ((directory (probe-file (option :directory)))
          (program (option :term))
          (arguments (mappend #'option (option .options))))
      (values program
              arguments
              (list* :directory directory
                     :search t
                     :status-hook (option :status-hook)
                     :wait (option :wait) 
                     :output (option :output)
                     :input (option :input)
                     :error (option :error)
                     (option :env))))))

(defun terminal% (program &key args)
  (multiple-value-bind (program arguments more-arguments)
      (terminal-arguments% program args)
    (when program
      (apply #'run-program-wrapper
             program
             arguments
             more-arguments))))

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

(defun tmux-sessions ()
  (with-pipeline (:channel t)
    (program "tmux" "list-sessions" "-F" "#{session_name}")
    #'signal-each-line))

(defun best-tmux-session (candidates)
  (let ((sessions (tmux-sessions)))
    (or (loop for c in candidates thereis (find c sessions :test #'string=))
        (first sessions))))

(defmacro with-tmux-session ((&rest choices) &body body)
  (let ((best (gensym)))
    `(let ((,best (best-tmux-session (list ,@choices))))
       (with-terminal-options ((:tmux-attach-to ,best))
         ,@body))))
