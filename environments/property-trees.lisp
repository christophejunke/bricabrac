(in-package :bricabrac.environments)

;; alist
(defvar *cons-node-walkers* '())

(declaim (ftype function map-property-leaves))

(defvar *current-reducers* nil)

(defclass property-tree ()
  ((root :initarg :root :accessor property-tree-root)
   (hash :initform (make-hash-table :test #'equalp)
         :reader property-tree-hash)))

(defun property-tree (tree)
  (let ((o (make-instance 'property-tree :root tree)))
    (prog1 o
      (let ((hash (property-tree-hash o)))
        (do-property-leaves ((path &rest env) tree)
          (setf (gethash (reverse path) hash) env))))))

(defmacro define-property-tree (name (&key parent) &body body)
  (destructuring-bind (root) body
    (when parent
      (destructuring-bind (tree &rest path) parent
        (with-gensyms (child)
          (setf root
                `(let ((,child ,root))
                   `((:root ,',tree ,@',path) () ,,child))))))
    `(register-property-tree ',name (property-tree ,root))))

(defun pt-get (pt path)
  (gethash path (property-tree-hash (ensure-pt pt))))

(defmacro do-property-leaves (((path &rest lambda-list)
                               tree &key result reducers) &body body)
  (with-gensyms (env)
    `(block nil
       (map-property-leaves ,tree (lambda (,path &rest ,env)
                                    ,@(if lambda-list
                                          `((destructuring-bind ,lambda-list ,env
                                              ,@body))
                                          `((declare (ignore ,env))
                                            ,@body)))
                            :reducers (or ,reducers *current-reducers*))
       (return ,result))))

(defgeneric walk-meta-node-for-kind
    (kind &key path arguments children environment recurse))

(defmethod walk-meta-node-for-kind ((_ (eql :root))
                                    &key path arguments children environment recurse)
  (destructuring-bind (root &rest selector) arguments
    (let ((initial-env (pt-get root selector)))
      (when environment
        (warn "Discarding environment ~a" environment))
      (funcall recurse
               `(_ ,initial-env ,@children) 
               (append path (reverse selector)) nil))))

(defun walk-meta-node (cons path children env recurse)
  (destructuring-bind (node-kind . arguments) cons
    ;; compound names represent special nodes
    (case node-kind
      (:each
       ;; Iterate over alternative branches, and for each of
       ;; them, establish the bindings associated with it while
       ;; processing children.
       ;;
       ;; (:each
       ;;  (node1 bindings1 . subtrees1)
       ;;  (node2 bindings2 . subtrees2))
       ;;
       ;; TODO: MORE DOCS! "SUBTREES" is NEW! MORE TESTS, MORE EXAMPLES.
       (loop
         for (name bindings . subtrees) in arguments
         for index from 0
         do (do-property-leaves ((sub-path &rest leaf-env)
                                 `(,(case name
                                      (:index index)
                                      (t name))
                                   ,env
                                   ,@subtrees))
              (funcall recurse
                       `(_ ,bindings
                           ,@children)
                       (append sub-path path)
                       leaf-env))))
      (:path
       ;; 
       ;; 
       (funcall recurse
                (second
                 (reduce (lambda (name tree)
                           (list () (list* name tree)))
                         arguments
                         :from-end t
                         :initial-value `(() ,@children)))
                path
                env))
      (t (restart-case
             (if-let (walker (assoc node-kind *cons-node-walkers*))
               (funcall (cdr walker)
                        :path path
                        :arguments arguments
                        :children children
                        :environment env
                        :recurse recurse)
               (walk-meta-node-for-kind node-kind
                                        :path path
                                        :arguments arguments
                                        :children children
                                        :environment env
                                        :recurse recurse))
           (ignore () :report "Ignore current node"))))))

;; (defvar *export-property-tree-symbols* nil)
;; (when (and *export-property-tree-symbols* (symbolp head))
;;                        ;; Intermediate nodes can be integers, etc.
;;                        (export head (symbol-package head)))

(define-condition path-wrapped-error (error)
  ((path :accessor path-wrapped-error/path :initarg :path)
   (error :accessor path-wrapped-error/error :initarg :error))
  (:report (lambda (condition stream)
             (format stream
                     "At node ~{~A~^ -> ~}.~%~A"
                     (reverse (path-wrapped-error/path condition))
                     (path-wrapped-error/error condition)))))

(defun map-property-leaves (tree callback &key env reducers)
  "Apply CALLBACK to each leaves of the property tree.

CALLBACK is a function accepting:

  - PATH, a list of symbols of each node from current leaf to root, without
    NIL or names starting with _.

  - zero or more keyword arguments, obtained by calling APPLY on the combined
    ENV associated with each leaf.

The return value of CALLBACK is not used.

ENV is an optional environment (property list) that can be used to provide
additional bindings inside CALLBACK. REDUCERS is a plist mapping attributes to
reducing function, which are called to combine old and new attributes when the
environment is extended. See COMBINE-ENVIRONMENTS."
  (let ((*current-reducers* reducers))
    (labels
        ((recurse (tree path env)
           (etypecase tree
             (cons
              ;; A tree is a name (possibly NIL or compound), a list of
              ;; additional bindings (either absent, or NIL, or a proper list
              ;; of bindings), as well as zero or more subtrees.
              (destructuring-bind (head &optional bindings &rest children) tree
                (let ((env (combine-environments env bindings reducers)))
                  (typecase head
                    (cons (walk-meta-node head path children env #'recurse))
                    (atom
                     (when (and head
                                (not
                                 (and (symbolp head)
                                      (string= :_ (char (string head) 0)))))
                       ;; Only add HEAD in front of PATH if it is not NIL, or
                       ;; if the NIL node is a leaf.  Internal NIL nodes are
                       ;; useful to introduce properties without cluttering the
                       ;; tree with useless names.  Leaf NIL nodes are useful
                       ;; to provide a dummy sprite (an entry point in the
                       ;; tree).
                       (push head path))
                     (if children
                         (dolist (child children)
                           (recurse child path env))
                         (handler-bind ((error
                                          (lambda (condition)
                                            (typecase condition
                                              (path-wrapped-error (error condition))
                                              (t (error 'path-wrapped-error
                                                        :path path
                                                        :error condition))))))
                           (apply callback path env))))))))
             (atom (recurse (list tree) path env)))))
      (recurse tree nil env))))

(defvar *property-leaves-test*
  '(nil        ; root anonymous node
    (:x 1)     ; environment of root node, bind :x to 1
    a          ; first child (leaf)
    b          ; second child (leaf)
    (c (:x 3)) ; third child (leaf), with a different environment
    (
     ;; ":each" meta-node which introduces the 2 next childre, each one with a
     ;; different environment, which inherits from the meta-node's environment.
     (:each (left (:x -1)) (right (:x 1)))
     (:x 10) ;; the meta-node's environment
     
     u ;; the children in a :each meta-node are logically copied
     v ;; for each node introduced by the meta-node: there are "u", "v" and "w"
     w ;; children under "left" and "right".
     (x nil
      ((:each
        (:index (:x 0))
        (nil)
        (:index (:x 30))) nil)))))

(defun propert-leaves-test ()
  (terpri)
  (do-property-leaves ((path &key x)
                       *property-leaves-test*
                       :reducers (list :x (lambda (old new) (* old new))))
    (format t "~8<x = ~a~> : ~{~a~^ / ~}~%" x (reverse path))))

;; (progn
;;   (terpri)
;;   (do-property-leaves ((path &rest things) '((:each
;;                                               ((:path x u) ())
;;                                               ((:path x v) ())
;;                                               (y ()))
;;                                              ()
;;                                              a
;;                                              b))
;;     (print (list (reverse path) things))))

;; ((X U A) NIL) 
;; ((X U B) NIL) 
;; ((X V A) NIL) 
;; ((X V B) NIL) 
;; ((Y A) NIL) 
;; ((Y B) NIL)

;; (progn
;;   (terpri)
;;   (do-property-leaves ((path &rest things)
;;                        '((:each
;;                           (x (:special 1) x1 (x2 (:special 3) x21 x22))
;;                           (y (:special 2))
;;                           (z))
;;                          (:special 0)
;;                          (a (:special 4))
;;                          b))
;;     (print (list (reverse path) things))))

