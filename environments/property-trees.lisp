(in-package :bricabrac.environmenst)

;; alist
(defvar *cons-node-walkers* '())

(defgeneric walk-meta-node-for-kind
    (kind &key path arguments children environment recurse))

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
       ;;  (node1 bindings1)
       ;;  (node2 bindings2))
       ;;
       (loop for (name bindings) in arguments
             for index from 0
             do (assert (symbolp name))
                (funcall recurse `(,(case name
                                      (:index index)
                                      (t name))
                                   ,bindings ,@children)
                         path
                         env)))
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

(defun map-property-leaves (tree callback &key env reducers)
  "Apply CALLBACK to each leaves of the property tree.

CALLBACK is a function accepting:

  - PATH, a list of symbols of each node from current leaf to root, without
    NIL.

  - zero or more keyword arguments, obtained by calling APPLY on the combined
    ENV associated with each leaf.

The return value of CALLBACK is not used.

ENV is an optional environment (property list) that can be used to provide
additional bindings inside CALLBACK. REDUCERS is a plist mapping attributes to
reducing function, which are called to combine old and new attributes when the
environment is extended. See COMBINE-ENVIRONMENTS."
  (labels
      ((recurse (tree path env)
         (etypecase tree
           (cons
            ;; A tree is a name (possibly NIL or compound), a list of additional
            ;; bindings (either absent, or NIL, or a proper list of bindings),
            ;; as well as zero or more subtrees.
            (destructuring-bind (head &optional bindings &rest children) tree
              (let ((env (combine-environments env bindings reducers)))
                (typecase head
                  (cons (walk-meta-node head path children env #'recurse))
                  (atom
                   (when (and head (not (and (symbolp head)
                                             (string= :_ head))))
                     ;; Only add HEAD in front of PATH if it is not NIL, or if
                     ;; the NIL node is a leaf.  Internal NIL nodes are useful
                     ;; to introduce properties without cluttering the tree with
                     ;; useless names.  Leaf NIL nodes are useful to provide a
                     ;; dummy sprite (an entry point in the tree).
                     (push head path))
                   (if children
                       (dolist (child children)
                         (recurse child path env))
                       (apply callback path env)))))))
           (atom (recurse (list tree) path env)))))
    (recurse tree nil env)))

(defmacro do-property-leaves (((path &rest lambda-list)
                               tree &key result reducers) &body body)
  (with-gensyms (env)
    `(block nil
       (map-property-leaves ,tree (lambda (,path &rest ,env)
                                    (destructuring-bind ,lambda-list ,env
                                      ,@body))
                            :reducers ,reducers)
       (return ,result))))

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
