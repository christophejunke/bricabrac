(in-package :bricabrac.environments)

;; alist of code walkers based on node type key is either T
;; (catch-all), a symbol or a list of symbols matching the head of the
;; compound form to walk with the associated walker function. The
;; walker must have the same signature as walk-compound-node-for-kind.

(defvar *cons-node-walkers* '((t . walk-compound-node-for-kind)))

(defvar *current-reducers* nil)

(defun map-property-leaves (tree callback &key env reducers)
  "Apply CALLBACK to each leaves of the property tree.

CALLBACK is a function accepting:

  - PATH, a list of symbols of each node from current leaf to root, without
    NIL or names starting with an underscore.

  - zero or more keyword arguments, corresponding to the combined ENV
    associated with each leaf.

The return value of CALLBACK is not used.

ENV is an optional environment (property list) that can be used to provide
additional bindings inside CALLBACK. REDUCERS is a plist mapping attributes to
reducing function, which are called to combine old and new attributes when the
environment is extended. See COMBINE-ENVIRONMENTS."
  (flet ((walker (node-head)
           (destructuring-bind (node-kind . arguments) node-head
             (values (or (cdr (assoc-if (lambda (key) 
                                          (or (eq key t)
                                              (member node-kind
                                                      (ensure-list key))))
                                        *cons-node-walkers*))
                         (error "No matcher found for node ~a" node-head))
                     node-kind
                     arguments)))
         (path-component-p (head)
           (typecase head
             (symbol (not (string= :_ (char (string head) 0))))))
         (wrap-with-path (path)
           (lambda (condition)
             (typecase condition
               (path-wrapped-error nil)
               (t (error 'path-wrapped-error
                         :path path
                         :error condition))))))
    (labels
        ((recurse (tree path env)
           "Visit TREE with current node path PATH and environment ENV"
           (etypecase tree
             (atom
              (recurse (list tree) path env))
             (cons
              ;; A tree is a name (possibly NIL or compound), a list of
              ;; additional bindings (either absent, or NIL, or a proper list
              ;; of bindings), as well as zero or more subtrees.
              (destructuring-bind (head &optional bindings &rest children) tree
                (let ((env (combine-environments env bindings reducers)))
                  (typecase head
                    (cons
                     ;; The node name itself is a compound term: the
                     ;; way this node should be visited is determined
                     ;; by custom code walkers
                     ;; (e.g. walk-compound-node-for-kind)
                     (with-simple-restart (ignore "Ignore node ~s" tree)
                       (multiple-value-bind (walker kind args) (walker head)
                         (funcall walker
                                  kind
                                  :path path
                                  :arguments args
                                  :children children
                                  :environment env
                                  :recurse #'recurse))))
                    (t
                     (when (path-component-p head)
                       (push head path))
                     (if children
                         (dolist (child children)
                           (recurse child path env))
                         (handler-bind ((error (wrap-with-path path)))
                           (apply callback path env)))))))))))
      (let ((*current-reducers* reducers))
        (recurse tree nil env)))))

(defmacro do-property-leaves (((path &rest lambda-list)
                               tree &key result reducers) 
                              &body body)
  (with-gensyms (env)
    `(block nil
       (map-property-leaves ,tree
                            (lambda (,path &rest ,env)
                              ,@(if lambda-list
                                    `((destructuring-bind ,lambda-list ,env
                                        ,@body))
                                    `((declare (ignore ,env))
                                      ,@body)))
                            :reducers (or ,reducers *current-reducers*))
       (return ,result))))

(defgeneric walk-compound-node-for-kind
    (kind &key path arguments children environment recurse))

(defmethod walk-compound-node-for-kind ((node-kind (eql :each)) 
                                        &key path arguments environment 
                                          recurse children &allow-other-keys)
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
                                ,environment
                                ,@subtrees))
          (funcall recurse
                   `(_ ,bindings
                       ,@children)
                   (append sub-path path)
                   leaf-env))))

(defmethod walk-compound-node-for-kind ((node-kind (eql :path)) 
                                    &key path arguments environment 
                                      recurse children &allow-other-keys)
  (funcall recurse
           (second (reduce (lambda (name tree)
                             (list () (list* name tree)))
                           arguments
                           :from-end t
                           :initial-value `(() ,@children)))
           path
           environment))

(define-condition path-wrapped-error (error)
  ((path :accessor path-wrapped-error/path :initarg :path)
   (error :accessor path-wrapped-error/error :initarg :error))
  (:report (lambda (condition stream)
             (format stream
                     "At node ~{~A~^ -> ~}.~%~A"
                     (reverse (path-wrapped-error/path condition))
                     (path-wrapped-error/error condition)))))

(defun test-1 ()
  (terpri)
  (do-property-leaves 
      ((path &key x)
       '(nil                    ; root anonymous node
         (:x 1)               ; environment of root node, bind :x to 1
         a                    ; first child (leaf)
         b                    ; second child (leaf)
         (c (:x 3)) ; third child (leaf), with a different environment
         (
          ;; ":each" meta-node which introduces the 2 next childre, each one with a
          ;; different environment, which inherits from the meta-node's environment.
          (:each (left (:x -1)) (right (:x 1)))
          (:x 10) ;; the meta-node's environment
          u ;; the children in a :each meta-node are logically copied
          v ;; for each node introduced by the meta-node: there are "u", "v" and "w"
          w ;; children under "left" and "right".
          ((:path a b c d) ;; add intermediate nodes in path for all children
           (:x 5)          ;; path environment
           e
           f)
          (x nil
           ((:each
             (:index (:x 0))
             (nil)
             (:index (:x 30)))
            nil))))
       ;; x property is combined by multiplying with
       ;; previous value, to help identitfing how
       ;; environments interact
       :reducers (list :x (lambda (old new) (* old new))))
    (format t "~8<x = ~a~> : ~{~a~^ / ~}~%" x (reverse path))))

(defun test-2 ()
  (terpri)
  (do-property-leaves ((path &rest things)
                       '((:each
                          (x (:special 1) x1 (x2 (:special 3) x21 x22))
                          (y (:special 2))
                          (z))
                         (:special 0)
                         (a (:special 4))
                         b))
    (print (list (reverse path) things))))


