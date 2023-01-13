(in-package :bricabrac.environments)

(defgeneric walk-compound-node-for-kind
    (kind &key path arguments children environment recurse)
  (:documentation
   "How to visit lists as node names

A node matching ((KIND . ARGUMENTS) PLIST CHILDREN) is being visited.
PLIST has already been combined to a contextual environment to produce
the ENVIRONMENT argument. PATH is the current path to root in the
tree, composed of path names that satifsy PATH-COMPONENT-P. CHILDREN
is a list of zero or more child nodes. 

RECURSE is a function of there parameters: a TREE to visit, a PATH and
an ENVIRONMENT."))

(defparameter *cons-node-walkers*
  (acons t #'walk-compound-node-for-kind nil))

(defun walker (node-head)
  (destructuring-bind (node-kind . arguments) node-head
    (values (or (cdr (assoc-if (lambda (key) 
                                 (or (eq key t)
                                     (member node-kind
                                             (ensure-list key))))
                               *cons-node-walkers*))
                (error "No matcher found for node ~a" node-head))
            node-kind
            arguments)))

(defun path-component-p (head)
  (typecase head
    (symbol (not (string= :_ (char (string head) 0))))))

(defun error/path (path)
  (lambda (condition)
    (typecase condition
      (path-wrapped-error nil)
      (t (error 'path-wrapped-error
                :path path
                :error condition)))))

(defun map-property-leaves (tree callback &key environment reducers)
  (labels
      ((visit (tree path env)
         (etypecase tree
           (cons
            (destructuring-bind (node &optional plist &rest children) tree
              (let ((env (combine-environments env plist reducers)))
                (typecase node
                  (cons
                   (with-simple-restart (ignore "ignore tree ~s" tree)
                     (multiple-value-bind (walker kind args) (walker node)
                       (funcall walker
                                kind
                                :path path
                                :arguments args
                                :children children
                                :environment env
                                :recurse #'visit))))
                  (otherwise
                   (when (path-component-p node)
                     (push node path))
                   (if children
                       (dolist (child children)
                         (visit child path env))
                       (handler-bind ((error (error/path path)))
                         (apply callback path env))))))))
           (atom
            (visit (list tree) path environment)))))
    (let ((*current-reducers* reducers))
      (visit tree nil environment))))

(defmacro do-property-leaves (((path &rest lambda-list)
                               tree &key result reducers) 
                              &body body)
  (with-gensyms (env)
    `(block nil
       (map-property-leaves 
        ,tree
        (lambda (,path &rest ,env)
          ,@(if lambda-list
                `((destructuring-bind ,lambda-list ,env ,@body))
                `((declare (ignore ,env)) ,@body)))
        :reducers (or ,reducers *current-reducers*))
       (return ,result))))


(defmethod walk-compound-node-for-kind ((node-kind (eql :each)) 
                                        &key path arguments environment 
                                          recurse children &allow-other-keys)
  "For-each construct in property tree

Iterate over alternative branches, and for each of
them, establish the bindings associated with it while
processing children.

    (:each
     (node1 bindings1 . subtrees1)
     (node2 bindings2 . subtrees2))

"
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
