(in-package #:bricabrac.property-trees)

(define-local-keyword .children)
(define-local-keyword .cache)

(defclass node ()
  ((name :initarg :name :accessor node-name)
   (parent :initarg :parent :accessor node-parent)
   (environment :initarg :environment :writer (setf node-environment))
   (.cache :initform nil :accessor .cache)
   (.children :initform nil :accessor .children)))

(defun %add-child (parent child)
  (when (and parent child)
    (push child (.children parent))))

(defun %remove-child (parent child)
  (when (and parent child)
    (setf (.children parent)
          (remove child (.children parent)))))

(defun %destroy-node (node)
  (when node
    (let ((parent (node-parent node))
          (children (.children node)))
      (%remove-child parent node)
      (dolist (c children)
        ;; silently detach
        (setf (slot-value c 'parent) nil))
      (slot-makunbound node 'parent)
      (slot-makunbound node 'environment)
      (slot-makunbound node .children)
      (slot-makunbound node .cache)
      (dolist (c children)
        ;; propagates
        (setf (.cache c) nil)))))

(defmethod (setf node-parent) :around (parent (node node))
  (let ((previous (slot-value node 'parent)))
    (cond
      ((eq parent previous)
       parent)
      ((not parent)
       (%remove-child previous node))
      (t
       (let ((result (call-next-method)))
         (prog1 result
           (%remove-child previous node)
           (%add-child parent node)
           (setf (.cache node) nil)))))))

(defmethod (setf node-parent) :before (parent (node node))
  (when parent
    (loop 
      for n = parent then (node-parent n)
      while n
      when (eq n node)
        do (error "~a is an ancestor of ~a" node parent))))

(defmethod (setf node-environment) :around (environment (node node))
  (let ((previous (slot-value node 'environment)))
    (cond
      ((equalp previous environment)
       environment)
      (t
       (let ((result (call-next-method)))
         (prog1 result
           (setf (.cache node) nil)))))))

(define-condition cache-clear () 
  ((node :initarg :node :reader cache-clear-node)))

(defmethod (setf .cache) :around (cache (node node))
  (let ((previous (slot-value node .cache)))
    (prog1 (call-next-method)
      (unless cache
        (when previous
          (signal 'cache-clear :node node)
          (dolist (c (.children node))
            (setf (.cache c) nil)))))))

(defun find-node (name)
  (when name
    (typecase name
      (null)
      (node name)
      (symbol
       (let ((n (get name 'node)))
         (prog1 n
           (check-type n (or null node))))))))

(defun (setf find-node) (node name)
  (check-type node (or node null))
  (let ((current (find-node name)))
    (cond
      ((eq current node) node)
      (t (when current
           (%destroy-node current))
         (setf (get name 'node) node)))))

(defun make-node% (name class parent environment)
  (let ((object (allocate-instance class)))
    (prog1 object
      (initialize-instance object :parent nil :environment nil :name name)      
      (setf (node-parent object) parent)
      (setf (node-environment object) environment))))

(defun ensure-node (n c p e &aux dirty)
  (let* ((current (find-node n))
         (parent (and p (or (find-node p) (error "Unknown parent ~s" p))))
         (class (case c
                  (:inherit (class-of parent))
                  (t (find-class (or c 'node)))))
         (environment e))
    (flet ((%fill (x class-changed)
             (prog1 x
               (when class-changed
                 (setf (.cache x) nil))
               ;; name change does not clear cache
               (setf (node-name x) n)
               (setf (node-parent x) parent)
               (setf (node-environment x) environment)
               ;; refresh dirty leaves
               (map () #'node-environment dirty)))
           (%handle-dirty (c &aux (n (cache-clear-node c)))
             (unless (.children n)
               (push n dirty))))
      (handler-bind ((cache-clear #'%handle-dirty))
        (cond
          ((not current)
           (setf (find-node n) (make-node% n class parent environment)))
          ((eq (class-of current) class)
           (%fill current nil))
          (t
           (%fill (change-class current class) t)))))))

(defun expand-define-global-node (name class parent environment)
  `(ensure-node ',name 
                ,(or class (and parent :inherit))
                ',parent
                (progn ,@environment)))

(defun node-environment (node/name &aux (node (find-node node/name)))
  (when node/name
    (or (.cache node)
        (setf (.cache node)
              (progn 
                (warn "recomputing for ~s" (node-name node))
                (fold-environments% (node-environment (node-parent node))
                                    (slot-value node 'environment)))))))

(defmacro defnode (name (&rest options) &body environment)
  "Create or update node definitions identified by NAME.

NAME is a symbol (see FIND-NODE).

OPTIONS :: PARENT &KEY CLASS | &KEY PARENT CLASS

If provided, PARENT is a literal symbol (unevaluated); the simplified
syntax is such that if the first element in OPTIONS is not a known
keyword it is implicitly treated as the value associated with :PARENT
keyword.

If provided, CLASS is an expression to be evaluated and represents the
class of node to allocate (if new) or to call CHANGE-CLASS with (if
updating an existing node). If not provided and the node has a parent,
the class defaults to the class of the parent.

ENVIRONMENT is evaluated and must produce a property list."
  (destructuring-bind (&key parent class)
      (if (or (null options)
              (member (first options) '(:parent :class)))
          options
          (list* :parent options))
    (expand-define-global-node name class parent environment)))

(defmethod print-object ((o node) stream)
  (print-unreadable-object (o stream :type t :identity t)
    (format stream "~s" (node-name o))))

;; (defnode :a () '(.fold-mapping (:name (list* #:new #:old))
;;                  :name "A"))
;;
;; (defnode :b (:a) '(:name "B"))
;; (defnode :b.0 (:b) '(:name "zero"))
;; (defnode :b.1 (:b) '(:name "one"))

