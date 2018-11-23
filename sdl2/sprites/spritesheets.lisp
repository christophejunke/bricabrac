(in-package :bricabrac.sdl2.sprites.spritesheets)

(defclass spritesheet ()
  ((name
    :type symbol
    :accessor spritesheet-name
    :accessor name
    :initarg :name)
   (;; tree of sprite nodes
    tree
    :accessor spritesheet-tree)
   (;; a vector of leaf SPRITE-DESCRIPTION instances, ordered
    ;; according to their ID slot.
    sprites
    :type (or vector null)
    :accessor spritesheet-sprites)
   (;; source s-expr from which the tree is derived
    source
    :type list
    :accessor spritesheet-source)
   (;; A simple vector of all unique tileset pathnames declared in the tree
    ;; through the :FILE property. Each SPRITE-DESCRIPTION instance contains an
    ;; index in this vector that refers to a file in this vector.
    texture-files
    :initform #() :type simple-vector :accessor spritesheet-texture-files)))

(defmethod print-object ((spritesheet spritesheet) stream)
  (print-unreadable-object (spritesheet stream :identity t :type t)
    (format stream
            "~a (~d sprite~:p)"
            (name spritesheet)
            (length (spritesheet-sprites spritesheet)))))

(defun make-spritesheet (&key name (class 'spritesheet))
  (make-instance class :name name))

;; spritesheet tree : tree of internal nodes (spritesheet-tree-node) with
;; sprite-description objects as leaves.

;; spritesheet sexp : external representation, parsed according to
;; map-spritesheet-sexp-leaves

(defmethod attribute-reducer ((attribute (eql :transform)) old new)
  (case new
    (:reset (identity-transform))
    (t (combine old new))))

(defclass spritesheet-tree-element (has-name
                                    has-parent
                                    has-children)
  ())

(defclass spritesheet-tree-node (spritesheet-tree-element)
  ((range :initarg :range :accessor range :initform nil)))

;; remove long accessors

(defclass sprite-description (spritesheet-tree-element)
  ((spritesheet
    :initarg :spritesheet
    :accessor sprite-description-spritesheet
    :accessor spritesheet)
   (id
    :initarg :id
    :accessor sprite-description-id
    :accessor id)
   (rectangles
    :initarg :rectangles
    :accessor sprite-description-rectangles
    :accessor rectangles)
   (reverse-path
    :initarg :reverse-path
    :accessor sprite-description-reverse-path
    :accessor reverse-path)
   (env
    :initarg :env
    :accessor sprite-description-env
    :accessor environment)
   (texture-id
    :initarg :texture-id
    :accessor sprite-description-texture-id
    :accessor texture-id)
   (timestamp
    :reader timestamp
    :initarg :timestamp
    :initform (get-internal-run-time))))

(defmethod range ((sd sprite-description))
  (let ((id (sprite-description-id sd)))
    (cons id id)))

(defmethod print-object ((object sprite-description) stream)
  (print-unreadable-object (object stream)
    (format stream
            "~a: ~a"
            (name (sprite-description-spritesheet object))
            (name object))))

(defun follow-path-from-root (root path &optional strictp)
  (if (endp path)
      root
      (destructuring-bind (child . path) path
        (let ((next (or (find-child-by-name root child)
                        (and (not strictp) root))))
          (when next
            (follow-path-from-root next path strictp))))))

(defun follow-path-to-leaf (root path)
  (let ((node (follow-path-from-root root path)))
    (etypecase node
      (sprite-description node)
      (spritesheet-tree-node
       (let ((unique-child (ignore-errors (aref (children node) 0))))
         (typecase unique-child
           (sprite-description unique-child)
           (spritesheet-tree-node nil)))))))

(define-condition branch-not-found ()
  ((root :initarg :root :accessor branch-not-found/root)
   (branch :initarg :branch :accessor branch-not-found/branch)))

(defun switch-branch (root branch &aux (branch-path (ensure-list branch)))
  (flet ((path-to-leaf-p (node stack)
           (and (not (eq root node))
                (let ((child (follow-path-from-root node branch-path :strict)))
                  (and child
                       (follow-path-to-leaf child stack))))))
    (or (loop
          for stack = nil then (cons (name node) stack)
          for node = (parent root) then (parent node)
          while node
            thereis (path-to-leaf-p node stack))
        (error 'branch-not-found :root root :branch branch))))

;; anamorphism
(defun make-spritesheet-tree-builder ()
  "Return a root node and a closure that builds a tree rooted under it.

The first return value is a SPRITESHEET-TREE-NODE that represents the root of
the tree, under which additional subtrees are built.

The secondary return value is a closure that accepts a leaf node of type
SPRITE-DESCRIPTION; based on the REVERSE-PATH slot value of this node, the
closure attach the leaf node existing subtrees under ROOT, creating if
necessary intermediate nodes."
  (let ((root (make-instance 'spritesheet-tree-node)))
    (labels
        ((add-child (node child)
           (prog1 child
             (vector-push-extend child (children node))))
         (make-node (name parent)
           (make-instance
            'spritesheet-tree-node
            :name name
            :parent parent))
         (collect-nodes (path parent leaf)
           (etypecase path
             (null
              (assert (not (find-child-by-name parent (name leaf))))
              (add-child parent leaf)
              (setf (parent leaf) parent))
             (cons
              (destructuring-bind (head . tail) path
                (collect-nodes
                 tail
                 (or (find-child-by-name parent head)
                     (add-child parent (make-node head parent)))
                 leaf))))))
      (values
       root
       (lambda (leaf)
         (collect-nodes (reverse (reverse-path leaf))
                        root
                        leaf))))))

(define-condition no-such-bound-in-env (error)
  ((bound :initarg :bound :reader no-such-bound-in-env/bound)
   (env :initarg :env :reader no-such-bound-in-env/env)))

(defun map-tile-indices-in-order (order env callback &aux (index -1))
  "Iterate over the coordinates of all tiles in a sprite-description.

Iteration is done in the order specified by ORDER. For example, a value of
'(:ROW :COL) represents a row-major order, implying that tiles are visited with
an outer loop over rows and an inner loop over columns.

Each loop is bounded by the value associated with each coordinate in ENV. For
example, the value associated with the keys :ROW and :COL in an environment
indicate the set of values to iterate over, for each coordinate.

In other words, MAP-TILE-INDICES-IN-ORDER iterates over the ordered set product
of the sets associated in ENV for each coordinate named in ORDER.

A bound associated with a corrdinate can be:

- a single number
- a list of numbers
- a compound form (:RANGE X Y), which represents the sequence from X to Y; X can
  be greater than Y, in which case iteration goes from X down to Y; both bounds
  are inclusive; if they are equal, the value is considered only once.

The CALLBACK function is called for each element of the ordered set product, as
keyword arguments. An additional :INDEX argument is given, which is a counter
starting from zero and increasing after each call to CALLBACK.

The call to CALLBACK is made with :ALLOW-OTHER-KEYS bound to T. Consequently,
valid lambda argument lists for CALLBACK need not name all coordinates:
both (&KEY INDEX ROW COL) and (&KEY ROW COL) are valid when ORDER is
'(:ROW :COL)."
  
  (declare (optimize (debug 3)))
  (when (member :index order)
    (error ":INDEX is a reserverd name and cannot be used as a coordinate."))
  (labels
      ((recurse (list indices)
         (etypecase list
           (null (apply callback
                        :index (incf index)
                        :allow-other-keys t
                        (mapcan #'list order (reverse indices))))
           (cons
            (destructuring-bind (head . tail) list
              (let ((bound (resolve-value env head)))
                (unless bound
                  (error 'no-such-bound-in-env :bound bound :env env))
                (labels
                    ((process-bound (bound)
                       (typecase bound
                         (atom (recurse tail (cons bound indices)))
                         (cons
                          (destructuring-bind (kind . args) bound
                            (etypecase kind
                              (integer (map nil #'process-bound bound))
                              (symbol
                               (case kind
                                 (:range
                                  (destructuring-bind (from to) args
                                    (if (= to from)
                                        (process-bound to)
                                        (let ((delta (signum (- to from))))
                                          (loop
                                            (process-bound from)
                                            (when (= from to)
                                              (return))
                                            (incf from delta))))))
                                 (t (recurse tail (cons bound indices)))))))))))
                  (process-bound bound))))))))
    (recurse order ())))

;; (progn (terpri)
;;        (map-tile-indices-in-order '(:x :y :z)
;;                                   '(:x (:range 3 1) :y (:range 3 5) :z (:range 2 3))
;;                                   (lambda (&key x y z)
;;                                     (format t "~&x: ~a y: ~a z: ~a~%" x y z))))

(defun xywh-vector-from-env (&key row col env transform)
  (multiple-value-call #'vector
    (pixel-rectangle col
                     row
                     (resolve-value env :width 1)
                     (resolve-value env :height 1)
                     transform)))

(defun xy-xy-vector-from-env (&key row col env transform)
  (multiple-value-bind (x y w h)
    (pixel-rectangle col
                     row
                     (resolve-value env :width 1)
                     (resolve-value env :height 1)
                     transform)
    (values
     (vector x y)
     (vector (+ x w) (+ y h)))))

(defvar *spritesheet-cleanup* t
  "Automatically unintern obsolete symbols after spritesheet redefinition.")

(defun ensure-sprite-descrption-instance (class &rest initargs &key name &allow-other-keys)
  (let* ((symbol (and *sprite-package* (find-symbol (string name) *sprite-package*)))
         (value (and symbol (boundp symbol) (symbol-value symbol))))
    (typecase value
      (sprite-description
       (apply #'change-class value class initargs))
      (t (apply #'make-instance class initargs)))))

(defgeneric finalize-all-sprite-descriptions (spritesheet descriptions)
  (:method (spritesheet descriptions)))

(deftype bound () `(or cons number))

(defun check-minimal-environment (spritesheet-name path environment)
  (flet
      ((check (attribute type example &aux (value (resolve-value environment attribute)))
         (unless (typep value type)
           (error
            "Spritesheet ~a: expected attribute ~s of type ~A (e.g. ~a)).~
             ~&The current value is ~S of type ~a.
             ~&Error detected for leaf node at path: ~{~<~%~:;~s~>~^ > ~}~
             ~&Environment is:~%~%(~{~s ~a~^~% ~})"
            spritesheet-name
            attribute
            type
            example
            value
            (type-of value)            
            (reverse path)
            environment))))
    (check :file 'pathname "#P\"tileset.png\"")
    (check :order 'cons "(:ROW :COL")
    (loop for key in (resolve-value environment :order)
          do (check key 'bound "3, or (:RANGE 1 5)"))))

(defun compile-spritesheet-tree
    (&key
       spritesheet-name
       attribute-reducers
       tree
       symbol-format
       spritesheet-class
     &aux
       (replacep (get spritesheet-name 'spritesheet))
       (now (get-internal-run-time)))
  (multiple-value-bind (root tree-builder) (make-spritesheet-tree-builder)
    (let ((collection nil)
          (file-indexer (make-indexer :key #'namestring))
          (counter -1)
          (names (make-hash-table :test #'eq))
          (spritesheet (or (let ((existing (get spritesheet-name
                                                'spritesheet)))
                             (and existing
                                  (change-class existing spritesheet-class)))
                           (make-spritesheet :name spritesheet-name
                                             :class spritesheet-class))))
      (setf (spritesheet-source spritesheet) tree)
      (with-hash-consing (hash :test #'equal)
        (handler-bind ((error
                         (lambda (condition)
                           (error "Error in spritesheet ~a.~%~a"
                                  spritesheet-name
                                  condition))))
          (do-property-leaves ((path &rest env) tree :reducers attribute-reducers)
            ;; data-sharing across all environments
            (loop
              with plist
              for (key . value)
                in (sort (plist-alist env)
                         #'string<
                         :key (compose #'string #'car))
              do (setf plist (hash
                              (cons key
                                    (hash
                                     (cons value
                                           (hash plist))))))
              finally (setf env plist))

            (let* ((sequence)
                   (name (with-standard-io-syntax
                           (format nil symbol-format (reverse path))))
                   (transform (resolve-value env
                                             :transform (identity-transform))))

              (check-minimal-environment spritesheet-name path env)
              
              ;; Intern and export sprite name
              (when *sprite-package*
                (setf name (intern name *sprite-package*))
                (export name *sprite-package*))

              ;; Checking for duplicates inside a tree
              (let ((path (gethash name names)))
                (if path
                    (error "Duplicate absolute paths in tree: ~A in ~S"
                           name
                           (cdr path))
                    (setf (gethash name names) (cons t path))))

              ;; For each leaf, we iterate over its tiles and compute their region
              ;; in pixel coordinates. Environment's :tile-callback is used to
              ;; build the concrete rectangle instance (can return nil).

              (let ((pixel-function (resolve-value env
                                                   :tile-callback
                                                   #'xywh-vector-from-env)))
                (map-tile-indices-in-order
                 (resolve-value env :order)
                 env
                 (lambda (&rest tile-indices)
                   (push (apply pixel-function
                                :transform transform
                                :env env
                                :allow-other-keys t
                                tile-indices)
                         sequence))))
              (let ((sd (ensure-sprite-descrption-instance
                         (resolve-value env
                                        :sprite-description-class
                                        'sprite-description)
                         :name name
                         :id (incf counter)
                         :spritesheet spritesheet
                         :env env
                         :reverse-path path
                         :texture-id (funcall file-indexer
                                              (namestring
                                               (resolve-value env
                                                              :file)))
                         :rectangles (coerce
                                      (nreverse sequence)
                                      'vector)
                         :timestamp now)))
                (funcall tree-builder sd)
                (push sd collection))))))

      ;; the tree is built, this is the last chance to perform a
      ;; validation/transformation step for all sprite-description instances
      ;; before committing changes.
      (finalize-all-sprite-descriptions spritesheet collection)
      
      ;; Compute ranges for all nodes
      (labels ((reducer (r1 r2)
                 (destructuring-bind (min1 . max1) r1
                   (destructuring-bind (min2 . max2) r2
                     (cons (min min1 min2) (max max1 max2)))))
               (walk-set-ranges (node)
                 (let ((children (children node)))
                   (case (length children)
                     (0 (range node))
                     (t (setf (range node)
                              (reduce #'reducer (map 'simple-vector
                                                     #'walk-set-ranges
                                                     children))))))))
        (walk-set-ranges root))

      ;; We collected all sprite descriptions. Now, perform a cleanup, if
      ;; necessary, of the previous definition of this spritesheet, and return
      ;; the spritesheet object

      (let ((sprites (coerce (nreverse collection) 'vector))
            (texture-files (funcall file-indexer)))

        ;; Everything went fine until here, we can "commit" changes. Unexpected
        ;; errors in the code below might result in an inconsistent state.

        (when replacep
          (let ((symbols-to-remove))
            (map nil
                 (lambda (s)
                   (let ((name (name s)))
                     (unless (gethash name names)
                       (when (symbolp name)
                         (push name symbols-to-remove)))))
                 (spritesheet-sprites spritesheet))
            (when symbols-to-remove
              (tagbody
                 (if *spritesheet-cleanup*
                     (go remove)
                     (restart-case
                         (error "Those symbols were previously defined ~
                         by the spritesheet: ~& ~S ~& ~% See also ~A."
                                symbols-to-remove
                                '*spritesheet-cleanup*)
                       (ignore ()
                         :report "Leave them."
                         (go end))
                       (unintern ()
                         :report "Unintern them."
                         (go remove))))
               remove
                 (mapc #'unintern symbols-to-remove)
               end))))

        ;; Globally bind symbols to their new sprite descriptions.
        ;; Fill and return the spritesheet.

        (when *sprite-package*
          (map nil
               (lambda (s &aux (sym (name s)))
                 (proclaim `(special ,sym))
                 (setf (symbol-value sym) s))
               sprites))

        (setf (spritesheet-tree spritesheet) root
              (spritesheet-sprites spritesheet) sprites
              (spritesheet-texture-files spritesheet) texture-files)

        spritesheet))))

;; TODO: compile-time information, to know the parent/child relationships (but
;; no SDLRect allocation) and compile relative motion in the tree directly (when
;; safety is lower than speed).

;; TODO: allow to have a minimal subclass of sprite-description, or a struct, so
;; that not all debug informations are kept.

(defun find-spritesheet (designator)
  (etypecase designator
    (symbol (or (get designator 'spritesheet)
                (error "Spritesheet not found: ~A" designator)))
    (spritesheet designator)))

(defun (setf find-spritesheet) (new name)
  (check-type name symbol)
  (check-type new spritesheet)
  (setf (get name 'spritesheet) new))

(defvar *sprite-package*)
(defvar *sprite-name-format* "~{~A~^-~}")

(defmacro define-spritesheet
    (name (&key
             (package '*package*)
             (class ''spritesheet)
             (format *sprite-name-format*))
     &body options-and-tree)
  (let* ((options (butlast options-and-tree))
         (tree (last options-and-tree)))
    (once-only (package)
      `(setf (find-spritesheet ',name)
             (let ((*sprite-package*
                     (and ,package
                          (or (find-package ,package)
                              (error "Package not found: ~a" ,package)))))
               (compile-spritesheet-tree
                :spritesheet-name ',name
                :spritesheet-class ,class
                :attribute-reducers
                (list ,@(loop for (attribute function) in options
                              collect `(quote ,attribute)
                              collect function))
                :tree (list nil nil ,@tree)
                :symbol-format ,format))))))

(defgeneric flip-p (sprite-designator)
  (:method ((sd sprite-description))
    (ensure-list (resolve-value (environment sd) :flip))))

(defun next-index (index sprite-description)
  (mod (1+ index) (length (rectangles sprite-description))))
