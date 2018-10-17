(defpackage :bricabrac.sdl2.sprites.spritesheets
  (:use
   :cl
   :alexandria
   :bricabrac.mixins
   :bricabrac.environmenst)
  (:export))

(in-package :bricabrac.sdl2.sprites.spritesheets)

(defstruct spritesheet
  name          ; symbol
  tree          ; The tree from which we compile the spritesheet
  sprites       ; A vector of SPRITE-DESCRIPTION instances, ordered
                ; according to their ID slot.
  texture-files ; A vector of all unique tileset pathnames declared in
                ; the tree through the :FILE property. Later, we bind
                ; SDL2 textures to each file in the same order (see
                ; TEXTURE-ID below)
  )

;; TODO

;; spritesheet tree : tree of internal nodes (spritesheet-tree-node) with
;; sprite-description objects as leaves.

;; spritesheet sexp : external representation, parsed according to
;; map-spritesheet-sexp-leaves

(defclass spritesheet-tree-node (has-name
                                 has-parent
                                 has-children)
  ())

(defclass sprite-description (spritesheet-tree-node)
  ((spritesheet
    :initarg :spritesheet
    :accessor sprite-description-spritesheet)
   (id
    :initarg :id
    :accessor sprite-description-id )
   (rectangles
    :initarg :rectangles
    :accessor sprite-description-rectangles)
   (flip
    :initarg :flip
    :accessor sprite-description-flip) ;; TODO: into env?
   (reverse-path
    :initarg :reverse-path
    :accessor sprite-description-reverse-path)
   (env
    :initarg :env
    :accessor sprite-description-env)
   (texture-id
    :initarg :texture-id
    :accessor sprite-description-texture-id)))

(defun free-sprite-description-rectangles (sprite-description)
  (map nil
       #'sdl2:free-rect
       (sprite-description-rectangles sprite-description)))

(defun find-child-node (root name)
  (find name (children root) :key #'name))

(defun follow-path-from-root (root path)
  (if (endp path)
      root
      (destructuring-bind (child . path) path
        (follow-path-from-root (or (find-child-node root child)
                                   root)
                               path))))

(defun follow-path-to-leaf (root path)
  (let ((node (follow-path-from-root root path)))
    (etypecase node
      (sprite-description node)
      (spritesheet-tree-node
       (let ((unique-child (ignore-errors (aref (children node) 0))))
         (typecase unique-child
           (spritesheet-tree-node nil)
           (sprite-description unique-child)))))))

(defun find-branch (root branch-path)
  (destructuring-bind (branch . down-path) (ensure-list branch-path)
    (labels
        ((up-and-down (node stack)
           (assert node () "Branch ~s not found from ~s" branch root)
           (let ((child (find-child-node node branch)))
             (if child
                 (or (follow-path-to-leaf
                      child
                      (remove nil (list* (first
                                          stack)
                                         (append
                                          down-path (rest
                                                     stack)))))
                     root)
                 (up (parent node) (list* (name node) stack))))))
      (up (parent root)))))

(defun make-spritesheet-tree-builder ()
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
              (assert (not (find-child-node parent (name leaf))))
              (add-child parent leaf)
              (setf (parent leaf) parent))
             (cons
              (destructuring-bind (head . tail) path
                (collect-nodes
                 tail
                 (or (find-child-node parent head)
                     (add-child parent (make-node head parent)))
                 leaf))))))
      (values
       root
       (lambda (leaf)
         (collect-nodes (reverse (sprite-description-reverse-path leaf))
                        root
                        leaf))))))

(define-condition no-such-bound-in-env (error)
  ((bound :initarg :bound :reader no-such-bound-in-env/bound)
   (env :initarg :env :reader no-such-bound-in-env/env)))

(defun map-tile-indices-in-order (order env callback &aux (index -1))
  "Iterate over the indices of ROWS and COLUMNS in a tileset.

Iteration is done in the order specified by ORDER (e.g. '(:ROW :COL) for an
outer loop over rows and an inner loop for columns), and with the bounds found
inside ENV for :ROW and :COL.

Each bound can be:

- a single number
- a list of bounds
- a compound form (:RANGE X Y), which represents the sequence from X to Y; X can
  be greater than Y, in which case iteration goes from X down to Y; both bounds
  are inclusives; if they are equal, the value is considered only
  once.

The CALLBACK function is called for each combination of row and column.  It is
called with 3 keyword arguments, :ROW, :COL and :INDEX, but does not need to use
them all because it is called with :ALLOW-OTHER-KEYS T.

:ROW and :COL are the current row and columns values. :INDEX is a counter which
increases after each call to the callback, and starts from zero. It represents
the current index of a tile in the sequence of tiles being visited."
  (declare (optimize (debug 3)))
  (labels
      ((recurse (list indices)
         (etypecase list
           (null (apply callback
                        :index (incf index)
                        :allow-other-keys t
                        (mapcan #'list order (reverse indices))))
           (cons
            (destructuring-bind (head . tail) list
              (let ((bound (getf env head)))
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


