(in-package :bricabrac.environmenst)

(defun make-indexer (&key (key #'identity) (test #'eql))
  "Create a numerical index for a set of values, without duplicates.

Based on the KEY and TEST auxiliary functions (where TEST is a suitable test
for hash-tables), the function builds a closure that accepts an optional
argument DATA.

Each time DATA is given that was not yet already given to this closure,
w.r.t. to KEY and TEST, we increment a counter and affect it to the value. If
however the value was already present, we return the existing index.

Finally, when no argument is given, the closure returns a vector of all the
DATA already seen by the closure, stored in the same order as the index which
was associated with the value. Note that those values are the one that were
being passed to the function, not the keys (which are used only for hashing)."
  (let ((map (make-hash-table :test test))
        (counter -1))
    (lambda (&optional (data nil datap))
      (if datap
          ;; Store data
          (let ((key (funcall key data)))
            (cdr
             (or (gethash key map)
                 (setf (gethash key map)
                       (cons data (incf counter))))))
          ;; Return index
          (let ((array (make-array (hash-table-count map) :adjustable nil)))
            (maphash (lambda (k v)
                       (declare (ignore  k))
                       (setf (aref array (cdr v)) (car v))) map)
            array)))))

;; (defun test-indexer ()
;;   (let ((indexer (make-indexer :key #'namestring :test #'equal)))
;;     (list (funcall indexer #P"/tmp/foo.lisp")
;;           (funcall indexer #P"/tmp/bar.lisp")
;;           (funcall indexer))))
