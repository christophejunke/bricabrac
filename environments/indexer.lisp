(in-package :bricabrac.environments)

(defun make-indexer (&key (key #'identity) (test #'eql) &aux (i 0))
  "Create a numerical index for a set of values, without duplicates.

Based on the KEY and TEST auxiliary functions (where TEST is a suitable
test for hash-tables), the function builds a closure that accepts an
optional argument DATA.

Each time DATA is given that was not yet already given to this closure,
w.r.t. to KEY and TEST, increment a counter and affect it to the
value. If however the value was already present, return the existing
index.

Finally, when no argument is given, the closure returns a vector of all
DATA values already seen by the closure, stored in the same order as the
index given to each. Note that those values are the one that were being
passed to the function, not the keys (which are used only for hashing).
"
  (let ((map (make-hash-table :test test)))
    (lambda (&optional (data nil datap))
      (if datap
	  (first (ensure-gethash
		  (funcall key data) map 
		  (list (shiftf i (1+ i)) data)))
	  (let ((result (make-array i :adjustable ())))
	    (prog1 result
	      (flet ((add (v) (setf (aref result (first v)) (second v))))
		(maphash-values #'add map))))))))

(defun test-indexer ()
  (flet ((str (x) (typecase x (pathname (namestring x)) (t (string x)))))
    (let ((indexer (make-indexer :key #'str :test #'equal)))
      (flet ((index () (funcall indexer)) (add (v) (funcall indexer v)))
	(values (list (add "/tmp/foo.lisp")
		      (add nil)
		      (add "/tmp/bar.lisp")
		      (add "/tmp/bar.lisp")
		      (add nil))
		(index))))))
