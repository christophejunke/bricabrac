(in-package #:bricabrac.local-keywords.private)

(defmacro define-keyword (symbol &optional (doc nil doc-p))
  `(prog1 ',symbol
     (defvar ,symbol ',symbol
       ,(if doc-p
            doc
            (format nil
                    "Self-evaluating symbol ~s"
                    symbol)))
     (setf (get ',symbol 'local-keyword) t)))

(defun local-keyword-p (symbol)
  (get symbol 'local-keyword))

(defun all-local-keywords (&optional (package *package*) list)
  (let ((p (find-package package)))
    (do-symbols (s p list)
      (when (eq (symbol-package s) p)
        (when (local-keyword-p s)
          (push s list))))))
