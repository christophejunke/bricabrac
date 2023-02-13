(in-package #:bricabrac.property-trees)

(defun map-property-leaves (root-node callback environment reducers)
  
  )

;; (defmacro do-property-leaves
;;     ((treepath
;;       &key environment result combinator)
;;      tree
;;      &body body)
;;   (destructuring-bind (path &optional environment) (ensure-list path-env)
;;     (unless environment
;;       (setf environment (gensym))
;;       (push `(declare (ignore ,environment)) body))
;;     `(block nil
;;        (map-property-leaves ,tree
;;                             ,combinator
;;                             (lambda (,path ,environment) ,@body))
;;        (return ,result))))

;; (&key environment path)

;; A tree is (NODE ENV-LIST &body CHILDREN)
;; ENV-LIST is a descriptor for a list of environments:
;;
;; NIL
;; An environment e.g. (:A 0 :B 1 ...)
;; A list of environments ((:A 0) (:B 1))
;;
;; This syntax allows to define environments as forms that look like
;; ALISTS if this is preferable.  In particular, automatic code
;; formatting is ugly with PLIST w.r.t. alignment but ok with Lists of
;; Lists.
;;

;; `(_ ((.combine (:a +))
;;      (:a 0)
;;      (:b 1))

;;     )

;; (do-property-tree-leaves (env path) (tree combinator result)
;;   (print path))
