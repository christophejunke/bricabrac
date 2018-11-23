(in-package :bricabrac.environments)

(defmacro environment-bind ((&rest keys) environment &body body)
  (once-only (environment)
    `(let ,(mapcar (lambda (k)
                     `(,k (resolve-value
                           ,environment
                           ,(make-keyword k))))
            keys)
       ,@body)))

(defgeneric attribute-reducer (attribute old-value new-value)
  (:documentation "Generic function for attribute combinators.

Define global attribute reducers (most likely with EQL methods).")
  (:method (_ old new)
    "Shadow the previous value with the new one."
    (declare (ignore old)) new))

(let ((var (or ())))
  (declare (special var))
  (let ((var (cons 3 var)))
    (declare (special var))
    var)
  var)

(defmacro with-special-var ((var &optional default) &body body)
  `(locally (declare (special ,var))
     (unless (boundp ',var)
       (setf ,var ,default))
     ,@body))

(defmacro let-special ((var &optional value) &body body)
  `(let ((,var ,value))
     (declare (special ,var))
     ,@body))

(defun maybe-resolve% (environment value attribute)
  (with-special-var (seen)
    (block nil
      (typecase value
        (cons (destructuring-bind (head . tail) value
                (case head
                  (ref (destructuring-bind (reference) tail
                         (return
                           (let-special (seen (cons attribute seen))
                             (resolve-value environment reference)))))))))
      value)))

(let ((absent (gensym)))
  (defun resolve-value (environment attribute &optional default)
    (with-special-var (seen)
      (when (member attribute seen)
        (error "Circular dereference of attribute ~S~
              ~%~
              ~% - environment : ~S~
              ~% - cycle       : ~{~S~^ -> ~}"
               attribute
               environment
               (reverse (cons attribute seen))))
      (let ((value (getf environment attribute absent)))
        (when (and seen (eq value absent))
          (error "Reference not found: ~s ~s"
                 attribute
                 (reverse seen)))
        (maybe-resolve% environment
                        (if (eq value absent) default value)
                        attribute)))))

;; (resolve-value '(:a (ref :c) :b (ref :a) :c (ref :b)) :c)
;; (resolve-value '(:a 0 :b (ref :a) :c (ref :b)) :c)

;; (untrace combine-environments)

(defun combine-environments (old-env new-env &optional reducers)
  "Combine property lists w.r.t. reducers.

REDUCERS is a plist of property names to functions.  A reducer function takes
the old value, the new value and returns the combined value. You can install
reducer functions globally by defining a method for ATTRIBUTE-REDUCER (the
REDUCERS list however takes precedence over them). When no reducer is defined
for an attribute, the new value shadows the previous one. For example:

   (combine-environments '(:a 1 :b 2 :c 3)
                         '(:a 0 :b 20 :c 4)
                         (list :a #'list :b #'+))
   => (:A (1 0) :B 22 :C 4)

An attribute can appear multiple times in the environment: if so, its
consecutive values will be combined in the order of appearance.

   (combine-environments `()
                         `(:transform ,(scale 3 3)
                           :transform ,(move 1 1)))

   => (:TRANSFORM #S(TRANSFORM :SX 3 :SY 3 :TX 3 :TY 3))
"
  (loop with env = (copy-seq old-env)
        for (attribute raw-value) on new-env by #'cddr
        for value = (maybe-resolve% env raw-value attribute)
        for old-value = (resolve-value env attribute)
        do (setf (getf env attribute)
                 (if old-value
                     (let ((reducer (getf reducers attribute)))
                       (if reducer
                           (funcall reducer old-value value)
                           (attribute-reducer attribute old-value value)))
                     value))
        finally (return env)))


