(in-package :bricabrac.environmenst)

(defgeneric attribute-reducer (attribute old-value new-value)
  (:documentation "Generic function for attribute combinators.

Define global attribute reducers (most likely with EQL methods).")
  (:method (_ old new)
    "Shadow the previous value with the new one."
    (declare (ignore old)) new))

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
        for (attribute value) on new-env by #'cddr
        for old-value = (getf env attribute)
        do (setf (getf env attribute)
                 (if old-value
                     (let ((reducer (getf reducers attribute)))
                       (if reducer
                           (funcall reducer old-value value)
                           (attribute-reducer attribute old-value value)))
                     value))
        finally (return env)))

