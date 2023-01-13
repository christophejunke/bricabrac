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
      (prog1 value
        (typecase value
          (cons
           (destructuring-bind (head . tail) value
             (case head
               (ref
                (destructuring-bind (reference) tail
                  (return
                    (let-special (seen (cons attribute seen))
                      (resolve-value environment
                                     reference)))))))))))))

(defgeneric eget-for-list (environment-type environment attribute)
  (:method ((_ (eql :plist)) environment attribute)
    (let* ((not-found '#.(gensym "NOT FOUND"))
           (entry (getf environment attribute not-found)))
      (values entry (not (eq entry not-found)))))
  (:method ((_ (eql :alist)) environment attribute)
    (let ((entry (assoc attribute environment)))
      (values (cdr entry) entry))))

(defgeneric eget (environment attribute)
  (:method ((environment list) attribute)
    (eget-for-list *environment-type* environment attribute)))

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
    (multiple-value-bind (value found) (eget environment attribute)
      (when (and seen (not found))
        (error "reference not found: ~s in ~s" attribute environment))
      (maybe-resolve% environment (if found value default) attribute))))


(defgeneric env+/list (environment-type environment attribute value)
  (:method ((_ (eql :plist)) env a v)
    (setf (getf env a) v)
    env)
  (:method ((_ (eql :alist)) e a v)
    (if-let ((entry (assoc a e)))
      (prog1 e (setf (cdr entry) v))
      (acons a v e))))

(defgeneric env+ (environment attribute value)
  (:method ((e list) a v)
    (env+/list *environment-type* e a v)))

;; (resolve-value '(:a (ref :c) :b (ref :a) :c (ref :b)) :c)
;; (resolve-value '(:a 0 :b (ref :a) :c (ref :b)) :c)
;; (untrace combine-environments)

(defvar *reducer* nil)
(defvar *env*)

(defvar *environment-type* :plist)

(defgeneric as-alist (e)
  (:method ((e list))
    (ecase *environment-type*
      (:alist e)
      (:plist (alexandria:plist-alist e)))))

(defun combine-environments (old-env new-env &optional reducers)
  "Combine property lists according to reducers.

Bind *ENV* to OLD-ENV and combine OLD-ENV and NEW-ENV using REDUCERS.
REDUCERS is either a plist of property names to reducer functions, or
a reducer function that is used for all properties.

A reducer function takes the old value, the new value and returns the
combined value. When an attribute is missing from REDUCERS, the
default *REDUCER* is tried. If all fails, the generic function
ATTRIBUTE-REDUCER is called. By default, *REDUCER* is NIL and the
default method for ATTRIBUTE-REDUCER is used, which simply let the new
value shadows the previous one. For example:


   (combine-environments '(:a 1 :b 2 :c 3)
                         '(:a 0 :b 20 :c 4)
                         (list :a #'list :b #'+))
   => (:A (1 0) :B 22 :C 4)

Above, the value for keyword :A is a list of its old value and its new value,
as given by #'list. The value for keyword :B is the sum of both values. For
keyword :C, lack of a reducer means the new value is used without considering
the previous one.

An attribute can appear multiple times in the environment: if so, its
consecutive values will be combined in the order of appearance.

   (combine-environments `()
                         `(:transform ,(scale 3 3)
                           :transform ,(move 1 1)))

   => (:TRANSFORM #S(TRANSFORM :SX 3 :SY 3 :TX 3 :TY 3))

:TRANSFORM holds a value that represents 2D transform, but only scaling or
translation. The SCALE function builds a function that scales an existing
transform. The MOVE function builds a function that translates an existing
transform. 

Here above, the resulting transform is obtained by first scaling by 3 on both
axes, then translating by 1 units in both directions. Since the scale was
changed before applying the transform, the combined transform is equivalent to
a transform that scales by 3 from an origin translated by 3 original units."

  (loop for env = (copy-tree old-env) then next-env
        for (attribute . raw-value) in (as-alist new-env)
        for value = (maybe-resolve% env raw-value attribute)
        for old-value = (resolve-value env attribute)
        for next-env = (env+ env
                             attribute
                             (let ((reducer (typecase reducers
                                              (function reducers)
                                              (list (getf reducers
                                                          attribute
                                                          *reducer*)))))
                               (let ((*env* old-env))
                                 (if reducer
                                     (funcall reducer old-value value)
                                     (attribute-reducer attribute old-value value)))))
        finally (return env)))

