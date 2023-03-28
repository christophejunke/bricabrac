(defpackage :test
  (:use :cl
        #:bricabrac.fold-environments.extend
        #:bricabrac.fold-environments.environment
        #:bricabrac.fold-environments
        #:bricabrac.local-keywords))

(in-package :test)

(defun flat-nest (op old new)
  (cond
    ((and (consp old) (eq op (car old)))
     (list* op new (cdr old)))
    (old (list op new old))
    (t new)))

;;

(define-local-keyword .type)

(declaim (inline vec vec-x vec-y))
(defstruct (vec (:type (vector (real -200 200)))
                (:constructor vec (&optional (x 0) (y x))))
  x y)

(vec 1)

(defun fold-type (old new)
  (destructuring-bind (var type) new
    (augment (without old var)
             var
             (flat-nest 'and (resolve var old) type))))

(defun norm-bindings (b)
  (flet ((norm (e)
           (etypecase e
             (symbol (list e nil))
             (cons (list (first e) (second e))))))
    (mapcar #'norm b)))

(defun lang-parse (form)
  (etypecase form
    (null `(:val nil))
    (symbol `(:var ,form))
    (number `(:val ,form))
    ((cons (eql let) t)
     (destructuring-bind (b e) (rest form)
       `(:let ,(norm-bindings b) ,e)))
    (cons
     (destructuring-bind (op . args) form
       `(:op ,op ,args)))))

(defmacro with-lang-match (f &body clauses)
  (alexandria:with-gensyms (k %args)
    (flet ((% (kw arity)
             (let ((code (assoc kw clauses :key #'car)))
               (when code
                 (destructuring-bind (m . e) code
                   (destructuring-bind (_ . a) m
                     (assert (eq _ kw))
                     (assert (= (length a) arity))
                     `((,kw (destructuring-bind ,a ,%args ,@e)))))))))
      `(destructuring-bind (,k . ,%args) (lang-parse ,f)
         (case ,k
           ,@(% :val 1)
           ,@(% :let 2)
           ,@(% :var 1)
           ,@(% :op 2))))))

;; (defmethod bricabrac.fold-environments.extend:generic-fold
;;   ((m function) env key old new)
;;   (or (bricabrac.fold-environments.extend:generic-fold nil env key old new)
;;       (funcall m old new)))

(defun vars (form)
  (let ((*fold-mapping* '(:vars (append #:new #:old)
                          :free (union #:old (list #:new) :test #'equalp)
                          :used (union #:old (list #:new) :test #'equalp)))
        (global))
    (labels ((walk (form scope)
               (with-lang-match form
                 ((:let B E)
                  (loop
                    for (V W) in B
                    do (walk W scope)
                    collect (list V (gensym)) into bindings
                    finally (walk E (fold-environments% scope (list :vars bindings)))))
                 ((:op OP ARGS)
                  (declare (ignore OP))
                  (dolist (A ARGS)
                    (walk A scope)))
                 ((:var V)
                  (let ((w (find V (resolve :vars scope) :key #'car)))
                    (if w
                        (foldf global :used w)
                        (foldf global :free (cons V scope))))))))
      (walk form '())
      global)))

(vars
 '(let ((a 3) (b (* c c)))
   (+ a (let ((b (* b b))) b) f (let ((c 10)) c))))

(with-environment (env nil) (.fold-mapping '((.type fold-type)))
  (with-environment env ((.type '(a integer))
                         (.type '(b float)))
    (with-environment env ((.type '(a fixnum)))
      (resolve 'b (resolve .type env)))))
;;

(defstruct tracker history)

(defmethod generic-fold ((_ tracker) env key old new)
  (let ((fold (generic-fold nil env key old new)))
    (prog1 fold
      (push (list key fold) (tracker-history _)))))
;;

(let ((*fold-mapping* (make-tracker)))
  (with-environment (env nil) (.fold-mapping '((.type fold-type)))
    (with-environment env ((.type '(a integer))
                           (.type '(b float)))
      (with-environment env ((.type '(a fixnum)))
        (list *fold-mapping* (resolve 'b (resolve .type env)))))))

;; (#S(TRACKER
;;     :HISTORY ((.TYPE (A (AND FIXNUM INTEGER) B FLOAT))
;;               (.TYPE (B FLOAT A INTEGER)) (.TYPE (A INTEGER))
;;               (.FOLD-MAPPING (.TYPE FOLD-TYPE)) (.TYPE FOLD-TYPE)))
;;  FLOAT)
