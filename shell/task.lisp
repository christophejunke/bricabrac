(in-package :bricabrac.task)

(defun %ite (if then &optional else)
  (case if
    ((nil) else)
    ((t) then)
    (t `(if ,if ,then ,else))))

(defun %body-single-form (body)
  (if (rest body)
      `(progn ,@body)
      (first body)))

(defun expand-terminal-bindings (env binding)
  (destructuring-bind (key
                       value
                       &rest all
                       &key
                       ((:when    w) t   wp)
                       ((:unless  u) nil up)
                       ((:default d) nil dp))
      binding
    (labels ((wrap-guards (body)
               (symbol-macrolet ((.body (%body-single-form body)))
                 (cond
                   ((and wp up)
                    (if (< (position :when all)
                           (position :unless all))
                        (list (%ite w (%ite u nil .body)))
                        (list (%ite u nil (%ite w .body)))))
                   (wp (list (%ite w .body)))
                   (up (list (%ite u nil .body)))
                   (t body))))
             (wrap-default (body)
               (if dp (list (%ite d `(unless (optionp ,key) ,@body))) body)))
      (wrap-default
       (wrap-guards
        `((augmentf ,env ,key ,value)))))))

(defun expand-with-terminal-options (bindings body)
  (alexandria:with-gensyms (e)
    `(let (,e)
       ,@(loop for b in bindings append (expand-terminal-bindings e b))
       (let ((*options* (combine-environments *options* ,e (option .reducers))))
         ,@body))))

(expand-with-terminal-options '((:a 3 :when t) (:b 10 :default t))
                              '((list (option :a) (option :b))))


(defmacro with-terminal-options ((&rest bindings) &body body)
  (expand-with-terminal-options bindings body))

(with-terminal-options ((:a 30 :when a :unless o)
                        (:b 10 :unless b :when p))
  (vector (option a)
          (option b)))
