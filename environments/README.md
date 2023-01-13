# EXAMPLE SPECIALIZATIONS

Define a pair of functions:

Or, change how CALL-WITH-CONTEXT works:

    (defmethod call-with-context ((type (eql 'timer)) fn &rest args)
      (destructuring-bind (&optional name) args
        (let* ((timestamp #1=(get-internal-real-time))
               (values (multiple-value-list (funcall fn))))
          (warn "Time: ~f seconds~@[ for timer ~a~]"
                (/ (- #1# timestamp) internal-time-units-per-second)
                name)
          (values-list values))))

    (with (timer :test-timer)
      (loop repeat 50 do (sleep 0.01)))

Or, change how the macro is expanded:

    (defmethod expand-with ((type (eql 'path)) args vars body)
      (assert (not vars))
      (assert args)
      `(let ((*default-pathname-defaults*
               (merge-pathnames
                ,(etypecase args
                   ;; string/pathname/form
                   ((cons (or cons string pathname) null)
                    (first args))
                   ;; make-pathname
                   ((cons keyword cons)
                    `(make-pathname ,@args))))))
         ,@body))

    (with (path
           :directory `(:absolute "proc"
                                  ,(princ-to-string
                                    (osicat-posix:getpid))
                                  "attr"))
      (probe-file "context"))

It is possible to dispatch directly on the class of a literal value:

    (defmethod expand-with ((path pathname) args vars body)
      (assert (not vars))
      (assert (not args))
      `(let ((*default-pathname-defaults* (merge-pathnames ,path)))
         ,@body))

    (with #P"/tmp/
      (probe-file "test.data"))
