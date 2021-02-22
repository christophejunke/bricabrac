(in-package :bricabrac.sdl2.event-loop)

(defmacro with-captured-bindings ((rebinding-name &rest symbols) &body body)
  "Capture dynamic bindings of SYMBOLS and reestablish them at REBINDING-NAME.

REBINDING-NAME is a symbol that is MACROLET-bound in BODY, such
that (REBINDING-NAME . INNER-BODY) is expanded as LET bindings of
SYMBOLS over INNER-BODY.

SYMBOLS is a literal list of special variables. They are evaluated in
sequence and their current respective values are stored. When the
REBINDING-NAME form is evaluated, the special variables are
dynamically bound to their respective saved values.

This is useful to capture bindings in a dynamic environment and
reestablish them in another dynamic environment, e.g. in a
closure (possibly in another thread).

  (with-captured-bindings (with-cb *standard-output* *debug-io*)
    (lambda ()
      (with-cb
        (print \"prints to the captured binding of *standard-output*\"))))
"
  (assert (every #'symbolp symbols))
  (with-gensyms (inner-body)
    (if symbols
        (loop for s in symbols
              for c = (gensym)
              collect (list c s) into capture
              collect (list s c) into rebind
              finally
                 (return
                   `(let ,capture
                      (macrolet ((,rebinding-name (&body ,inner-body)
                                   `(let ,',rebind ,@,inner-body)))
                        ,@body))))
        `(macrolet ((,rebinding-name (&body ,inner-body)
                      `(progn ,@,inner-body)))
           ,@body))))
