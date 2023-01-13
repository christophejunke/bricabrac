(defpackage :tmux (:use :cl))
(in-package :tmux)

(defun tmux-session-program (name programs &key (layout "even-vertical"))
  (reduce (lambda d(program cmd)
            (if cmd
                (list* program cmd))
            )
          programs
          :initial-value nil))
