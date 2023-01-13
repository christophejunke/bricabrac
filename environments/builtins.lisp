(in-package :bricabrac.environments.builtins)

(defun fold-pathname (old new)
  (if old (merge-pathnames new old) new))

(defun fold-set-difference (old new)
  (set-difference new old))
