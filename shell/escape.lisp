(in-package :bricabrac.shell)

(defun sh-escape (args)
  (format nil
          "~{~a~^ ~}"
          (mapcar (lambda (a)
                    (shellwords:escape
                     (princ-to-string a)))
                  args)))

