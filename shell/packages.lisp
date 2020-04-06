(defpackage :bricabrac.shell
  (:use :alexandria :cl #:bricabrac.environments)
  (:export #:terminal
           #:option
           #:with-terminal-options
           #:call-within-temporary-directory
           #:within-temporary-directory
           #:*tmpdir-name*
           #:sh-escape))

