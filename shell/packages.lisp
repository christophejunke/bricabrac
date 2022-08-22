
(defpackage :bricabrac.shell
  (:use :alexandria :cl #:bricabrac.environments
        #:bricabrac.utils)
  (:export #:terminal
           #:terminal%
           #:run-program-wrapper
           #:execute
           #:execute%
           #:*options*
           #:*standard-options*
           #:.environment
           #:.screen
           #:.reducers
           #:query-color
           #:x11
           #:exactly
           #:rgb
           #:option
           #:with-terminal-options
           #:with-standard-terminal-options
           #:wto
           #:term
           #:call-within-temporary-directory
           #:within-temporary-directory
           #:*tmpdir-name*
           #:sh-escape))

(defpackage :bricabrac.task
  (:use :alexandria :cl #:bricabrac.environments
        #:bricabrac.utils))

