
(defpackage :bricabrac.shell
  (:use :alexandria :cl #:bricabrac.environments
        #:bricabrac.utils #:pipeline)
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
           #:option*

           #:tmux-sessions
           #:best-tmux-session
           #:with-tmux-session

           #:with-terminal-options
           #:with-standard-terminal-options
           #:with-output-to-script
           #:with-open-task-socket

           #:*from-path*
           #:*socket-file*
           #:*script-permissions*
           #:*script-name*

           #:*script-mappings*
           #:write-script-header
           #:write-script-footer

           #:with-task-client
           #:wto
           #:term
           #:call-within-temporary-directory
           #:within-temporary-directory
           #:*tmpdir-name*
           #:sh-escape))

(defpackage :bricabrac.task
  (:use :alexandria :cl #:bricabrac.environments
        #:bricabrac.utils))

