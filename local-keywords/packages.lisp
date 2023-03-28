(defpackage #:bricabrac.local-keywords
  (:use)
  (:export #:define-local-keyword
           #:local-keyword
           #:local-keyword-p
           #:all-local-keywords))

(defpackage #:bricabrac.local-keywords.private
  (:use :cl
        #:bricabrac.local-keywords))
