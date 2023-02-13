(defpackage #:bricabrac.combine-environments.details
  (:documentation "Extension interface")
  (:use)
  (:export

   ;; GENERIC FUNCTIONS
   #:combine-for-key
   #:combine

   ;; ENVIRONMENT
   #:augment
   #:resolve
   #:without

   ;; HELPER
   #:ensure-list-of-environments))

(defpackage #:bricabrac.combine-environments
  (:documentation "User interface")
  (:use #:bricabrac.combine-environments.details)
  (:export

   ;; CURRENT COMBINATOR KNOWLEDGE
   #:*combinator*

   ;; CURRENT ENVIRONMENT
   #:*environment*

   ;; BINDING MACRO
   #:environment-bind

   ;; COMBINE TWO OR MORE ENVIRONMENTS
   #:combine-two-environments
   #:combine-environments

   ;; CREATE COMBINATOR FROM INIT AND FUNCTION
   #:init
   ))

(defpackage #:bricabrac.combine-environments.private
  (:documentation "Other unexported symbols")
  (:use :cl
        #:bricabrac.local-keywords
        #:bricabrac.combine-environments
        #:bricabrac.combine-environments.details))
