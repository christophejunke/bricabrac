(defpackage #:bricabrac.fold-environments.environment
  (:documentation "Environment interface")
  (:use)
  (:export

   ;; ENVIRONMENT
   #:augment
   #:without
   #:resolve

   ;; CURRENT ENVIRONMENT
   #:*environment*

   ;; BINDING MACRO
   #:environment-bind

   ;; LOCAL ENVIRONMENT BINDINGS
   #:with-environment))

(defpackage #:bricabrac.fold-environments.utils
  (:documentation "Extension interface")
  (:use)
  (:export

   ;; CODE MANIPULATION
   #:uninterned=
   #:transform-code
   #:as-function
   
   ;; META MAPPING
   #:find-fold-mapping
   
   ;; HELPER
   #:ensure-list-of-environments))

(defpackage #:bricabrac.fold-environments.modes
  (:documentation "Predefined interpretation modes")
  (:use)
  (:export
   #:.simple
   #:.simple/eval
   #:.eval))

(defpackage #:bricabrac.fold-environments.extend
  (:documentation "Extension interface")
  (:use)
  (:export

   ;; GENERIC FUNCTIONS
   #:interpret-fold-function
   #:fold-for-key
   #:generic-fold))

(defpackage #:bricabrac.fold-environments
  (:documentation "User interface")
  (:use)
  (:export

   ;; CURRENT MAPPING OF FOLD FUNCTIONS
   #:*fold-mapping*
   
   ;; INTERPRET CODE DIRECTLY IN *FOLD-MAPPING* (see doc)
   #:*fold-mapping-interpretation-mode*

   ;; SPECIAL KEY
   #:.fold-mapping
   
   ;; Fold place
   #:foldf

   ;; FOLD TWO OR MORE ENVIRONMENTS
   #:fold-environments
   #:fold-environments%
   #:fold-environments*))

(defpackage #:bricabrac.fold-environments.private
  (:documentation "Other unexported symbols")
  (:use :cl
        #:bricabrac.local-keywords
        #:bricabrac.fold-environments
        #:bricabrac.fold-environments.utils
        #:bricabrac.fold-environments.environment
        #:bricabrac.fold-environments.extend
        #:bricabrac.fold-environments.modes))
