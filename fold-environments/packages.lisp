(defpackage #:bricabrac.fold-environments.utils
  (:documentation "Extension interface")
  (:use)
  (:export

   ;; ENVIRONMENT
   #:augment
   #:resolve
   #:without

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

   ;; CURRENT ENVIRONMENT
   #:*environment*

   ;; BINDING MACRO
   #:environment-bind

   ;; FOLD TWO OR MORE ENVIRONMENTS
   #:fold-environments
   #:fold-environments*

   ))

(defpackage #:bricabrac.fold-environments.private
  (:documentation "Other unexported symbols")
  (:use :cl
        #:bricabrac.local-keywords
        #:bricabrac.fold-environments
        #:bricabrac.fold-environments.utils
        #:bricabrac.fold-environments.extend
        #:bricabrac.fold-environments.modes))
