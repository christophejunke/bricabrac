(defsystem #:bricabrac
  :depends-on ( 
               #:ALEXANDRIA
               #:BORDEAUX-THREADS
               #:CL-CONTAINERS
               #:CL-SHELLWORDS
               #:DRAKMA
               #:EXTERNAL-PROGRAM
               #:IOLIB
               #:IOLIB/SOCKETS
               #:OPTIMA
               #:OSICAT
               #:PIPELINE
               )

  :serial t
  :components

  ((:module #:UTILS
            :pathname "utils/"
            :components
            ((:file "utils")))

   (:module #:MIXINS
            :pathname "mixins/"
            :components
            ((:file "mixins")))

   (:module #:CODE
            :pathname "code/"
            :components
            ((:file "cee")))

   (:module #:DEBUG
            :pathname "debug/"
            :components ((:file "debug")))

   ;; (:module #:KEYMAPS
   ;;  :pathname "keymaps/"
   ;;  :components
   ;;  ((:file "package")
   ;;   (:file "keymaps" :depends-on ("package"))))

   (:module #:TIME
            :pathname "time/"
            :components
            ((:file "internal-time")))

   ;; (:module #:OPERATING-SYSTEM
   ;;  :pathname "os/"
   ;;  :components
   ;;  ((:file "pipeline")))
   ))

(defsystem :bricabrac/environments
  :pathname "environments/"
  :components
  ((:file "package")
   (:file "hash-consing" :depends-on ("package"))
   (:file "environments" :depends-on ("hash-consing"))
   (:file "indexer" :depends-on ("package"))
   (:file "property-trees" :depends-on ("environments"))))

(defsystem :bricabrac/property-trees
  :pathname "property-trees"
  :depends-on ("alexandria"
               "bricabrac/fold-environments")
  :serial t
  :components ((:file "packages")
               (:file "main")))

(defsystem :bricabrac/local-keywords
  :pathname "local-keywords"
  :depends-on ()
  :serial t
  :components ((:file "packages")
               (:file "main")))

(defsystem :bricabrac/shell
  :pathname "shell"
  :depends-on (:bricabrac/local-keywords
               :bricabrac/environments
               :foldenv)
  :serial t
  :components ((:file "packages")
               (:file "escape")
               (:file "terminal")
               (:file "directory")))

;; DEPRECATED => SEE FOLDENV
(defsystem :bricabrac/fold-environments
  :depends-on (:bricabrac/local-keywords)
  :pathname "fold-environments"
  :serial t
  :components ((:file "packages")
               (:file "environment")
               (:file "modes")
               (:file "main")))

(defsystem :bricabrac/sdl2
  :depends-on (#:SDL2 #:SDL2-IMAGE)
  :components 
  ((:module #:SDL2
            :pathname "sdl2/"
            :components
            ((:module #:EVENT-LOOP
                      :pathname "event-loop/"
                      :components
                      ((:module #:SOURCE
                                :pathname "source/"
                                :serial t
                                :components ((:file "package")
                                             (:file "macros")
                                             (:file "ffi")
                                             (:file "events")
                                             (:file "definitions")))
                       (:module #:TESTS
                                :pathname "tests/"
                                :components ((:file "tests")))))

             (:module #:SPRITES
                      :pathname "sprites/"
                      :serial t
                      :components ((:file "packages")
                                   (:file "transform")
                                   (:file "spritesheets")
                                   (:file "textures")

                                   ;; move to test system?
                                   ;; (:file "test")
                                   ))))))

(defsystem :bricabrac/sdl2.event-loop.demo
  :depends-on (#:bricabrac #:cl-opengl)
  :components ((:module #:demo
                :pathname "sdl2/event-loop/demo/"
                :serial t
                :components ((:file "time")
                             (:file "demo"))))
  :perform (test-op (o c) (symbol-call :bricabrac.sdl2.demo :demo)))
