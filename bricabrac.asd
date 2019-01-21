(defsystem #:bricabrac
  :depends-on (#:alexandria
               #:bordeaux-threads
               #:osicat
               #:external-program
               #:sdl2
               #:sdl2-image
               #:optima)
  :serial t
  :components

  ((:module #:MIXINS
    :pathname "mixins/"
    :components
    ((:file "mixins")))

   (:module #:DEBUG
    :pathname "debug/"
    :components ((:file "debug")))

   (:module #:ENVIRONMENTS
    :pathname "environments/"
    :components
    ((:file "package")
     (:file "hash-consing" :depends-on ("package"))
     (:file "environments" :depends-on ("hash-consing"))
     (:file "indexer" :depends-on ("package"))
     (:file "property-trees" :depends-on ("environments"))))

   ;; (:module #:KEYMAPS
   ;;  :pathname "keymaps/"
   ;;  :components
   ;;  ((:file "package")
   ;;   (:file "keymaps" :depends-on ("package"))))

   (:module #:TIME
    :pathname "time/"
    :components
    ((:file "internal-time")))

   (:module #:SDL2
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
                   ))))

   ;; (:module #:OPERATING-SYSTEM
   ;;  :pathname "os/"
   ;;  :components
   ;;  ((:file "pipeline")))
   ))

(defsystem :bricabrac/sdl2.event-loop.demo
  :depends-on (#:bricabrac #:cl-opengl)
  :components ((:module #:demo
                :pathname "sdl2/event-loop/demo/"
                :serial t
                :components ((:file "time")
                             (:file "demo"))))
  :perform (test-op (o c) (symbol-call :bricabrac.sdl2.demo :demo)))
