(defsystem :bricabrac
  :depends-on (#:sdl2 #:alexandria)
  :serial t
  :components

  ((:module #:MIXINS
    :pathname "mixins/"
    :components
    ((:file "mixins")))

   ;; (:module #:DEBUG
   ;;  :pathname "debug/"
   ;;  :components ((:file "debug")))

   (:module #:ENVIRONMENTS
    :pathname "environments/"
    :components
    ((:file "package")
     (:file "environments" :depends-on ("package"))
     (:file "indexer" :depends-on ("package"))
     (:file "property-trees" :depends-on ("environments"))))

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
                     (:file "events")
                     (:file "definitions")))
       (:module #:TESTS
        :pathname "tests/"
        :components ((:file "tests")))))

     (:module #:SPRITES
      :pathname "sprites/"
      :components ((:file "spritesheets")))))))

(defsystem :bricabrac/sdl2.event-loop.demo
  :depends-on (#:bricabrac #:cl-opengl)
  :components ((:module #:demo
                :pathname "sdl2/event-loop/demo/"
                :serial t
                :components ((:file "time")
                             (:file "demo"))))
  :perform (test-op (o c) (symbol-call :bricabrac.sdl2.demo :demo)))
