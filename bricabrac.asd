(defsystem #:bricabrac
  :depends-on (#:sdl2 #:alexandria)
  :serial t
  :components ((:module #:sld2.event-loop
                :pathname "sdl2/event-loop/"
                :components
                ((:module #:source
                  :pathname "source/"
                  :serial t
                  :components ((:file "package")
                               (:file "macros")
                               (:file "events")
                               (:file "definitions")))
                 (:module #:tests
                  :pathname "tests/"
                  :components ((:file "tests")))))))

(defsystem #:bricabrac/sdl2.event-loop.demo
  :depends-on (#:bricabrac #:cl-opengl)
  :components ((:module #:demo
                :pathname "sdl2/event-loop/demo/"
                :serial t
                :components ((:file "time")
                             (:file "demo")))))
