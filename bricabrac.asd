(defsystem #:bricabrac
  :depends-on (#:sdl2 #:alexandria)
  :serial t
  :components ((:module #:sld2
                :pathname "sdl2/"
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

(defsystem #:bricabrac/sdl2-demo
  :depends-on (#:bricabrac #:cl-opengl)
  :components ((:module #:demo
                :pathname "sdl2/demo/"
                :serial t
                :components ((:file "time")
                             (:file "demo")))))
