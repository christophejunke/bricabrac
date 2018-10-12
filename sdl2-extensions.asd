(defsystem #:sdl2-extensions
  :depends-on (#:sdl2 #:alexandria)
  :serial t
  :components ((:module #:source
                :pathname "source/"
                :serial t
                :components ((:file "package")
                             (:file "macros")
                             (:file "events")
                             (:file "definitions")))
               (:module #:tests
                :pathname "tests/"
                :components ((:file "tests")))))

(defsystem #:sdl2-extensions/demo
  :depends-on (#:sdl2-extensions #:cl-opengl)
  :components ((:module #:demo
                :pathname "demo/"
                :serial t
                :components ((:file "time")
                             (:file "demo")))))
