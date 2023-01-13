(defpackage s4m-demo
  (:use "CL"
        "SDL2"
        "ALEXANDRIA"
        "BRICABRAC.ENVIRONMENTS"
        "BRICABRAC.ENVIRONMENTS.HASH-CONSING"
        "BRICABRAC.ENVIRONMENTS.BUILTINS"
        "BRICABRAC.SDL2.SPRITES.SPRITESHEETS"
        "BRICABRAC.SDL2.SPRITES.TEXTURES"
        "BRICABRAC.SDL2.SPRITES.TRANSFORM"
        "BRICABRAC.SDL2.EVENT-LOOP"
        "BRICABRAC.MIXINS")
  (:export #:run))

(defpackage s4m (:use))
(in-package :s4m-demo)

(defclass character-sprite (sprite-description)
  ((animation :accessor animation)
   (offsets :accessor offsets)))

(define-spritesheet s4m (:package :s4m)
  (:file #'fold-pathname)
  (:flip #'fold-set-difference)
  `(_ (:file #P"/home/chris/1bit/s4m/*.png"
       :default-target-rect (0 0 200 240)
       :tile-callback ,#'sdl-rect-from-env
       :sprite-description-class ,(find-class 'character-sprite)       
       :gravity :south
       :order (:row :col)
       :transform ,(scale 20 24)
       :transform ,(move -1 -1)       
       :col (:range 1 4)
       :row (:range 1 12))
    (hero (:file #p"s4m_ur4i_huge-assetpack-character-animations")
     (dash ()
      (right (:frame 8))
      (left (:frame 9)))
     ((:each
       (item (:frame 10))
       (sleep (:frame (32 33 34 35)))
       (attack (:frame 40))
       (fishing ()
        (wait (:frame (28 29)))
        (pull (:frame 30)))
       (run (:frame (4 4 7 6 5 5 6 7))))
      ()
      left
      (right (:flip (:horizontal)))))))

(define-spritesheet stormhead (:package :stormhead)
  (:file #'fold-pathname)
  `(_ (:file #p"~/1bit/stormhead/*.png"
       :order (:row :col)
       :transform ,(scale 119 124)
       :tile-callback ,#'sdl-rect-from-env
       :sprite-description-class character-sprite 
       :gravity :south
       :col 0
       :row (:range 0 8))
    ((:each 
      (attack (:file "attack"))
      (damaged (:file "damaged"))
      (death (:file "death"))
      (idle (:file "idle"))
      (run (:file "run")))
     ()
     (left (:flip (:horizontal)))
     right)))

(defgeneric handle-event (game event-type event)
  (:method (game type event))
  (:method (game (type (eql :quit)) event)
    (break)))

(defgeneric game-loop (game)
  (:method (game)
    (with-simple-restart (abort "Exit game")
      (do-events (event :event-type type :method :poll)
        (restart-case (handle-event game type event)
          (ignore () :report "Ignore event"))))
    (on-quit game)))

(defclass has-window-renderer-context ()
  ((.window :accessor .window)
   (.renderer :accessor .renderer)))

(defun run (game)
  (with-captured-bindings (rebind-in-sdl-thread
                           *standard-output*
                           *trace-output*
                           *error-output*
                           *default-pathname-defaults*)
    (with-init (:everything)
      (rebind-in-sdl-thread
       (loop
         (restart-case (return (game-loop game))
           (restart-demo-loop ()
             :report "Restart demo")))))))

(defclass abstract-demo (has-window-renderer-context)
  ((test-sprite :initarg :test-sprite :accessor test-sprite)
   (meta :initarg :meta :reader %meta)))

(defclass s4m-demo (abstract-demo)
  ())

(defclass stormhead-demo (abstract-demo)
  ()
  (:default-initargs 
   :meta '(:title "Stormhead" :width 119 :height 124
           :tilesets (stormhead))))

(defun gravity-ratios (gravity)
  (etypecase gravity
    (keyword
     (ecase gravity
       ((:nw :northweast) (values 0 0))
       ((:n :north) (values 1/2 0))
       ((:ne :northeast (values 1 0)))

       ((:w :west) (values 0 1/2))
       ((:c :center) (values 1/2 1/2))
       ((:e :east (values 1 1/2)))
       
       ((:sw :southwest) (values 0 1))
       ((:s :south) (values 1/2 1))
       ((:se :southeast (values 1 1)))))
    (cons (destructuring-bind (rx . ry) gravity
            (values rx ry)))))

(defmethod shared-initialize :after
    ((sprite character-sprite) slot-names &key &allow-other-keys)
  (with-accessors ((rectangles rectangles)) sprite
    (environment-bind (frame gravity) (environment sprite)
      (setf (offsets sprite)
            (multiple-value-bind (xr yr) (gravity-ratios gravity)
              (with-hash-consing (hash :test #'equalp)
                (flet ((offset (w h) (hash (vector (* xr w) (* yr h)))))
                  (map 'simple-vector
                       (lambda (r)
                         (offset
                          (rect-width r)
                          (rect-height r)))
                       rectangles)))))
      (setf (animation sprite)
            (if frame
                (let ((list (loop for f in (ensure-list frame)
                                  collect (aref rectangles f))))
                  (coerce list 'vector))
                rectangles)))))

(defmethod game-loop :around ((game s4m-demo))
  (with-window (w :title "S4M DEMO" :w 800 :h 800 
                :flags '(:resizable :shown))
    (with-renderer (renderer w)
      (with-active-spritesheets ('s4m :renderer renderer)
        (setf (.window game) w)
        (setf (.renderer game) renderer)
        (sdl2-ffi.functions:sdl-render-set-logical-size renderer 800 800)
        (unwind-protect (call-next-method)
          (slot-makunbound game '.window))))))

(defmethod game-loop :around ((game abstract-demo))
  (destructuring-bind (&key title width height tilesets) (%meta game)
    (with-window (w :title title :w 800 :h 800
                  :flags '(:resizable :shown))
      (with-renderer (renderer w)
        (with-active-spritesheets (tilesets :renderer renderer)
          (setf (.window game) w)
          (setf (.renderer game) renderer)
          (sdl2-ffi.functions:sdl-render-set-logical-size renderer width height)
          (unwind-protect (call-next-method)
            (slot-makunbound game '.window)))))))

(defstruct basic-sprite index description)

(defstruct (rect-sprite (:include basic-sprite)) (rect (make-rect 0 0 0 0)))

(defgeneric make-sprite (sprite-description &key &allow-other-keys)
  (:method ((s sprite-description) &key x y w h &allow-other-keys)
    (make-rect-sprite :index 0
                      :description s
                      :rect (if (and x y w h)
                                (make-rect x y w h)
                                (aref (rectangles s) 0)))))

(defgeneric free-sprite (sprite)
  (:method ((s rect-sprite))
    (free-rect (rect-sprite-rect s))
    (setf (rect-sprite-rect s) nil)))

(defmethod (setf test-sprite) :before (nw game)
  (let ((old (and (slot-boundp game 'test-sprite)
                  (slot-value game 'test-sprite))))
    (when old (free-sprite old))))

(defgeneric display (renderer object &key &allow-other-keys)
  (:method (_ (n null) &key &allow-other-keys))
  (:method (renderer (s rect-sprite) &key &allow-other-keys)
    (with-accessors ((d rect-sprite-description)) s
      (let ((src (aref (animation d) (rect-sprite-index s)))
            (dst (rect-sprite-rect s)))
        (sdl2-render renderer d src dst)))))

(defmethod display (renderer (game abstract-demo) &key &allow-other-keys)
  (display renderer (test-sprite game)))

(defmethod handle-event ((game abstract-demo) (event-type (eql :idle)) _)
  (update game)
  (let ((renderer (.renderer game)))
    (set-render-draw-color renderer 0 0 0 255)
    (render-clear renderer)
    (display renderer game)
    (render-present renderer)
    (delay 50)))

(defmethod handle-event ((game abstract-demo) (event-type (eql :keydown)) e)
  (declare (ignore e))
  ())

(defgeneric update (object)
  (:method (_))
  (:method ((g abstract-demo))
    (update (test-sprite g))))

(defmethod update ((s basic-sprite))
  (let ((i (basic-sprite-index s)))
    (setf (basic-sprite-index s)
          (mod (+ i 1) 
               (length
                (animation
                 (basic-sprite-description s)))))))

(defparameter *game*
  (make-instance 'stormhead-demo
                 :test-sprite (make-sprite stormhead:run-left)))


(setf (test-sprite *game*) nil)

;; (run *game*)

