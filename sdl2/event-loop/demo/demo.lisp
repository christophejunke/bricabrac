(defpackage :bricabrac.sdl2.demo
  (:use
   :cl
   :sdl2
   :bricabrac.sdl2.event-loop
   :bricabrac.sdl2.demo.time)
  (:export #:demo))

(in-package :bricabrac.sdl2.demo)

(defclass demo ()
  ((timer/spin :initform (make-periodic-timer (seconds 3))
               :accessor timer/spin)
   (timer/spin-world :initform (make-periodic-timer (seconds 5))
                     :accessor timer/spin-world)
   (timer/plan :initform (make-latch-timer :period (seconds 4))
               :accessor timer/plan)
   (smoothstep :initform (make-smoothstep 0.0 1.0)
               :accessor smoothstep)
   (dt-source :initform (make-dt-source) :accessor dt-source)
   (grid :accessor grid :initform 200)
   (width :accessor width :initform 300)
   (height :accessor height :initform 300)
   (cross-x :accessor cross-x :initform 30)
   (cross-y :accessor cross-y :initform 30)))

(defparameter *demo* (make-instance 'demo))

(defgeneric handle (game event-type &key event window)
  (:method (game type &key &allow-other-keys)
    ;; too much output otherwise
    (unless (member type '(:windowevent :mousemotion :idle))
      (format t "~&Event ~@{~a~^ ~}~%" game type)))
  (:method (game (type (eql :quit)) &key &allow-other-keys)
    (throw 'quit nil)))

(defgeneric dispatch (game)
  (:method ((game demo))
    (with-everything
        (:window (w :w (width game)
                    :h (height game)
                    :flags '(:opengl :resizable :shown))
         :gl gl)
      (sdl2:gl-make-current w gl)
      (handle game :start :window w)
      (catch 'quit
        (do-events (event :event-type type :method :wait :timeout (dt game))
          (restart-case (handle game type :event event :window w)
            (ignore () :report "Ignore event")))))))

(defgeneric display (thing &key window)
  (:method (_ &key &allow-other-keys)))

(defun set-size (game width height)
  (setf (height game) height)
  (setf (width game) width)
  (multiple-value-bind (x y w h) (shrink-view width height)
    (gl:viewport x y w h))
  (gl:matrix-mode :projection)
  (gl:load-identity)
  (gl:ortho 0 (grid game) (grid game) 0 (- (grid game)) (grid game)))

(defmethod (setf grid) :after (nv (game demo))
  (sdl2:in-main-thread ()
    (set-size game (width game) (height game))))

(defmethod handle ((game demo)
                         (type (eql :start))
                         &key &allow-other-keys)
  (gl:enable :blend)
  (gl:blend-func :src-alpha :one-minus-src-alpha)
  (gl:blend-equation :func-add)
  (gl:clear-color 0 0 0 1)
  (gl:clear :color-buffer)
  (set-size game (width game) (height game)))

(defmethod handle ((game demo)
                         (type (eql :idle))
                         &key window &allow-other-keys)
  (gl:clear-color .1 .1 .1 1)
  (gl:clear :color-buffer)
  (gl:color 0 0 0 0.5)
  (gl:rect 0 0 (width game) (height game))
  (display game :window window)
  (gl:flush)
  (sdl2:gl-swap-window window))

(defun transform (game window x y)
  (multiple-value-bind (ww wh) (sdl2:get-window-size window)
    (values (* (grid game) (/ x ww))
            (* (grid game) (/ y wh)))))

(defun set-cross (game window x y)
  (multiple-value-bind (x y) (transform game window x y)
    (setf (cross-x game) (round x))
    (setf (cross-y game) (round y))))

(defmethod handle ((game demo)
                   (type (eql :mousebuttondown))
                   &key event window &allow-other-keys)
  (with-mouse-button-down-event (event :x x :y y)
    (set-cross game window x y)))

(defmethod handle ((game demo)
                   (type (eql :mousemotion))
                   &key event window &allow-other-keys)
  (with-mouse-motion-event (event :state state :x x :y y)
    (when (= 1 state)
      (set-cross game window x y))))

(defmethod handle ((game demo) (type (eql :fingerdown))
                         &key &allow-other-keys)
  (handle game :rotate-plan))

(defmethod handle ((game demo) (type (eql :fingerdown))
                         &key &allow-other-keys)
  (handle game :rotate-plan))

(defmethod handle ((game demo) (type (eql :keydown))
                         &key event window &allow-other-keys)
  (with-key-down-event (event :keysym key)
    (let ((scancode (sdl2:scancode key)))
      (case scancode
        (:scancode-space (handle game :rotate-plan))
        (:scancode-escape (handle game :quit))
        (t (handle game
                   scancode
                   :window window))))))

(defmethod handle ((game demo) (type (eql :scancode-s))
                   &key window &allow-other-keys)
  (cffi:with-foreign-string (file "/tmp/screenshot.bmp")
    (sdl2-ffi.functions:sdl-save-bmp-rw window file 1)))

(defmethod handle ((game demo)
                         (type (eql :rotate-plan)) &key
                         &allow-other-keys)
  (setf (timer-enabled (timer/plan game)) t))

(defgeneric dt (game)
  (:method (game)
    "Constant DT per tick"
    50))

(defmethod dt ((game demo))
  "Use DT source"
  (funcall (dt-source game)))

;;(setf (timer-period (timer/spin-world *demo*)) 5000)

(defmethod display ((game demo) &key &allow-other-keys)
  (let* ((dt (dt game))
         (scale/spin (timestep (timer/spin game) dt))
         (scale/plan (funcall (smoothstep game)
                              (timestep (timer/plan game) dt)))
         (angle/spin (alexandria:lerp scale/spin 0 #.360))
         (angle/spin-world (alexandria:lerp (timestep (timer/spin-world
                                                       game)
                                                      dt)
                                            0 #.360))
         (angle/plan (alexandria:lerp scale/plan 0 #.360))
         (half (floor (grid game) 2))
         (noise (* scale/plan (- 1 scale/plan))))
    (flet ((gear (x y s a)
             (display :gear
                      :x x :y y
                      :steps s :angle a
                      :noise noise)))

      (gl:with-pushed-matrix
        (gl:translate half half 0)
        (gl:rotate angle/plan 11 13 87)
        (gl:rotate angle/spin-world 0 0 1)

        (gl:color 0.3 1 0.3 .4)
        (gear 5 -22 5 (- 85 angle/spin))

        (gl:color 0.7 1.0 0.4 0.8)
        (gear -28 -12 4 angle/spin)

        (gl:color 1.0 1.0 1 0.3)
        (gear -15 -8 3 (- 45 angle/spin))

        (gl:color 1 1 0 0.9)
        (gear 0 0 5 angle/spin)

        (gl:color 1 0.7 0 0.4)
        (gear 20 5 6 (- 30 angle/spin))

        (gl:color 1 0.4 0 0.9)
        (gear -10 15 5 (- 30 angle/spin))

        )

      ;; (gl:color 1 1 0 0.3)
      ;; (display :sonar
      ;;          :x (cross-x game)
      ;;          :y (cross-y game)
      ;;          :angle (* 4 angle/spin)
      ;;          :steps 10
      ;;          :noise noise)

      (gl:color 0.5 0.5 0.5 0.5)
      (let* ((low 10)
             (high (- (grid game) low)))
        (flet ((draw (x y)
                 (display :gear
                          :x x :y y
                          :steps 2 :angle (* angle/spin 2)
                          :noise 0)))
          (draw low low)
          (draw low high)
          (draw high low)
          (draw high high))))))

(defun randnoise (noise scale)
  (let ((z (round (* noise scale))))
    (if (plusp z) (- (random z) (floor z 2)) 0)))

(defmethod display ((item (eql :gear))
                    &key x y angle (steps 5) (noise nil)
                    &allow-other-keys)
  (gl:with-pushed-matrix
    (gl:translate x y 0)
    (gl:rotate angle 0 0 1)
    (loop
      repeat 3
      do (gl:rotate #.(/ 360 3) 0 0 1)
         (gl:translate 0 0 #1=(randnoise noise 60))
         (gl:with-pushed-matrix
           (dotimes (i steps)
             ;(gl:rect -.3 -1 .3 1)
             (gl:rect -1 -1 1 1)
;             (gl:rotate (randnoise noise 100) 0 0 1)
             (gl:translate (+ 3 (randnoise noise 30)) 0 #1#)
             )))))


(defmethod display ((item (eql :sonar))
                    &key x y angle (steps 5) (noise nil)
                    &allow-other-keys)
  (gl:with-pushed-matrix
    (gl:translate x y 0)
    (gl:rotate angle 0 0 1)
    (loop repeat 2
          do
             (gl:with-pushed-matrix
               (loop
                 repeat 5
                 do (gl:rotate 5 0 0 1)
                    (gl:translate 0 0 #1=(randnoise noise 60))
                    (gl:with-pushed-matrix
                      (dotimes (i steps)
;(gl:rect -.3 -1 .3 1)
                        (gl:rect -1 -1 1 1)
;             (gl:rotate (randnoise noise 100) 0 0 1)
                        (gl:translate (+ 5 (randnoise noise 30)) 0 #1#)
                        ))))
          (gl:rotate 180 0 0 1))))


(defun fill-view (width height)
  (let* ((max (max width height))
         (w-delta (/ (- max width) 2))
         (h-delta (/ (- max height) 2)))
    (values (ceiling (- w-delta))
            (ceiling (- h-delta))
            max
            max)))

(defun shrink-view (width height)
  (let* ((min (min width height))
         (w-delta (/ (- min width) 2))
         (h-delta (/ (- min height) 2)))
    (values (ceiling (- w-delta))
            (ceiling (- h-delta))
            min
            min)))

(defmethod handle ((game demo)
                         (type (eql :windowevent-resized))
                         &key event &allow-other-keys)
  (with-window-event-resized (event :width w :height h)
    (set-size game w h)))

(defmethod handle ((game demo)
                         (type (eql :windowevent))
                         &key event window &allow-other-keys)
  (with-raw-window-event (event :event code)
    (handle game (windowevent code) :event event :window window)))

;;(setf (timer-period (timer *demo*)) (seconds 2))
;;(setf (timer-period (timer/plan *demo*)) (seconds 4))
;;(setf (smoothstep *demo*) (make-smoothstep 0.0 1))
;;(setf (grid *demo*) 200)

(defun demo ()
  (dispatch *demo*))

#+additional-demo
;; WHile (demo) is executing, change the grid size
(progn

  ;; redefine as returning NIL to stop animation
  (defun run () t)

  ;; in its own thread (e.g. slime-eval)
  (let ((timer (make-periodic-timer (seconds 1))))
    (loop
      while (run)
      for value = (timestep timer 20)
      for angle = (alexandria:lerp value 0 #.(* pi 2))
      for sinus = (sin angle)
      do (setf (grid *demo*) (+ 350 (* sinus 300)))
         (sleep 0.02))))
