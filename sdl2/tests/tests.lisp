(defpackage :bricabrac.sdl2.tests
  (:use :cl :sdl2 :bricabrac.sdl2 :alexandria)
  (:export #:single-loop
           #:dispatch))

(in-package :bricabrac.sdl2.tests)

(defparameter *message*
  "NOTE: nothing is displayed in the window.
Events are logged to *STANDARD-OUTPUT*.")

;;;;
;;;; Example of single loop event handling
;;;;

(defun single-loop ()
  (format t "~&~a~%" *message*)
  (with-captured-bindings (rebind *standard-output* *error-output*)
    (with-everything (:window (w :w 600 :h 600) :gl gl)
      (rebind
       (flet ((info (&rest rest)
                (declare (dynamic-extent rest))
                (print rest)))
         (do-match-events (:method :wait :timeout 600)
           (:quit (return))
           (:idle (sb-ext:gc) (info :idle))
           (with-window-event-moved (_ :x x :y y)
             (info :window :x x :y y))
           (with-window-event-focus-gained (_)
             (info :focused))
           (with-mouse-motion-event (_ :xrel dx :yrel dy)
             (info :mouse :dx dx :dy dy))
           (with-finger-motion-event (_ :dx dx :dy dy)
             (info :finger :dx dx :dy dy))))))))

;;;;
;;;; Example of CLOS-based event-handler
;;;;

(defgeneric handle-event (game event-type event)
  (:method (game type _)
    ;; too much output otherwise
    (unless (member type '(:windowevent :mousemotion :idle))
      (format t "~&Event ~@{~a~^ ~}~%" game type)))
  (:method (game (type (eql :quit)) _)
    (throw 'quit nil)))

(defun dispatch ()
  (format t "~&~a~%~%" *message*)
  (restart-case
      (with-captured-bindings (rebind *standard-output* *error-output*)
        (with-everything (:window (w :w 600 :h 600) :gl gl)
          (rebind
           (catch 'quit
             (do-events (event :event-type type :method :poll)
               (restart-case (handle-event :test type event)
                 (ignore () :report "Ignore event")))))))
    (retry () :report "Recreate window")))

(defmethod handle-event (game (type (eql :windowevent)) event)
  (with-raw-window-event (event :event code)
    (handle-event game (windowevent code) event)))

(defmethod handle-event (game (type (eql :windowevent-moved)) event)
  (with-window-event-moved (event :x x :y y)
    (format t "~&Window moved for game ~s: ~d, ~d~%" game x y)))

(defmethod handle-event ((game (eql :test)) (type (eql :keydown)) event)
  (with-key-down-event (event :keysym keysym)
    (if-let (command
             (case (scancode keysym)
               (:scancode-left :go-left)
               (:scancode-right :go-right)))
      (handle-event game :command command))))

(defmethod handle-event (game (type (eql :textinput)) event)
  (with-text-input-event (event :text code)
    (format t
            "~&Text input: ~S~%"
            (if (typep code 'char-code)
                (string (code-char code))
                :UNKNOWN))))

(defmethod handle-event (game (type (eql :command)) event)
  (format t "~&Game ~s received command ~a~%" game event))

(register-user-event-type :test-event)
(defmethod handle-event (game (type (eql :test-event)) event)
  (format t "~&Game ~s received :test-event~%" game))

(defmethod handle-event (game (type (eql :fingerup)) event)
  (with-finger-up-event (event)
    (format t "~&Finger up~%")
    (push-user-event :test-event)))
