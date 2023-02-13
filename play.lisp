(defpackage :play
  (:use :cl :bricabrac.with)
  (:use :bricabrac.sdl2.event-loop)
  (:use :bricabrac.utils))

(in-package :play)

(ql:quickload :local-time)
(ql:quickload :cl-opengl)

(defun dbg (format &rest args)
  (fresh-line *debug-io*)
  (apply #'format *debug-io* format args)
  (finish-output *debug-io*))

(defun seconds (seconds)
  (/ seconds internal-time-units-per-second))

(defun update (time-sampler)
  (with-accessors ((last-time .last-time)
                   (reservoir .reservoir)
                   (max-steps .max-steps)
                   (period    .period)
                   (callback  .callback))
      time-sampler
    (flet ((process (dt) 
             (decf reservoir dt)
             (funcall callback dt)))
      (let ((now (osicat:get-monotonic-time)))
        (when last-time
          (let ((dt (- now last-time)))
            (incf reservoir dt)
            (let* ((steps (floor reservoir period))
                   (total (* steps period)))
              (if (> steps max-steps)
                  (process total)
                  (loop repeat  steps do
                    (process period)
                    finally (funcall callback 0 :extra reservoir))))))
        (setf last-time now)))))

(defun fake-computation ())

(defclass time-sampler ()
  ((reservoir :initform 0   :accessor .reservoir)
   (last-time :initform nil :accessor .last-time)
   (callback                :accessor .callback  :initarg :callback)
   (max-steps               :accessor .max-steps :initarg :max-steps)
   (period                  :accessor .period    :initarg :period)))

(defparameter *sampler* 
  (make-instance 'time-sampler
                 :max-steps 10
                 :period 1/5
                 :callback (dbg "P" #'fake-computation)))

(defmethod call-with-context ((w (eql :with))
                              (_ (eql :debug-io-socket))
                              symbols
                              function
                              arguments)
  (assert (null symbols) ())
  (destructuring-bind (&key (file "debug")) arguments
    (iolib:with-open-socket (*debug-io*
                             :address-family :local
                             :remote-filename (namestring 
                                               (merge-pathnames file)))
      (funcall function))))

;; ctx holds the keyword (using/introducing) of the macro
(defstruct bindings data)

(defmethod expand-context ((w (eql :with))
                           (_ (eql :capture))
                           arguments
                           variables
                           body)
  (when (find :io arguments)
    (setf arguments
          (append '(*standard-input*
                    *standard-output* 
                    *error-output*)
                  (remove :io 
                          (subst '*default-pathname-defaults*
                                 :path
                                 arguments)))))
  (assert arguments)
  (loop for a in arguments
        for g = (gensym)
        collect `(,g ,a) into bind
        collect `(,a ,g) into rebind
        finally
           (destructuring-bind (var) variables
             (let ((bindings (make-bindings :data rebind)))
               (return
                 `(let ,bind
                    ,@(subst bindings var body)))))))

(defmethod expand-context ((w (eql :with))
                           (b bindings)
                           arguments
                           variables
                           body)
  (assert (null arguments))
  (assert (null variables))
  `(let ,(bindings-data b)
     ,@body))

(defun run-main-loop (main-loop)
  (assert (symbolp main-loop))
  (assert (symbol-function main-loop))
  (within :temporary-directory
    (with (:capture :path :io) :as <bindings>
      (sdl2:with-everything (:window (w :w 600 :h 600) :gl gl)
        (with (:debug-io-socket :file "/tmp/debug")
          (with <bindings>
            (catch :quit
              (loop
                (dbg "----- ~a" (local-time:now))
                (with-simple-restart (main-loop "Restart main loop")
                  (return
                    (funcall main-loop w)))))))))))

(defun make-smoothstep (v0 v1)
  (let ((diff (- v1 v0)))
    (assert (plusp diff))
    (lambda (ratio)
      (let ((x (alexandria:clamp (/ (- ratio v0) diff) 0 1)))
        (* x x x (+ 10 (* x (- (* x 6) 15))))))))

(declaim (inline transform))

(defun transform (m x y z w)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type single-float x y z w)
           (type (simple-array single-float (16)) m))
  (macrolet ((m (i) `(aref m ,i)))
    (VALUES (+ (* (M 00) X) (* (M 04) Y) (* (M 08) Z) (* (M 12) W))
            (+ (* (M 01) X) (* (M 05) Y) (* (M 09) Z) (* (M 13) W))
       
         ;; (+ (* (M 02) X) (* (M 06) Y) (* (M 10) Z) (* (M 14) W))     
         ;;    (+ (* (M 03) X) (* (M 07) Y) (* (M 11) Z) (* (M 15) W))
            )))

(declaim (inline round-middle))

(defmacro /2 (x) `(/ ,x 2))

(defun round-middle (f)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type (single-float) f))
  (rationalize (- (/2 (+ (fceiling f)
                         (ffloor f)))
                  0.5f0)))

(defun project-point (m x y f)
  (declare (optimize (speed 3) (debug 0) (safety 1))
           (type function f))
  (multiple-value-bind (x y) (transform m x y 0.0 1.0)
    (let ((x (round-middle x)) (y (round-middle y)))
      (funcall f x y))))

(defun run-test (w)
  (let ((*print-right-margin* most-positive-fixnum))
    (let ((range-max 100))
      (let ((smooth (make-smoothstep 0 range-max))
            (margin 20))
        (loop for i from (- margin) to (+ range-max margin) by 1/2
              for s = (funcall smooth i)
              for r = (* s 90 8)
              do (dbg "rot: ~f" r)
                 (gl:clear :color-buffer)
                 (gl:color 0.1 0.1 0.1 1.0)
                 (gl:polygon-mode :front-and-back :line)
                 (gl:rect -1 -4 -1 4) 
                 (gl:rect 0 -4 0 4) 
                 (gl:rect +1 -4 +1 4)
                 (gl:rect -4 -1 +4 -1) 
                 (gl:rect -4 0 +4 0) 
                 (gl:rect -4 +1 +4 +1) 
                 (gl:polygon-mode :front-and-back :fill)                 
                 (let ((matrix (gl:with-pushed-matrix* (:modelview) 
                                 (gl:rotate r 0 0 1)
                                 (gl:rect 0 0 1 1)
                                 (gl:get-float :modelview-matrix))))
                   (gl:color 1 1 1 0.5)
                   (let ((coords))
                     (flet ((add (x y) (push (list x y) coords)))
                       (flet ((process (x y) (project-point matrix x y #'add )))
                         (symbol-macrolet ((lo 0.01) (hi 0.99))
                           (process lo lo)
                           (process lo hi)
                           (process hi hi)
                           (process hi lo))))                     
                     (loop for (x y) in (delete-duplicates coords :test #'equalp) do 
                       ;; (dbg "    => ~3:d ~3:d" (- x 1/2) (- y 1/2))
                       (gl:rect (+ x 0.3) (+ y 0.3) (+ x 0.7) (+ y 0.7))
                       ;; (gl:rect (- x 0.1) (- y 0.1) (+ x 0.1) (+ y 0.1))
                           ))
                   (gl:flush)
                   (sdl2:gl-swap-window w)))))))

(defun test-loop (w)
  (tagbody
   :init
     (gl:matrix-mode :projection)
     (gl:load-identity)
     (gl:ortho -4 4 -4 4 -1 1)
     (gl:clear-color 0.2 0.2 0.2 1.0)
     (gl:enable :blend)
     (gl:blend-func :src-alpha :one-minus-src-alpha)
     (gl:blend-equation :func-add)
     
   :clear
     (gl:clear :color-buffer)
     (gl:color 0.1 0.1 0.1 1.0)
     (gl:flush)
     (sdl2:gl-swap-window w)
     (dbg "----- Press a key -----")
   :wait
     (do-match-events (:method :wait)
       (with-key-down-event (_ :keysym k)
         (case (sdl2:scancode k)
           (:scancode-escape (cerror "IGNORE" "Escape key pressed"))
           (t 
            (funcall 'run-test w)
            (sdl2:with-sdl-event (e)
              (loop until (= 0 (sdl2-ffi.functions:sdl-poll-event e))
                    when (eql :quit (sdl2:get-event-type e))
                      do (throw :quit t)))
            (go :clear))))
       (:quit (return)))))

;; (run-main-loop 'test-loop)

