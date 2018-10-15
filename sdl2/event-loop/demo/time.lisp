(defpackage :bricabrac.sdl2.demo.time
  (:use :cl)
  (:export #:make-timer
           #:make-periodic-timer
           #:make-latch-timer
           #:make-dt-source
           #:make-smoothstep
           #:timestep
           #:timer-enabled
           #:timer-period))

(in-package :bricabrac.sdl2.demo.time)

(defstruct (ability (:conc-name)) (enabled t))

(defstruct (timer (:include ability))
  (sum 0))

(defgeneric reset (timer))
(defgeneric timestep (timer dt))

;;;;

(defmethod reset ((timer timer)) (setf sum 0))
(defmethod timestep ((timer timer) dt)  (incf sum dt))

;;;;

(defstruct (countdown
            (:include ability)
            (:constructor make-countdown (delay &aux (remaining delay))))
  (delay internal-time-units-per-second
   :type (integer 0))
  remaining)

(defmethod reset ((countdown countdown))
  (setf (remaining countdown) (delay countdown)))

(defmethod timestep ((countdown countdown) dt)
  "Primary return value: is countdown elapsed.
   Secondary value: remaining time."
  (with-slots (remaining) countdown
    (when (plusp remaining)
      (setf remaining (max 0 (- remaining dt))))
    (values (zerop remaining) remaining)))

;;;;

(defstruct (periodic-timer
            (:include timer)
            (:constructor make-periodic-timer (period)))
  (ratio 0 :type (real 0.0 1.0))
  (period internal-time-units-per-second
   :type (integer 1)))

(defmethod timestep ((periodic-timer periodic-timer) dt)
  (with-accessors ((sum timer-sum)
                   (ratio periodic-timer-ratio)
                   (period periodic-timer-period)
                   (enabled enabled))
      periodic-timer
    (when enabled
      (setf sum (mod (+ sum dt) period))
      (setf ratio (/ sum period)))
    ratio))

(defun make-smoothstep (edge0 edge1)
  (let ((diff (- edge1 edge0)))
    (assert (plusp diff))
    (lambda (ratio)
      (let ((x (alexandria:clamp (/ (- ratio edge0) diff) 0.0 1.0)))
        (* x x x (+ 10 (* x (- (* x 6) 15))))))))

(defun make-dt-source ()
  (let (last)
    (lambda (&aux (now (get-internal-real-time)))
      (prog1 (if last (- now last) 0)
        (setf last now)))))

(defstruct (latch-timer (:include periodic-timer)))

(defmethod timestep ((timer latch-timer) dt)
  (with-accessors ((sum timer-sum)
                   (ratio periodic-timer-ratio)
                   (period periodic-timer-period)
                   (enabled timer-enabled))
      timer
    (when enabled
      (setf sum (+ sum dt))
      (when (> sum period)
        (setf enabled nil)
        (setf sum 0))
      (setf ratio (/ sum period)))
    ratio))
