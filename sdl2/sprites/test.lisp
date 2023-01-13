(defpackage :bricabrac.sdl2.sprites.spritesheets.test
  (:use
   :cl
   :sdl2
   :alexandria
   :bricabrac.sdl2.sprites.spritesheets
   :bricabrac.sdl2.sprites.textures
   :bricabrac.sdl2.sprites.transform
   :bricabrac.sdl2.event-loop
   :bricabrac.environments
   :bricabrac.mixins)
  (:export #:snake-test))

(defpackage :forest (:use))

(in-package :bricabrac.sdl2.sprites.spritesheets.test)

(defun sdl-rect-from-env (&key row col env transform)
  (multiple-value-call #'sdl2:make-rect
    (pixel-rectangle col
                     row
                     (getf env :width 1)
                     (getf env :height 1)
                     transform)))

(define-spritesheet platformer-in-the-forest (:package :forest)

  ;; speed per frame is expressed as a pixel-increment per update, with respect
  ;; to the original pixel size of the tile (here 32).
  (:speed/frame (lambda (old new)
                  (case new
                    (:negate (map 'simple-vector #'- old))
                    (t new))))

  ;; :event represents event handlers as an alist mapping actions (keywords) to
  ;; branch paths; those are intended to be used with switch-branch. Event
  ;; handlers are inherited from parent environment but can be shadowed by
  ;; child environments.
  (:event (lambda (old new) (append new old)))

  ;; flip is a list, combine with union
  (:flip #'union)
  
  ;; merge pathnames
  (:file (lambda (old new) (if old (merge-pathnames new old) new)))
  
  `(_ (:tile-callback ,#'sdl-rect-from-env
       :order (:row :col))

      (_ (:file #p"snake.png"
          :transform ,(move 8 0) ;; align to actual left of sprite
          :transform ,(scale 32) ;; each row/col takes 32 pixels in width
          :transform ,(move -1)  ;; 1-based index

          ;; the actual sprite width is 16 pixels
          :width (:px 16))
         (snake (:row 4
                 :event ((:stop idle)))
                (idle (:col 1 :speed/frame #(0))
                      (left (
                             :speed/frame :negate
                             :event ((:left slither)
                                     (:right right))))

                      (right (:flip (:horizontal)
                              :event ((:right slither)
                                      (:left left)))))

                (slither (:col
                          (2 2 2 1 1 1)
                          :speed/frame
                          #(1 2 3 0 0 0))

                         (left (:speed/frame :negate
                                :event ((:right idle))))

                         (right (:flip (:horizontal)
                                 :event ((:left idle)))))))
      (_ (:file #p"exterior.png"
                :transform ,(scale 16)
                :height 1
                :width 1)
         (ground (:col 0 :row 3)))))

;;(find-spritesheet 'platformer-in-the-forest)
;; on-sprite-redefinition-hook

(defun snake-test ()
  (let* ((width 800) (height 512) (origin 100) (size 128)
         ;; width of destination rectangle
         (dest-width (* 16/32 size))
         (extended-width (+ width dest-width))
         (real-x origin)
         (next-description)
         (snake forest:snake-idle-right)
         (index 0)
         (speed 0))
    (flet ((wrap-around (position)
             "Wrap position around window, taking into account dest-width"
             (- (mod (+ position dest-width)
                     extended-width)
                dest-width))
           (scale-speed (speed)
             "Adjust speed for size"
             ;; the original speed values in the spritesheet are for a 128
             ;; pixel-wide sprite box.
             (* speed size 1/32)))
      (with-everything (:window (window :w width
                                        :h height
                                        :title "Snake")
                        :gl gl)
        (let* ((*default-pathname-defaults* #P"~/calciumtrice/")
               (ground-src (svref (rectangles forest:ground) 0))
               (ground-dst (sdl2:copy-rect ground-src)))
          (setf (rect-y ground-dst) (- height (rect-height ground-dst)))
          (with-renderer (renderer window)
            (set-render-draw-color renderer 20 15 15 255)
            (with-rects ((dest origin
                               (- height size (sdl2:rect-height ground-dst) -2)
                               dest-width
                               size))
              (with-active-spritesheets ('platformer-in-the-forest
                                         :renderer renderer)
                (do-match-events (:method :wait :timeout 60)
                  (:quit (return))
                  (:idle
                   ;; logic
                   (when (and next-description (zerop speed))
                     (setf index 0)
                     (shiftf snake next-description nil))
                   (environment-bind (speed/frame) (environment snake)
                     (setf speed (svref speed/frame index))
                     (setf real-x (wrap-around (+ (scale-speed speed) real-x)))
                     (setf (rect-x dest) (round real-x)))
                   ;; render
                   (render-clear renderer)
                   (loop
                     for x from 0 below width by (rect-width ground-dst)
                     do (setf (rect-x ground-dst) x)
                        (sdl2-render renderer
                                     forest:ground
                                     ground-src
                                     ground-dst))
                   (sdl2-render-index renderer snake index dest)
                   (setf index (mod (1+ index) (length (rectangles snake))))
                   (render-present renderer))
                  (with-key-down-event (_ :keysym key :repeat repeat)
                    (when (zerop repeat)
                      (environment-bind (event) (environment snake)
                        (when-let (branch
                                   (cdr (assoc
                                         (case (scancode key)
                                           ((:scancode-space
                                             :scancode-down
                                             :scancode-up)
                                            :stop)
                                           (:scancode-left :left)
                                           (:scancode-right :right))
                                         event)))
                          (let ((switch (switch-branch snake branch)))
                            (unless (eq switch snake)
                              (setf next-description switch))))))))))))))))

 (snake-test)

