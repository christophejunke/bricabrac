(defpackage :dungeon
  (:use
   :cl
   :sdl2
   :alexandria
   :bricabrac.environments.hash-consing
   :bricabrac.sdl2.sprites.spritesheets
   :bricabrac.sdl2.sprites.textures
   :bricabrac.sdl2.sprites.transform
   :bricabrac.sdl2.event-loop
   :bricabrac.environments
   :bricabrac.mixins)
  (:export #:dungeon)
  (:shadow #:character))

(defpackage :calciumtrice (:use))

(in-package :dungeon)

(defvar *debug* t)

(defun sdl-rect-from-env (&key row col env transform)
  (multiple-value-call #'sdl2:make-rect
    (pixel-rectangle col
                     row
                     (resolve-value env :width 1)
                     (resolve-value env :height 1)
                     transform)))

;; (define-spritesheet toto (:package nil)
;;   '(root (:file #P"tileset.png"
;;           :order (:row :col) :row 1 :col 1) child))

(defun delta/frame-reducer (old new)
  (case new
    (:negate (typecase old
               (number (- old))
               (vector (map 'simple-vector #'- old))
               (t 0)))
    (t new)))

(defun calciumtrice (name)
  (merge-pathnames
   (make-pathname :directory '(:relative "jeu")
                  :type "png"
                  :name (format nil "~a spritesheet calciumtrice" name) )
   (user-homedir-pathname)))

;; (find-spritesheet 'dungeon-by-calciumtrice)
;; on-sprite-redefinition-hook

(defmacro switchf (place branch)
  `(setf ,place (switch-branch ,place ,branch)))

;; (switchf (first *sprite*) 'rat)
;; (switchf (first *sprite*) 'rat)
;; (switchf (first *sprite*) '(gesture right))
;; (switchf (first *sprite*) 'short)

(defclass has-keymap ()
  ((keymap :accessor keymap
           :initarg :keymap
           :initform nil)))

(defstruct keymap-entry key target)
(defstruct (event-keymap-entry (:include keymap-entry)) event)
(defstruct (action-keymap-entry (:include keymap-entry)) action)
(defstruct (delegate-keymap-entry (:include keymap-entry)) accessor)

(defun parse-keymap-entry (entry target)
  (etypecase entry
    (keymap-entry entry)
    (cons
     (destructuring-bind (key type . args)
         (if (symbolp (cdr entry))
             (list (first entry) :action (cdr entry))
             entry)
       (check-type key symbol)
       (when (eq key 'otherwise)
         (setf key t))
       (ecase type
         (:delegate
          (destructuring-bind (accessor) args
            (make-delegate-keymap-entry
             :key key
             :target target
             :accessor accessor)))
         (:event
          (destructuring-bind (event) args
            (make-event-keymap-entry
             :key key
             :target target
             :event event)))
         (:action
          (destructuring-bind (action) args
            (check-type action (or keyword function))
            (make-action-keymap-entry
             :key key
             :target target
             :action action))))))))

(define-condition shadowed-key-entry (error)
  ((new :initarg :new :accessor shadowed-key-entry/new)
   (old :initarg :old :accessor shadowed-key-entry/old))
  (:report (lambda (object stream)
             (format stream
                     "~@<~a ~:_is shadowed by previous entry ~:_~a~:>"
                     (shadowed-key-entry/new object)
                     (shadowed-key-entry/old object)))))

(defun ensure-keymap (keymap target)
  (let ((entries (mapcar (lambda (clause)
                           (parse-keymap-entry clause target))
                         keymap))
        (seen (make-hash-table :test #'eq))
        (errors))
    (dolist (entry entries)
      (let* ((key (keymap-entry-key entry))
             (existing (or #1=(gethash key seen)
                           (gethash 't seen))))
        (if existing
            (push (make-condition 'shadowed-key-entry
                                  :new entry
                                  :old existing)
                  errors)
            (setf #1# entry))))
    (when errors
      (if (rest errors)
          (error
           "Keymap errors: ~%~%~{  - ~a~%~}"
           (nreverse errors))
          (error (first errors))))
    entries))

(defun ensure-keymaps (keymaps target)
  (loop for (modifier . keymap) in keymaps
        collect (cons modifier (ensure-keymap keymap target))))

;; (handler-bind ((error
;;                  (lambda (e)
;;                    (let ((*print-right-margin* 10))
;;                      (format t "~&~%~a" e)) (invoke-restart 'ignore))))
;;   (restart-case
;;       (ensure-keymap '((:scancode-quit :event :quit)
;;                        (:scancode-quit :event :crash)
;;                        (t :delegate hero)))
;;     (ignore ())))

(defmethod initialize-instance :after ((object has-keymap) &key &allow-other-keys)
  (setf (keymap object) (ensure-keymaps (keymap object) object)))

(defclass has-position ()
  ((x :initform 0 :accessor x)
   (y :initform 0 :accessor y)
   (rectangle :initform (make-rect 0 0 0 0)
              :accessor rectangle)))

(defclass character (has-position
                     has-keymap)
  ((sprite :allocation :class
           :initarg :sprite
           :accessor sprite)
   (index :initarg :index
          :initform 0
          :accessor index)
   (next-action :initform nil
                :accessor next-action)))

(defmacro kmap (&body bindings)
  `',(ensure-keymaps bindings nil))

 ;;;;;;;;;;

(defstruct animation
  (direction 1 :type (member 1 nil -1))
  continuation)

(defun reverse-animation (animation)
  (make-animation
   :direction (- (animation-direction animation))
   :continuation (animation-continuation animation)))

(defun ensure-animation (animation)
  (etypecase animation
    (animation animation)
    ((eql :circular) (make-animation :continuation #'identity))
    (cons (destructuring-bind (clause . args) animation
            (ecase clause
              (:next (make-animation
                      :direction nil
                      :continuation (lambda (sprite)
                                      (switch-branch sprite args)))))))))

(defun animation-start-index (sprite)
  (if (plusp (animation-direction (animation sprite)))
      0
      (1- (length (rectangles sprite)))))

(defun next-sprite-index (sprite index)
  (let* ((animation (animation sprite))
         (rectangles (rectangles sprite))
         (next-index (+ index (animation-direction animation))))
    (if (array-in-bounds-p rectangles next-index)
        (values sprite next-index)
        (let ((next-sprite (funcall (animation-continuation animation)
                                    sprite)))
          (values next-sprite (animation-start-index next-sprite))))))
;;;;;;

(defclass character-sprite (sprite-description)
  ((animation :accessor animation)
   (offsets :accessor offsets)))

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
    (environment-bind (animation gravity) (environment sprite)
      (setf (animation sprite)
            (ensure-animation animation))
      (setf (offsets sprite)
            (multiple-value-bind (xr yr) (gravity-ratios gravity)
              (with-hash-consing (hash :test #'equalp)
                (flet ((offset (w h) (hash (vector (* xr w) (* yr h)))))
                  (map 'simple-vector
                       (lambda (r)
                         (offset
                          (rect-width r)
                          (rect-height r)))
                       rectangles))))))))

;; (funcall (animation-continuation (ensure-animation '(:next walk left)))
;;          (sprite (hero *game*)))

(defun animation-reducer (old new)
  (let ((old (ensure-animation old)))
    (etypecase old
      (null (ensure-animation new))
      (animation (case new
                   (:reverse (reverse-animation old))
                   (t (let ((new (ensure-animation new)))
                        (prog1 new
                          (unless (animation-direction new)
                            (setf (animation-direction new)
                                  (animation-direction old)))))))))))

(defclass sprites-have-offsets () ())
(defclass dungeon-spritesheet (spritesheet
                               sprites-have-offsets)
  ())

(defmethod finalize-all-sprite-descriptions :after
    ((spritesheet sprites-have-offsets) descriptions)
  (with-hash-consing (hash :test #'equalp)
    (map ()
         (lambda (sprite)
           (with-accessors ((offsets offsets)) sprite
             (setf offsets (hash (map-into offsets #'hash offsets)))))
         descriptions)))

(defun range (from to &aux (delta (- to from)))
  (alexandria:iota (1+ (abs delta))
                   :start from
                   :step (signum delta)))

;;;;;;

(define-spritesheet dungeon-by-calciumtrice (:package :calciumtrice
                                             :class 'dungeon-spritesheet)
  (:flip (lambda (old new) (set-difference new old)))
  (:dx/frame #'delta/frame-reducer)
  (:dy/frame #'delta/frame-reducer)
  (:animation #'animation-reducer)
  (:switch (lambda (old new)
             (case new
               (:disable nil)
               (t (append new old)))))
  (let ((flip '(:horizontal)))
    (flet ((vertical-move (direction)
             "Hack"
             (lambda (character)
               (incf (y character) (* (game-scale *game*)
                                      direction)))))
      `(_ (:tile-callback ,#'sdl-rect-from-env
           :sprite-description-class ,(find-class 'character-sprite)
           :order (:row :col)
           :col (:range 1 10)
           :dx/frame 0
           :dy/frame 0
           :animation :circular
           :gravity :south

           :gesture-row 2
           :speed 2
           :attack-prepare (1 2 3 4)
           :attack-hold (5 6 7 6)
           :attack-stop (4 3 2 1)
           :attack-release (5 6 7 8 9 10 10)
           :gesture-cols (:range 1 10)           

           :switch ((:left walk left)
                    (:right walk right)
                    (:attack attack prepare)
                    (:gesture gesture)
                    (:stop idle)
                    (:down idle)
                    (:up idle)))
          (_ (:transform ,(scale 32) 
              :transform ,(move -1)
              :variant ,(move 0 5)) 
             ((:each
               (wizard (:file ,(calciumtrice "wizard")
                        :attack-prepare (1 2 3 4)
                        :attack-hold 4
                        :attack-release (5 6 7 8 9 10 2 3 4))
                       (short (:switch ((:toggle long))))
                       (long (:transform (ref :variant)
                              :switch ((:toggle short)))))
               (rogue (:file ,(calciumtrice "rogue")
                       :transform (ref :variant)
                       :attack-prepare (1 2 )
                       :attack-hold 2
                       :speed 3))
               (cleric (:file ,(calciumtrice "cleric")
                        :gesture-cols (,@(range 1 10) 10 10)
                        :attack-prepare (1 2)
                        :attack-hold 3
                        :attack-release (2 3 4 5 6 7 8 9 10 4 3 1)))
               (goblin (:file ,(calciumtrice "goblin")
                        :attack-prepare (1 2)
                        :attack-hold 3)
                       (weak (:switch ((:toggle strong))))
                       (strong (:transform (ref :variant)
                                :switch ((:toggle weak)))))                   
               (ranger (:file ,(calciumtrice "ranger")
                        :gesture-cols 1
                        :speed 2
                        :attack-prepare (:range 1 6)
                        :attack-hold 7
                        :attack-stop (:range 5 1)
                        :attack-release (8 9 10)))
               (minotaur (:file ,(calciumtrice "minotaur")
                          :flip ,flip
                          :gesture-cols (1 2 3 4 5 6 6 6 6 7 8 9 10)
                          :attack-prepare (1 2)
                          :attack-hold (3)
                          :attack-stop 1
                          :attack-release (:range 4 10)
                          :transform :reset
                          :transform ,(scale 48)
                          :transform ,(move -1))))
              ;; default properties for characters
              ()
              ((:each (idle (:row 1
                             :switch ((:up . ,(vertical-move -10))
                                      (:down . ,(vertical-move 10)))))
                      #2=(gesture (:row (ref :gesture-row)
                                   :switch ((t))
                                   :col (ref :gesture-cols)
                                   :animation (:next idle)))
                      #3=(walk (:row 3 :dx/frame (ref :speed)))
                      (attack (:row 4)
                              (prepare (:col (ref :attack-prepare)
                                        :animation (:next hold)
                                        :switch ((:cancel-attack idle)
                                                 (t))))
                              (hold (:col (ref :attack-hold) 
                                     :switch ((:cancel-attack stop)
                                              (:up release)
                                              (:left left)
                                              (:right right)
                                              (t))))
                              (stop (:col (ref :attack-stop)
                                     :animation (:next idle)
                                     :switch ((t))))
                              (release (:col (ref :attack-release)
                                        :trigger :attack
                                        :switch ((:cancel-attack idle)
                                                 (t))
                                        :animation (:next hold))))
                      #4=(death (:row 5)
                                (dying (:animation (:next dead)
                                        :col (1 1 2 2 3 3 4 4 5 5 6 6 7 7 8 8 9 9 10 10)))
                                (dead (:col 10))))
               ()
               (left (:flip (:horizontal) :dx/frame :negate))
               (right)))

             ((:each
               (_ (:file ,(calciumtrice "rat and bat")
                   :flip ,flip
                   :switch ((:cancel-attack)))
                  (rat (:switch ((:toggle bat))
                         :idle-range (1 1 2 2 3 2)))
                  (bat (:transform (ref :variant)
                        :gravity (1/2 . 0.6)
                        :switch ((:toggle rat))
                        :attack-after (:next walk)
                        :attack-dx10 #(1  1  1  1  3  5  5  3  3  1)))))
              (:attack-after (:next idle)
               :attack-dx10 #(0  0  0  0  3  3  3  0  0  0)
               :switch ((:attack attack moving))
               :idle-range (:range 1 10))
              ((:each (idle (:row 1
                             :col (ref :idle-range)
                             :switch ((:up . ,(vertical-move -10))
                                      (:down . ,(vertical-move 10))
                                      (:attack attack standing))))
                      #2#
                      #3#
                      #4#
                      (attack (:row 4
                               :col (:range 1 10)
                               :dx/frame (ref :attack-dx10))
                              (moving (:animation (ref :attack-after)))
                              (standing (:animation (:next idle)))))
               ()
               (left (:flip (:horizontal)
                      :dx/frame :negate))
               (right)))
            
             ;; (slime (:file ,(calciumtrice "slime") :flip t)
             ;;        ((:each (green ,(filenv :variant1))
             ;;                (blue ,(filenv :variant2)))
             ;;         ()
             ;;         ((:each 
             ;;           (idle (:row 1))
             ;;           (walk (:row 3 :dx/frame #(1 2 2 1 1 0 0 0 0 0)))
             ;;           (attack  (:row 4 :col (1 1 1 1 2 3 4 5 6 7 8 9 10 2 1))))
             ;;          ()
             ;;          (left (:flip (:horizontal) :dx/frame :negate))
             ;;          (right))))
             )))))

(defclass wizard (character)
  ()
  (:default-initargs
   :sprite calciumtrice:wizard-short-idle-right))

(defclass forest-dungeon (has-keymap)
    ((hero :initarg :hero
           :accessor hero
           :initform (make-instance 'wizard))
     (sleep-duration
      :initarg :sleep-duration
      :initform 0.05
      :accessor sleep-duration)
     (scale
      :initarg :scale
      :initform 2
      :accessor game-scale)))

(defparameter *hero*
  (make-instance 'wizard
                 :keymap '((:scandown
                            (:scancode-left   . :left)
                            (:scancode-right  . :right)
                            (:scancode-space  . :stop)
                            (:scancode-return . :stop)
                            (:scancode-up     . :up)
                            (:scancode-down   . :down)
                            (:scancode-f1     . :toggle)
                            (:scancode-lctrl  . :attack)
                            (:scancode-g      . :gesture))
                           (:scanup
                            (:scancode-lctrl  . :cancel-attack)))))

(defparameter *game*
  (make-instance 'forest-dungeon
                 :hero *hero*
                 :keymap '((:scandown
                            (:scancode-escape :event :quit)
                            (:scancode-i :event :zoom-in)
                            (:scancode-d :event :zoom-out)                   
                            (t :delegate hero))
                           (:scanup
                            (t :delegate hero)))))

(defvar *renderer*)

(defgeneric game-loop (game))
(defgeneric event-step (game))

(defgeneric handle-event (game event-type event)
  (:method (game type event))
  (:method (game (type (eql :quit)) event)
    (throw 'quit nil)))

(defgeneric render (object renderer)
  (:method (object renderer)))

(defmethod game-loop ((game forest-dungeon))
  (with-active-spritesheets ('dungeon-by-calciumtrice
                             :renderer *renderer*)
    (catch 'quit
      (do-events (event :event-type type :method :poll)
        (restart-case (handle-event game type event)
          (ignore () :report "Ignore event"))))))

(defmethod handle-event :around ((game forest-dungeon)
                                 (type (eql :idle))
                                 event)
  (set-render-draw-color *renderer* 20 30 20 255)
  (render-clear *renderer*)
  (set-render-draw-color *renderer* 255 255 255 255)
  (call-next-method)
  (render-present *renderer*)
  (sleep (sleep-duration game)))

(defgeneric update (object)
  (:method (object)))

(defmethod handle-event ((game forest-dungeon)
                         (type (eql :idle))
                         event)
  (update game)
  (render game *renderer*))

(defmethod handle-event ((game forest-dungeon)
                         (type (eql :zoom-in))
                         event)
  (incf (game-scale game)))

(defmethod handle-event ((game forest-dungeon)
                         (type (eql :zoom-out))
                         event)
  (decf (game-scale game)))

(defmethod render ((game forest-dungeon) renderer)
  (render (hero game) renderer))

(defmethod render ((character character) renderer)
  (with-accessors ((sprite sprite)
                   (index index)
                   (x x)
                   (y y))
      character
    (let ((source (svref (rectangles sprite) index))
          (offset (svref (offsets sprite) index))
          (scale (game-scale *game*)))
      (with-rects ((rectangle (round (- x (* scale (svref offset 0))))
                              (round (- y (* scale (svref offset 1))))
                              (round (* scale (sdl2:rect-width source)))
                              (round (* scale (sdl2:rect-height source))))
                   (gravity (- x 10)
                            (- y 10)
                            21
                            21))
        (bricabrac.sdl2.sprites.textures:sdl2-render
         renderer
         (sprite character)
         source
         rectangle)
        (when *debug*
          (render-fill-rect *renderer* gravity))))))

(defmethod update ((game forest-dungeon))
  (update (hero game)))

(defmethod update ((character character))
  (with-accessors ((index index)
                   (sprite sprite)
                   (x x)
                   (y y)
                   (next-action next-action)) character
    (let ((action next-action))
      (when action
        (setf next-action nil)
        (typecase action
          (function (funcall action character))
          (t (switchf sprite action)
           (setf index (animation-start-index sprite))))))
    (environment-bind (dx/frame dy/frame) (environment sprite)
      (multiple-value-bind (new-sprite new-index) (next-sprite-index sprite
                                                                     index)
        (setf sprite new-sprite)
        (setf index new-index))
      (tagbody
       retry
         (restart-case
             (let ((sx (etypecase dx/frame
                         (number dx/frame)
                         (simple-vector (svref dx/frame index))))
                   (sy (etypecase dy/frame
                         (number dy/frame)
                         (simple-vector (svref dy/frame index)))))
               (incf x (* sx (game-scale *game*)))
               (incf y (* sy (game-scale *game*))))
           (retry () :report "Retry computing speed"
             (go retry))
           (ignore () :report "Ignore move"))))))

;; (setf (sprite (hero *game*))
;;       calciumtrice:slime-green-idle-left )
;; (switchf (sprite (hero *game*)) '(wizard short))
;; (switchf (sprite (hero *game*)) 'rogue)
;; (decf (game-scale *game*))

(defmethod handle-event ((game forest-dungeon) (type (eql :keydown)) event)
  (with-key-down-event (event :keysym keysym :repeat repeat)
    (unless (plusp repeat)
      (when *debug*
        (format *trace-output* "~&>> ~a" (scancode keysym)))
      (handle-event game :scandown (scancode keysym)))))

(defmethod handle-event ((game forest-dungeon) (type (eql :keyup)) event)
  (with-key-down-event (event :keysym keysym :repeat repeat)
    (unless (plusp repeat)
      (handle-event game :scanup (scancode keysym)))))

(defgeneric trigger-keymap-entry (entry scancode mode)
  (:method ((not-found null) scancode mode))
  (:method ((entry delegate-keymap-entry) scancode mode)
    (handle-event (funcall (delegate-keymap-entry-accessor entry)
                           (keymap-entry-target entry))
                  mode
                  scancode))
  (:method ((entry action-keymap-entry) scancode mode)
    (declare (ignore scancode mode))
    (handle-event (keymap-entry-target entry)
                  :action
                  (action-keymap-entry-action entry)))
  (:method ((entry event-keymap-entry) scancode mode)
    (handle-event (keymap-entry-target entry)
                  (event-keymap-entry-event entry)
                  `(:from-keymap ,entry ,scancode ,mode))))

(defmethod handle-event (object (event-type (eql :action)) (event function))
  (funcall event object))

(defun resolve-keymap (key keymap modifier)
  (when-let (keymap (cdr (assoc modifier keymap)))
    (flet ((seek (k) (find-if (lambda (u) (or (eq u k) (eq u t)))
                              keymap
                              :key #'keymap-entry-key)))
      (or (seek key) (seek t)))))

(progn
  (defmethod handle-event ((object has-keymap)
                           (type (eql :scandown))
                           scancode)
    #1=(trigger-keymap-entry (resolve-keymap scancode
                                             (keymap object)
                                             type)
                             scancode
                             type))
  (defmethod handle-event ((object has-keymap)
                           (type (eql :scanup))
                           scancode)
    #1#))

(defun resolve-action (action alist)
  "Find first entry in alist that matches action (either T or action-key)"
  (cdr
   (find-if (lambda (entry)
              (or (eq entry action)
                  (eq entry t)))
            alist
            :key #'car)))

(defmethod handle-event ((character character) (type (eql :action)) action)
  (with-accessors ((sprite sprite) (next-action next-action)) character
    (environment-bind (switch) (environment sprite)
      (let ((action (resolve-action action switch)))
        (when action
          (setf next-action action))))))

(let* ((width 1200)
       (height 800)
       (*default-pathname-defaults*
         (asdf:system-relative-pathname :bricabrac
                                        "sdl2/sprites/")))
  (with-captured-bindings (rebind
                           *standard-output*
                           *trace-output*
                           *error-output*
                           *default-pathname-defaults*)
    (with-everything (:window (window :w width
                                      :h height
                                      :title "Forest")
                      :gl gl)
      (rebind
       (with-renderer (*renderer* window)
         (restart-case (game-loop *game*)
           (restart-game-loop ()
             :report "Restart game loop")))))))

;; (decf (game-scale *game*))
;; (switchf (sprite (hero *game*)) '(cleric))
;; (hero *game*)

(setf *debug* nil)

(switchf (sprite (hero *game*)) '(idle left))
(switchf (sprite (hero *game*)) '(wizard long))
(switchf (sprite (hero *game*)) '(goblin strong))
(switchf (sprite (hero *game*)) '(rogue))
(switchf (sprite (hero *game*)) '(ranger))
(switchf (sprite (hero *game*)) '(cleric))
(switchf (sprite (hero *game*)) '(minotaur))
(switchf (sprite (hero *game*)) '(bat))
(switchf (sprite (hero *game*)) '(death dying))

