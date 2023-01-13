(in-package :bricabrac.sdl2.sprites.textures)

(defparameter *active-textures* nil)

;; we map each spritesheet to a "textures" struct
(defstruct textures vector spritesheet)

(defstruct file-texture texture (uses 0))
(defstruct active-spritesheet vector spritesheet)
(defclass active-spritesheet-registry ()
  ((file-textures :initform (make-hash-table :test #'equal))
   (active-spritesheets :initform (make-hash-table :test #'eq))))

(defun make-textures-from-spritesheet (renderer spritesheet)
  (make-textures
   :spritesheet spritesheet
   :vector (map 'vector
                (lambda (f)
                  (let ((file (probe-file f)))
                    (assert file () "File not found: ~a" f)
                    (create-texture-from-surface
                     renderer
                     (sdl2-image:load-image file))))
                (spritesheet-texture-files spritesheet))))

(defun destroy-textures (textures)
  (ignore-errors
   (map nil #'destroy-texture (textures-vector textures)))
  (setf (textures-vector textures) nil))

(define-condition inactive-spritesheet (error)
  ((sprite-description :initarg :sprite-description
                       :accessor invalid-textures-sprite-description))
  (:report
   "Cannot instanciate sprite because its spritesheet is inactive."))

(define-condition invalid-textures (error)
  ((instance :initarg :instance :accessor invalid-textures-instance))
  (:report
   (lambda (err stream &aux (textures (invalid-textures-instance err)))
     (typecase textures
       (textures
        (format stream
                "Invalid TEXTURES instance~
                  ~:[ (the spritesheet is inactive)~
                   ~; even though the spritesheet is active (unexpected!)~]."
                (getf *active-textures* (textures-spritesheet textures))))
       (null (format stream "Invalid TEXTURES (nil instance is NIL)"))
       (symbol (format stream "Inactive spritesheet ~a" textures))))))

(defun check-textures-active (textures)
  (unless (and textures (textures-vector textures))
    (error 'invalid-textures :instance textures)))

(defun deactivate-textures-or-error (textures)
  (nth-value 1 (ignore-errors (destroy-textures textures))))

(defgeneric activate-spritesheet (renderer spritesheet)
  (:method (renderer (name symbol))
    (activate-spritesheet renderer (find-spritesheet name)))
  (:method (renderer (s spritesheet))
    (let ((name (spritesheet-name s)))
      (when (getf *active-textures* name)
        (warn "Already activated: ~a" name))
      (push (make-textures-from-spritesheet renderer s) *active-textures*)
      (push name *active-textures*))))

(defun call-with-active-spritesheets (renderer spritesheets fn)
  (let ((old *active-textures*)
        (*active-textures* *active-textures*)
        (spritesheets (ensure-list spritesheets)))
    (map nil (curry #'activate-spritesheet renderer) spritesheets)
    (unwind-protect (funcall fn)
      (loop
        with errors = nil
        for list on *active-textures* by #'cddr
        until (eq list old)
        do (let ((error (deactivate-textures-or-error (second list))))
             (when error
               (push error errors)))
        finally
           (when errors
             (cerror "CONTINUE" "Errors: ~S" (nreverse errors)))))))

(defun find-active-texture (spritesheet texture-id)
  (let ((textures (getf *active-textures* (spritesheet-name spritesheet))))
    (check-textures-active textures)
    (svref (textures-vector textures) texture-id)))

(defmacro with-active-spritesheets ((designator &key renderer) &body body)
  `(call-with-active-spritesheets ,renderer ,designator (lambda () ,@body)))

;; remove caches, let sprite-description knows all sprite instances (just a
;; list of source rects: no)?

;; (defclass sprite ()
;;   ((sprite-description
;;      :initarg :description
;;      :accessor sprite-description)
;;    (%flip
;;     :initarg :flip
;;     :accessor flip
;;     :initform nil)
;;    (%texture
;;     :accessor sdl2-texture
;;     :initform nil)
;;    (%rectangles
;;     :initarg :rectangles
;;     :accessor rectangles)
;;    (%timestamp
;;     :initarg :timestamp
;;     :accessor timestamp
;;     :initform nil)))

;; (defvar *default-sprite-class* (find-class 'sprite))

;; (defun make-sprite (sprite-description &optional (class 'sprite))
;;   (make-instance class
;;                  :description sprite-description
;;                  :rectangles (rectangles sprite-description)
;;                  :timestamp (timestamp sprite-description)
;;                  :flip (flip-p sprite-description)))

;; (defmethod (setf sprite-description) :after (new (sprite sprite))
;;   (maybe-refresh-sprite-cache sprite t))

;; (defun maybe-refresh-sprite-cache (sprite &optional force)
;;   (let* ((sprite-description (sprite-description sprite))
;;          (description-timestamp (timestamp sprite-description)))
;;     (with-slots (%rectangles %texture) sprite
;;       (unless (and (not force)
;;                    %rectangles
;;                    %texture
;;                    (eql (timestamp sprite) description-timestamp))
;;         (setf (flip sprite) (flip-p sprite-description))
;;         (setf (timestamp sprite) description-timestamp)
;;         (setf (rectangles sprite) (rectangles sprite-description))
;;         (setf (sdl2-texture sprite)
;;               (find-active-texture (spritesheet sprite-description)
;;                                    (texture-id sprite-description)))))))

;; (defmethod sdl2-texture :before ((sprite sprite))
;;   (maybe-refresh-sprite-cache sprite))

;; (defmethod rectangles :before ((sprite sprite))
;;   (maybe-refresh-sprite-cache sprite))



;; add caches, maybe a metaclass?



(declaim (inline sdl2-render))
(defun sdl2-render (renderer
                    sprite-description
                    source-rect
                    dest-rect)
  (environment-bind (flip) (environment sprite-description)
    (sdl2:render-copy-ex renderer
                         (find-active-texture (spritesheet sprite-description)
                                              (texture-id sprite-description))
                         :source-rect source-rect
                         :dest-rect dest-rect
                         :flip flip)))

(declaim (inline sdl2-render-index))
(defun sdl2-render-index (renderer sprite-description frame-index dest-rect)
  (sdl2-render renderer
               sprite-description
               (svref (rectangles sprite-description) frame-index)
               dest-rect))

;; (declaim (inline sdl2-render-sprite))
;; (defun sdl2-render-sprite-rect (renderer sprite source-rect dest-rect)
;;   (sdl2:render-copy-ex renderer
;;                        (sdl2-texture sprite)
;;                        :source-rect source-rect
;;                        :dest-rect dest-rect
;;                        :flip (flip sprite)))

;; (defun sdl2-render-sprite-index (renderer sprite frame-index dest-rect)
;;   (sdl2:render-copy-ex renderer
;;                        (sdl2-texture sprite)
;;                        :source-rect (svref (rectangles sprite) frame-index)
;;                        :dest-rect dest-rect
;;                        :flip (flip sprite)))
