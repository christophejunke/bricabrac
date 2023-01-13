(in-package :bricabrac.sdl2.event-loop)

(defun sdl2-ffi-windowevents ()
  (flet ((keywordize (symbol &aux (name (string symbol)))
           (make-keyword
            (concatenate 'string
                         (string :WINDOWEVENT-)
                         (subseq name
                                 #.(length #1="+SDL-WINDOWEVENT-")
                                 (1- (length name)))))))
    (let ((windowevent-constants)
          (size 0))
      (do-external-symbols (s (find-package "SDL2-FFI"))
        (let ((name (symbol-name s)))
          (when (search #1# name)
            (incf size)
            (push (cons (symbol-value s) (keywordize s))
                  windowevent-constants))))
      (let ((array (make-array size :initial-element nil)))
        (loop
           for (index . keyword) in windowevent-constants
           do (assert (null (aref array index)))
             (setf (aref array index) keyword)
           finally (assert (notany #'null array)))
        (coerce array `(simple-array  keyword (,size)))))))

(let ((keywords (sdl2-ffi-windowevents)))
  (defun windowevent (code)
    "Return the keyword corresponding to a windowevent code"
    (aref keywords code))
  
  (deftype windowevent ()
    `(member
      ,@(coerce keywords 'list))))
