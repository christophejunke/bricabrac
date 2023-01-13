(cl-ansi-term:update-style-sheet '((:error :red)
				   (:trace :green)
				   (:term :yellow)
				   (:out :default)))

(defclass delegate-stream (sb-gray:fundamental-stream)
  ((stream :initarg :stream :reader wrapped-stream)))

(defclass locked-stream (delegate-stream)
  ((mutex :initarg :lock
	  :initform (sb-thread:make-mutex)
	  :reader mutex
	  :reader lock)))

(defclass styled-stream (locked-stream)
  ((style :initarg :style :reader style)
   (active :initform nil :accessor active)))

(macrolet ((delegate (gf (&rest args) (clf &rest clargs))
	     `(progn
		(defmethod ,gf ,(substitute '(s delegate-stream) '_ args)
		  (,clf ,@(substitute '(wrapped-stream s) '_ clargs)))
		(defmethod ,gf ,(substitute '(s locked-stream) '_ args)
		  (sb-thread:with-recursive-lock ((mutex s))
		    (call-next-method))))))
  (declare (sb-ext:muffle-conditions style-warning))
  (delegate interactive-stream-p (_) (interactive-stream-p _))
  (delegate sb-gray:stream-listen (_) (listen _))
  (delegate sb-gray:stream-terpri (_) (terpri _))
  (delegate sb-gray:stream-peek-char (_) (peek-char nil _))
  (delegate sb-gray:stream-read-byte (_) (read-byte _))
  (delegate sb-gray:stream-read-char (_) (read-char _))
  (delegate sb-gray:stream-read-line (_) (read-line _))
  (delegate sb-gray:stream-fresh-line (_) (fresh-line _))
  (delegate sb-gray:stream-write-byte (_ integer) (write-byte integer _))
  (delegate sb-gray:stream-write-char (_ character) (write-char character _))
  (delegate sb-gray:stream-clear-input (_) (clear-input _))
  (delegate sb-gray:stream-line-column (_) (sb-gray:stream-line-column _))
  (delegate sb-gray:stream-line-length (_) (sb-gray:stream-line-length _))
  (delegate sb-gray:stream-unread-char (_ character) (unread-char character _))
  (delegate sb-gray:stream-clear-output (_) (clear-output _))
  (delegate sb-gray:stream-force-output (_) (force-output _))
  (delegate sb-gray:stream-start-line-p (_) (sb-gray:stream-start-line-p _))
  (delegate sb-gray:stream-write-string (_ string &optional start end) (write-string string _ :start start :end end))
  (delegate sb-gray:stream-file-position (_ &optional position) (file-position _ position))
  (delegate sb-gray:stream-finish-output (_) (finish-output _))
  (delegate sb-gray:stream-read-sequence (_ seq &optional start end) (read-sequence seq _ :start start :end end))
  (delegate sb-gray:stream-write-sequence (_ seq &optional start end) (write-sequence seq _ :start start :end end))
  (delegate sb-gray:stream-advance-to-column (_ column) (sb-gray:stream-advance-to-column _ column))
  (delegate sb-gray:stream-read-char-no-hang (_) (read-char-no-hang _)))

(defmethod no-applicable-method
    ((m (eql #'sb-gray::stream-line-column)) &rest args)
  nil)

(defmacro with-active-style ((stream) &body body)
  (alexandria:once-only (stream)
    (alexandria:with-gensyms (w a old)
      `(let (,old
	     (,a (active ,stream))
	     (,w (wrapped-stream ,stream)))
	 (unless ,a
	   (setf (active ,stream) t)
	   (setf ,old cl-ansi-term::*style*)
	   (cl-ansi-term::set-style (style ,stream) ,w))
	 (unwind-protect (progn ,@body)
	   (unless ,a
	     (setf (active ,stream) nil)
	     (cl-ansi-term::set-style ,old ,w)))))))

(defmethod sb-gray:stream-write-string
    ((stream styled-stream) string &optional start end)
  (declare (ignore start end))
  (with-active-style (stream)
    (call-next-method)))

(defmethod sb-gray:stream-write-char
    ((stream styled-stream) character)
  (with-active-style (stream)
    (call-next-method)))
