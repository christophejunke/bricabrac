(defun slice (string start end)
  (make-array (- (or end (length string)) start)
              :element-type (array-element-type string)
              :displaced-to string
              :displaced-index-offset start))

(defun trim-bounds (bucket side string start end)
  (setf end (or end (length string)))
  (check-type side (member :both :right :left))
  (let ((bucketp (etypecase bucket
                   (null (return-from trim-bounds (values start end)))
                   (string (lambda (c) (find c bucket)))
                   (character (lambda (c) (char= c bucket)))
                   (cons (lambda (c) (member c bucket :test #'char=))))))
    (labels ((non-bucket-pos (from-end)
               (position-if-not bucketp
                                string
                                :from-end from-end
                                :start start
                                :end end)))
      (when (member side '(:both :left))
        (setf start (non-bucket-pos nil)))
      (if start
          (values start
                  (if (member side '(:both :right))
                      (1+ (non-bucket-pos t))
                      end))
          (values 0 0)))))

(defun trim (bucket string &optional (start 0) end (side :both))
  (multiple-value-bind (s e) (trim-bounds bucket side string start end)
    (slice string s e )))
