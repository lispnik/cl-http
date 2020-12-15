
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Patches to server/server.lisp

(in-package :http)

(defun %%write-binary-template-file (pathname url content-type stream &optional charset)
  (let ((template-parameters (url::template-parameters url)))
    (cond (template-parameters
           (with-open-file (file-stream pathname :direction :input #-cmu :element-type #-cmu '(unsigned-byte 8))
             (let ((expires (expiration-universal-time url))
                   (cache-control (url:response-cache-control-directives url))
                   (languages (languages url)))
               (with-successful-response (stream content-type :status :success
                                                 :last-modification (get-universal-time)
                                                 :character-set charset
                                                 :expires expires
                                                 :cache-control cache-control
                                                 :content-location url
                                                 :content-language languages
                                                 :termination-line-p t)
                 (with-binary-stream (stream :output)
                   (loop for (start end function . param-alist) in template-parameters
                         for n-bytes = (unless (eql start end) (- end start))
                         when n-bytes
                           do (file-position file-stream start)
                              (stream-copy-bytes file-stream stream n-bytes)
                         when function
                           do (funcall function url stream param-alist)))))))
          (t (%%write-binary-file pathname url content-type stream charset)))))

(defun %%write-binary-file (pathname url content-type stream &optional charset last-modification version)
  (with-open-file (file-stream pathname :direction :input #-CMU :element-type #-CMU '(unsigned-byte 8))
    (let ((resource-length (file-stream-length-in-bytes file-stream)))
      (unless-every
        (last-modification (setq last-modification (file-stream-modification-date file-stream)))
        (version (setq version (file-stream-version file-stream))))
      (%writing-binary-file
        (stream url content-type resource-length last-modification version :charset charset)
        (stream-copy-byte-range file-stream stream start end)
        (stream-copy-until-eof file-stream stream :binary)))))

(defun %%write-binary-range (url pathname content-type stream start end &optional charset last-modification version)
  (with-open-file (file-stream pathname :direction :input #-CMU :element-type #-CMU '(unsigned-byte 8))
    (unless-every
      (last-modification (setq last-modification (file-stream-modification-date file-stream)))
      (version (setq version (file-stream-version file-stream))))
    (%writing-write-binary-range
      (stream url content-type last-modification version :charset charset :start start :end end)
      (stream-copy-byte-range file-stream stream start end))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Patches to server/utils.lisp

(in-package :http)

(define copy-mode-element-type (copy-mode)
  "Returns the element type associated with COPY-MODE."
  (ecase copy-mode
    ((:binary :crlf) #-cmu '(unsigned-byte 8) #+cmu *standard-character-type*)
    (:text *standard-character-type*)))

(defmethod stream-copy-until-eof (from-stream to-stream &optional (copy-mode :text))
  (declare (optimize (speed 3)))
  (ecase copy-mode
    (:text
      (with-text-stream  (from-stream :input)
	(with-text-stream (to-stream :output)
	  (loop for line = (read-line from-stream nil)
		while line
		do (write-line line to-stream)))))
    ((:binary :crlf)
     (with-binary-stream (from-stream :input)
       (with-binary-stream (to-stream :output)
	 (loop for char = (read-char from-stream nil)
	       while char
	       do (write-char char to-stream)))))))

(defmethod advance-input-buffer (stream &optional delta)
  (if delta
      (dotimes (i delta)
	(read-char stream t))
      (loop while (read-char stream nil nil))))

(defmethod stream-encode-crlf-until-eof (from-stream to-stream)
  (declare (optimize (speed 3)))
  (using-resource (line-buffer line-buffer *line-buffer-size*)
    (loop with line and eof and delimiter and length fixnum
          do (multiple-value-setq (line eof delimiter length)
               (read-delimited-line from-stream '(#\Return #\Linefeed) nil line-buffer))
          unless (zerop length)
            do (with-fast-array-references ((array line vector))
                 delimiter                      ;ignore
                 (loop for idx fixnum upfrom 0 below length
                       do (write-char (aref array idx) to-stream)))
          do (write-char #\Return to-stream)
             (write-char #\Linefeed to-stream)
          until eof)))

(defmethod crlf-canonicalize-file (pathname &optional destination-pathname &aux source destination)
  (if destination-pathname
      (setq source pathname
            destination destination-pathname)
      (setq source (probe-file pathname)
            destination (crlf-pathname source)))
  (with-open-file (file source :direction :input :element-type *standard-character-type* :if-does-not-exist :error)
    (with-open-file (to-stream destination :direction :output #-CMU :element-type #-CMU '(unsigned-byte 8)
                               :if-exists :supersede :if-does-not-exist :create)
      (stream-encode-crlf-until-eof file to-stream)
    ;; return the true pathname
    (truename to-stream))))

(defmethod stream-copy-byte-range (from-stream to-stream start end)
  (declare (fixnum end)
	   (optimize (speed 3)))
  (cond ((file-position from-stream start)
	 (with-binary-stream (to-stream :output)
	   (loop for bytes fixnum upfrom start below end
		 while (< bytes end)
		 do (write-char (read-char from-stream) to-stream))))
	(t (error "Unable to set file position for byte range copy."))))

(defmethod stream-copy-bytes (from-stream to-stream n-bytes &optional (copy-mode :binary))
  (declare (fixnum n-bytes)
	   (optimize (speed 3)))
  (ecase copy-mode
    ((:binary :crlf)
     (loop for bytes fixnum upfrom 0
           while (< bytes n-bytes)
           do (write-char (read-char from-stream t) to-stream)))
    (:text
      (loop for bytes fixnum upfrom 0
            while (< bytes n-bytes)
            do (write-char (read-char from-stream t) to-stream)))))

(defmethod write-vector (stream (vector vector) &optional (start 0) (end (length vector)))
  (declare (optimize (speed 3)))
  (with-fast-array-references ((v vector vector))
    (loop for idx fixnum upfrom start below end
          do (write-char (character (aref v idx)) stream)))
  vector)

(defmethod write-vector (stream (vector string) &optional (start 0) (end (length vector)))
  (declare (optimize (speed 3)))
  (with-fast-array-references ((v vector vector))
    (loop for idx fixnum upfrom start below end
          do (write-char (aref v idx) stream)))
  vector)


;;;

(define gif-image-size (stream &optional (error-p t)
			       &aux color-table-flags global-color-table-p (global-color-table-size 0)
			       (nextbyte 0) (width 0) (height 0) (acceptable-headers '("GIF87a" "GIF89a")))
  "Returns WIDTH and HEIGHT for the first image encountered in a GIF data stream. 
   Assumes stream is positioned at beginning of gif data.
   For invalid GIF data, when ERROR-P it throws an error, otherwise
     it returns null values."
  (declare (fixnum global-color-table-size nextbyte width height)
	   (optimize (speed 3)))
  (let ((startpos (file-position stream))
        (test-header (make-array 6 :element-type *standard-character-type*))
        (image-descriptor-found nil))
    (declare (fixnum startpos))
    (loop for idx fixnum upfrom 0 below 6
          do (setf (aref test-header idx) (read-char stream nil nil)))
    (cond
      ((member test-header acceptable-headers :test #'equalp)
       (file-position stream (+ startpos 10))   ; set up to read color table flags
       (setf color-table-flags (char-code (read-char stream nil nil)))
       (setf global-color-table-p (logbitp 7 color-table-flags))        ; global color table present?
       (when global-color-table-p 
	 (setf global-color-table-size (* 3 (expt 2 (1+ (logand 7 color-table-flags))))))
       (file-position stream (+ startpos 13 global-color-table-size))
       ;; now we could have an AE, a CE, a PTE, a GCB, a GCE, or an Image Descriptor
       (setf nextbyte (char-code (read-char stream nil nil)))       ; is all you need
       (loop until image-descriptor-found
	     do (case nextbyte
		  (#x21				; it's an extension
		   (setf nextbyte (char-code (read-char stream nil nil)))
		   (case nextbyte
		     (#xF9			; Graphic control extension
		      (file-position stream (+ 6 (file-position stream)))
		      (setf nextbyte (char-code (read-char stream nil nil))))
		     ;; Barring a rare Plain Text extension, the next block will be our image descriptor.
		     ;;  But just to be absolutely sure, we'll do the outer loop again.
		     (#xFE			; Comment extension
		      (loop for byte = (char-code (read-char stream nil nil))
			    until (eql byte 0)
			    do (file-position stream (+ byte (file-position stream))))
		      (setf nextbyte (char-code (read-char stream nil nil))))
		     (#xFF			; Application extension
		      (file-position stream (+ (char-code (read-char stream nil nil))
					       (file-position stream)))
		      (loop for byte = (char-code (read-char stream nil nil))
			    until (eql byte 0)
			    do (file-position stream (+ byte (file-position stream))))
		      (setf nextbyte (char-code (read-char stream nil nil))))
		     ;; Other extensions, like Plain Text (#x01) seem to
		     ;;  be extremely rarely used and thus we aren't handling them for now.
		     (t (if error-p
			    (error "Unknown extension found, ~X, in GIF data stream, ~S. Please report the problem."
				   nextbyte stream)
			    (return-from gif-image-size (values nil nil t))))))
		  (#x2C				; It's the Image Descriptor
		   (file-position stream (+ 4 (file-position stream)))
		   (setf image-descriptor-found t))
		  (t (if error-p
			 (error "Invalid GIF format. Byte header of ~X found." nextbyte)
			 (return-from gif-image-size (values nil nil t))))))
       (setf width (char-code (read-char stream nil nil)))  ; LS Byte is first
       (incf width (* 256 (char-code (read-char stream nil nil))))
       (setf height (char-code (read-char stream nil nil))) ; LS Byte is first
       (incf height (* 256 (char-code (read-char stream nil nil))))
       (values width height t))
      (error-p (error "~S is not a GIF data stream." stream))
      (t (values nil nil nil))))) 

(define gif-image-dimensions (pathname &optional (error-p t))
  "Returns HEIGHT and WIDTH for the first image encountered in a GIF data stream. 
   For an invalid GIF file, when ERROR-P it throws an error, otherwise
     it returns null values."
  (declare (optimize (speed 3))
	   (values width height gif-image-p))
  (with-open-file (stream pathname :direction :input #-cmu :element-type #-cmu '(unsigned-byte 8))
    (gif-image-size stream error-p)))

(define jpeg-image-size (stream &optional (error-p t) &aux (file-length (file-length stream)))
  "Returns WIDTH and HEIGHT for JPEG encountered in data stream. 
   Assumes stream is positioned at beginning of JPEG data."
  (declare (fixnum file-length)
	   (optimize (speed 3)))
  (labels ((read-two-bytes (stream)
             (+ (ash (char-code (read-char stream nil nil)) 8)
                (char-code (read-char stream nil nil))))
           (get-next-type (stream error-p)
             (let ((lead (char-code (read-char stream nil nil)))
                   (next (char-code (read-char stream nil nil))))
               (cond ((and lead (= lead #xff)) next)
		     (error-p (error "~X is not a mark for JPEG stream ~A" lead stream))
		     (t (return-from jpeg-image-size (values nil nil t))))))
           (image-block-p (type)
             (and type (<= #xc0 type #xcf) (/= type #xc4) (/= type #xcc)))
           (get-size-from-image-block (stream)
             (file-position stream (+ (file-position stream) 3))
             (let ((height (read-two-bytes stream))
                   (width (read-two-bytes stream)))
               (values width height t)))
           (skip-block (stream type error-p)
             (unless (= type #xd9)
	       (let* ((length (read-two-bytes stream))
		      (new-length (+ length -2 (file-position stream))))
		 (declare (fixnum length new-length))
		 (file-position stream (+ length -20 (file-position stream)))
		 (cond ((<= new-length file-length)
			(file-position stream new-length))
		       (error-p (error "Wrong format in JPEG stream: ~S" stream))
		       (t (return-from jpeg-image-size (values nil nil t))))))))
  (let ((first-mark (get-next-type stream error-p)))
    (cond ((and first-mark (= first-mark #xd8))
	   (loop for type = (get-next-type stream error-p)
		 while type
		 when (image-block-p type)
		   do (return (get-size-from-image-block stream))
		 else do (skip-block stream type error-p)))
	  (error-p (error "First mark in JPEG stream is ~X and ~A not #xd8." first-mark stream))
	  (t (values nil nil nil))))))

(define jpeg-image-dimensions (pathname &optional (error-p t))
  "Returns WIDTH and HEIGHT for the JPEG image. 
   For an invalid JPEG file, when ERROR-P it throws an error, otherwise
     it returns null values."
  (declare (values width height))
  (with-open-file (stream pathname :direction :input #-cmu :element-type #-cmu '(unsigned-byte 8))
    (jpeg-image-size stream error-p)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Patches to server/shtml.lisp

(in-package :http)

(defmethod parse-shtml-template (pathname)
  (declare (values template-parameters))
  (labels ((char-to-ascii (char) (char-code char))      ;UNIX, Win32
           (ascii-to-char (byte) (code-char byte))      ;UNIX, Win32
           (string-to-8-bit-vector (string)
             (map '(simple-array (unsigned-byte 8) 1) #'char-to-ascii string)))
    (declare (inline char-to-ascii ascii-to-char))
    (let* ((+shtml-tag-start+ (string-to-8-bit-vector "<!--#"))
           (+shtml-tag-start-length+ (length +shtml-tag-start+))
           (+shtml-tag-end+ (string-to-8-bit-vector "-->"))
           (+shtml-tag-end-length+ (length +shtml-tag-end+)))
      (with-open-file (file-stream pathname :direction :input)
        (let* ((length (file-length file-stream))
               (data (make-array length :element-type '(unsigned-byte 8)))
               (positions (make-array length)))
          (dotimes (index length)
            (setf (svref positions index) (file-position file-stream)
                  (aref data index) (char-code (read-char file-stream))))
          (let ((end-position (file-position file-stream))
                (read-start 0)
                (template-parameters '()))
            (loop (let ((tag-start (search +shtml-tag-start+ data :start2 read-start)))
                    (unless tag-start
                      (let ((read-end end-position))
                        (unless (eql read-start read-end)
                          (push (list read-start read-end) template-parameters))
                        (return)))
                    (let ((tag-end (search +shtml-tag-end+ data :start2 (+ tag-start +shtml-tag-start-length+))))
                      (unless tag-end
                        (error "Unbalanced element at byte ~D" read-start))
                      (incf tag-end +shtml-tag-end-length+)
                      (let* ((string-length (- tag-end tag-start))
                             (string (make-string string-length)))
                        (dotimes (string-index string-length)
                          (let ((char (ascii-to-char (aref data (+ tag-start string-index)))))
                            (setf (schar string string-index)
                                  (if (member char '(#\Return #\Newline))
                                      #\Space
                                      char))))
			(multiple-value-bind (function parameter-plist)
			    (http::parse-shtml-element string)
			  (push (list* read-start (aref positions tag-start)
				       function parameter-plist)
				template-parameters)))
                      (setq read-start tag-end))))
            (nreverse template-parameters)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;; Patches to w3p/w3p-system.lisp

(in-package :www-present)

(defmethod get-presentation-type ((class pcl::standard-class) &optional (errorp t))
  "get an exact match for a presentation-type from a class"
  (get-presentation-type (pcl:class-name class) errorp))

(defmethod get-presentation-type ((class pcl:built-in-class) &optional (errorp t))
  "get an exact match for a presentation-type from a class"
  (get-presentation-type (pcl:class-name class) errorp))

