;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and has been placed in
;;; the Public domain, and is provided 'as is'.
;;;
;;; **********************************************************************
;;;
;;; CMUCL specific W4 Web Walker client support.

(in-package "HTTP")

;;; Binary stream copy into a byte vector.
;;;
(defun stream-copy-into-8-bit-array (stream length)
  (declare (fixnum length)
	   (optimize (speed 3)))
  (let ((vector (make-array length :element-type '(unsigned-byte 8))))
    (dotimes (i length)
      (declare (fixnum i))
      (let ((byte (read-byte stream t)))
	(setf (aref vector i) byte)))
    vector))

;;; Binary stream copy until EOF into a byte vector.
;;;
(defun stream-copy-until-eof-into-8-bit-array (stream)
  (declare (optimize (speed 3)))
  (let ((res (make-array 1000 :element-type '(unsigned-byte 8)))
	(len 1000)
	(index 0))
    (declare (fixnum len index))
    (loop
     (let ((byte (read-byte stream nil nil)))
       (cond (byte
	      (when (= index len)
		(setq len (* len 2))
		(let ((new (make-array len :element-type '(unsigned-byte 8))))
		  (replace new res)
		  (setq res new)))
	      (setf (aref res index) byte)
	      (incf index))
	     (t
	      (setf (lisp::%array-fill-pointer res) index)
	      (return res)))))))

;;; Text or CRLF stream copy into a string.
;;;
(defun stream-copy-into-string (stream length)
  (declare (fixnum length)
	   (optimize (speed 3)))
  (let ((string (make-string length)))
    (dotimes (i length)
      (declare (fixnum i))
      (let ((char (read-char stream t)))
	(setf (aref string i) char)))
    string))

;;; Text or CRLF stream copy until EOF into a string.
;;;
(defun stream-copy-until-eof-into-string (stream)
  (declare (optimize (speed 3)))
  (let ((res (make-string 1000))
	(len 1000)
	(index 0))
    (declare (fixnum len index))
    (loop
     (let ((ch (read-char stream nil nil)))
       (cond (ch
	      (when (= index len)
		(setq len (* len 2))
		(let ((new (make-string len)))
		  (replace new res)
		  (setq res new)))
	      (setf (schar res index) ch)
	      (incf index))
	     (t
	      (setf (lisp::%array-fill-pointer res) index)
	      (return res)))))))

;;; Fast chunked transfer capture.
;;;
(defmethod chunked-input-capture (stream copy-mode headers)
  (declare (optimize (speed 3)))
  (with-chunked-transfer-decoding (stream :headers headers)
    (ecase copy-mode
      ((:text :crlf) (stream-copy-until-eof-into-string stream))
      (:binary (stream-copy-until-eof-into-8-bit-array stream)))))

;;; Modified version from client/w4-client.lisp, using the above
;;; function definitions, and implementing the flushing of reply
;;; bodies upon error giving more reliable chunk-transfer operation.
;;;
(defun %get-url-headers-and-body (url headers report-stream authorization)
  (handling-redirects (url)
    (with-http-request
	(url :get 
	     :request-headers (compute-standard-request-headers
			       url :authorization authorization
			       :header-plist headers
			       :user-agent (if (getf headers :user-agent)
					       nil
					       *server-version*)))
      (let ((status (client-status client))
	    (http-version (client-connection-version client))
	    (response-headers (client-response-headers client))
	    response-body redirection)
	(case status
	  ((200 205 206)
	   (let* ((content-type (get-header :content-type response-headers))
		  (copy-mode (mime-content-type-copy-mode content-type))
		  (content-length
		   (get-header :content-length response-headers))
		  (transfer-encoding
		   (get-header :transfer-encoding response-headers)))
	     (setq response-body
		   (cond (content-length
			  (ecase copy-mode
			    ((:text :crlf)
			     (stream-copy-into-string remote-stream
						      content-length))
			    (:binary
			     (with-binary-stream (stream :input)
			       (binary-stream-copy-into-8-bit-array
				remote-stream content-length)))))
			 ((member http-version '(:http/1.0 :http/0.9))
			  (ecase copy-mode
			    ((:text :crlf)
			     (stream-copy-until-eof-into-string remote-stream))
			    (:binary
			     (stream-copy-until-eof-into-8-bit-array
			      remote-stream))))
			 ((eq transfer-encoding :chunked)
			  (chunked-input-capture remote-stream copy-mode
						 response-headers))
			 (t (error 'server-not-implemented
				   :close-connection t :url url
				   :format-string "The HTTP transfer decoding, ~A, is not implemented."
				   :format-args (list transfer-encoding)))))))
	  ((201 202 203 204 300 402 403 405 406 407 415)
	   (flush-input-entity remote-stream response-headers http-version))
	  ((301 302)
	   (let ((alternate-urls
		  (mapcar #'url:intern-url
			  (ensure-list
			   (or (get-header :location response-headers) 
			       (get-header :content-location
					   response-headers))))))
	     (flush-input-entity remote-stream response-headers http-version)
	     (push alternate-urls redirection)
	     (signal (ecase status
		       (301 'document-moved-permanently)
		       (302 'document-moved-temporarily))
		     :new-urls alternate-urls :version http-version)))
	  ;; do something about authentication -- JCMa 12/10/1996.
	  (401
	   (destructuring-bind (&optional authentication-method . realm)
	       (get-header :WWW-Authenticate response-headers)
	     (declare (ignore authentication-method realm))
	     (flush-input-entity remote-stream response-headers http-version)
	     nil))
	  (404
	   (when *debug-client*
	     (fresh-line report-stream)
	     (%write-common-logfile-entry
	      (host-string url)
	      (concatenate 'string (url:name-string url) " GET")
	      status 0 "-" *log-times-in-gmt* report-stream))
	   (flush-input-entity remote-stream response-headers http-version))
	  ((nil) (setq status 408))		; didn't return a status code
	  ((408 411 414 500 501 502 503 504 505)
	   (flush-input-entity remote-stream response-headers http-version))
	  (t (client-signal-http-code
	      url status :get
	      :headers response-headers
	      :reason (client-reason client)
	      :version http-version)))
	(values response-body (durable-response-headers client) status redirection
		http-version)))))
