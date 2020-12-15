;;;
;;; **********************************************************************
;;; This code was written by Douglas T. Crosher and has been placed in
;;; the Public domain, and is provided 'as is'.
;;;
;;; **********************************************************************
;;;
;;; FTP file transfer support.
;;;  Only passive mode operation is currently supported.

(in-package "WWW-UTILS")

(defparameter *debug-ftp* nil)

(defconstant *ftp-data-port* 20)
(defconstant *ftp-control-port* 21)

(defun read-ftp-response (stream expected-responses)
  "Read a reply from the FTP server, checking the response against a given
  response code or list of valid responses. The response code and response
  string are returned, with only the last line of multi-line responses
  being returned."
  (declare (type (or fixnum list) expected-responses))
  (finish-output stream)
  (let* ((*read-eval* nil)
	 (response-text "")
	 (response nil)
	 (len 0))
    (declare (type simple-base-string response-text)
	     (type (or null fixnum) response)
	     (type fixnum len))
    (loop
     (setq response-text (read-line stream))
     (setq len (length response-text))

     ;; Cleanup trailing CR.
     (when (and (plusp len) (char= (aref response-text (1- len)) #\Return))
       (setf (aref response-text (1- len)) #\Space))
     (when *debug-ftp*
       (format t "FTP: ~A~%" response-text))

     (cond ((null response)
	    ;; Response start
	    (setq response
		  (and (>= len 3)
		       (parse-integer response-text :start 0 :end 3
				      :junk-allowed t)))
	    (unless (and response (>= len 4)
			 (char= (aref response-text 3) #\-))
	      (return)))
	   (t
	    ;; Continuation.
	    (when (and (>= len 4)
		       (eql (parse-integer response-text :start 0 :end 3
					   :junk-allowed t)
			    response)
		       (char= (aref response-text 3) #\Space))
	      (return)))))
    
    (when (or (null response)
	      (and (consp expected-responses)
		   (not (member response expected-responses)))
	      (and (typep expected-responses 'fixnum)
		   (not (eql response expected-responses))))
      (error "Unexpected FTP response, expected ~s but received:~%~4T~A"
	      expected-responses response-text))
    (values response response-text)))

#+CMU
(defun open-ftp-stream (host port)
  (let* ((fd (ext:connect-to-inet-socket host port))
	 (stream (system:make-fd-stream fd :input t :output t)))
    (read-ftp-response stream 220)
    stream))

(defmacro with-ftp-stream ((stream host
			     &optional (user "anonymous") (password "")
			     &key (port *ftp-control-port*))
			   &body body)
  `(with-open-stream (,stream (open-ftp-stream ,host ,port))
     (write-string "USER " ,stream)
     (write-string ,user ,stream)
     (write-char #\Return ,stream)
     (write-char #\Newline ,stream)
     (read-ftp-response ,stream 331)
     (write-string "PASS " ,stream)
     (write-string ,password ,stream)
     (write-char #\Return ,stream)
     (write-char #\Newline ,stream)
     (read-ftp-response ,stream 230)
     ,@body))

(defun open-ftp-passive-stream (stream &key (input t) (output t))
  "Open a passive data stream, returning the data stream."
  (write-string "PASV" stream)
  (write-char #\Return stream)
  (write-char #\Newline stream)
  (multiple-value-bind (response text)
      (read-ftp-response stream 227)
    (declare (ignore response))
    (let ((start (position-if #'digit-char-p text :start 4)))
      (multiple-value-bind (a1 start)
	  (parse-integer text :start start :junk-allowed t)
	(declare (type (or null (mod 256)) a1)
		 (fixnum start))
	(multiple-value-bind (a2 start)
	    (parse-integer text :start (1+ start) :junk-allowed t)
	  (declare (type (or null (mod 256)) a2)
		   (fixnum start))
	  (multiple-value-bind (a3 start)
	      (parse-integer text :start (1+ start) :junk-allowed t)
	    (declare (type (or null (mod 256)) a3)
		     (fixnum start))
	    (multiple-value-bind (a4 start)
		(parse-integer text :start (1+ start) :junk-allowed t)
	      (declare (type (or null (mod 256)) a4)
		       (fixnum start))
	      (multiple-value-bind (port-hi start)
		  (parse-integer text :start (1+ start) :junk-allowed t)
		(declare (type (or null (mod 256)) port-hi)
			 (fixnum start))
		(multiple-value-bind (port-lo start)
		    (parse-integer text :start (1+ start) :junk-allowed t)
		  (declare (type (or null (mod 256)) port-lo)
			   (ignore start))
		  (unless (and port-hi port-lo)
		    (error "FTP Error reading the port from the following response:~%~4T~A"
			   text))
		  (let ((address
			 (logior (ash a1 24) (ash a2 16) (ash a3 8) a4))
			(port (logior (ash port-hi 8) port-lo)))
		    ;; Open the data stream.
		    (let* ((fd (ext:connect-to-inet-socket address port))
			   (stream (system:make-fd-stream
				    fd :input input :output output)))
		      (when *debug-ftp*
			(format t "Passive connection to host ~D.~D.~D.~D ~
				   port ~D, on FD ~D~%"
			      a1 a2 a3 a4 port fd))
		      stream)))))))))))

(defmacro with-ftp-data-stream ((data-stream control-stream
				 operation direction &optional file-spec
				 (type "I"))
				&body body)
  "Open a passive FTP data-stream with the given direction to the server on
  the control-stream, sending the server the operation with an optional
  file-spec. The direction must be either :input or :output. The type of
  transfer defaults to, bInary, and may alternatively be Ascii."
  (let ((transfer-started (gensym))
	(abortp (gensym)))
    `(let ((,data-stream nil)
	   (,transfer-started nil)
	   (,abortp t))
      (write-string "TYPE " ,control-stream)
      (write-string ,type ,control-stream)
      (write-char #\Return ,control-stream)
      (write-char #\Newline ,control-stream)
      (read-ftp-response ,control-stream 200)
      (unwind-protect
	   (multiple-value-prog1
	       (progn
		 (setq ,data-stream
		       (open-ftp-passive-stream
			,control-stream
			:input ,(eq direction :input)
			:output ,(eq direction :output)))
		 (write-string ,operation ,control-stream)
		 (when ,file-spec
		   (format ,control-stream " ~A" ,file-spec))
		 (write-char #\Return ,control-stream)
		 (write-char #\Newline ,control-stream)
		 (read-ftp-response ,control-stream 150)
		 (setq ,transfer-started t)
		 ,@body)
	     (setq ,abortp nil))
	(when ,data-stream
	  (close ,data-stream :abort ,abortp)
	  (when ,transfer-started
	    (read-ftp-response ,control-stream 226)))))))

;;; Example performing some directory LISTing, GET, and PUT
;;; operations.
;;;
#+nil
(defun tst ()
  (flet ((read-until-eof (data-stream)
	   (loop
	    (let ((text (read-line data-stream nil nil)))
	      (when (null text)
		(return))
	      (write-line text)))))
  (with-ftp-stream (stream "dtc-pc2" "ftp")
    (with-ftp-data-stream (data-stream stream "LIST" :input "/tmp/")
      (read-until-eof data-stream))
    (with-ftp-data-stream (data-stream stream "STOR" :output "aaa")
      (format data-stream "Test~%"))
    (with-ftp-data-stream (data-stream stream "LIST" :input)
      (read-until-eof data-stream))
    (with-ftp-data-stream (data-stream stream "RETR" :input "aaa")
      (read-until-eof data-stream))
    )))
