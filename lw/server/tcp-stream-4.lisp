;;; LispWorks 4 interface to TCP/IP streams
;;;
;;; Copyright (C) 1997-1998 Harlequin Group plc.  All rights reserved.
;;;


(in-package "IPC")

;;; This is the condition hierarchy, based on the LispM one.
;;; network-error
;;;   domain-resolver-error
;;;   local-network-error
;;;     network-resources-exhausted
;;;     unknown-address
;;;     unknown-host-name
;;;   remote-network-error
;;;     bad-connection-state
;;;       connection-closed
;;;       connection-lost
;;;       host-stopped-responding
;;;     connection-error
;;;       connection-refused
;;;     host-not-responding
;;;     protocol-timeout
;;; network-parse-error


(define-condition www-utils:network-error (simple-error)
  ()
  (:report (lambda (condition stream)
             (let ((control (simple-condition-format-control condition)))
               (if control
                   (apply 'format stream
                          control
                          (simple-condition-format-arguments condition))
                 (format stream "A network error of type ~S occurred."
                         (type-of condition))))))
  (:default-initargs
   :format-control nil
   :format-arguments nil))

(define-condition www-utils:domain-resolver-error (www-utils:network-error)
  ((address :initarg :address))
  (:report (lambda (condition stream)
             (if (slot-boundp condition 'address)
	         (format stream "Cannot resolve IP address ~A"
                         (ip-address-string (slot-value condition 'address)))
	       (format stream "Cannot find current domainname")))))

(define-condition www-utils:local-network-error (www-utils:network-error)
  ())

(define-condition www-utils:unknown-host-name (www-utils:local-network-error)
  ((hostname :initarg :hostname))
  (:report (lambda (condition stream)
	     (format stream "Unknown host name ~A"
		     (slot-value condition 'hostname)))))

#+comment ; MJS 07Oct97: not used
(define-condition www-utils:unknown-address (www-utils:local-network-error)
  ((address :initarg :address))
  (:report (lambda (condition stream)
	     (let ((address (slot-value condition 'address)))
	       (format stream "Unknown address ~A"
		       (ip-address-string address))))))

(define-condition www-utils:remote-network-error (www-utils:network-error)
  ())

(define-condition www-utils:bad-connection-state (www-utils:remote-network-error)
  ())

(define-condition www-utils:connection-closed (www-utils:bad-connection-state)
  ())

(define-condition www-utils:connection-lost (www-utils:bad-connection-state)
  ())

(define-condition www-utils:host-stopped-responding (www-utils:bad-connection-state)
  ())

(define-condition www-utils:connection-error (www-utils:remote-network-error)
  ())

(define-condition www-utils:connection-refused (www-utils:connection-error)
  ())

(define-condition www-utils:host-not-responding (www-utils:remote-network-error)
  ())

(define-condition www-utils:protocol-timeout (www-utils:remote-network-error)
  ())

(define-condition www-utils:network-error-mixin
		  (www-utils:network-error)
  ()
  (:documentation "Mixin to allow ports to inherit instance variables and methods to network conditions
defined at the portable code level."))

(define-condition connection-timed-out (www-utils:protocol-timeout www-utils:bad-connection-state)
  ()
  (:report (lambda (condition stream)
             (declare (ignore condition))
	     (format stream "Connection timed out."))))



;;; ----------------------------------------------------------------------------------
;;; Network address code

;;; Turn internet address into string format
(defun ip-address-string (address)
  (comm:ip-address-string address))

(defun string-ip-address (address-string)
  (let ((address (comm:string-ip-address address-string)))
    #+LispWorks4.0
    (when (eql address #xffffffff)
      (setq address nil))
    address))

(defun internet-address (name)
  (or (comm:get-host-entry name :fields '(:address))
      (error 'www-utils:unknown-host-name :hostname name)))


#+unix
(fli:define-foreign-function (c-getdomainname getdomainname)
                             ((name :pointer)
                              (namelen :int))
                             :result-type :int)

(defvar *domainname* nil)

;;; There are more than one strategies you can use
;;; based on what OS you are using. Instead of providing
;;; each one we try one.
;;;
(defun getdomainname (&optional (where "/etc/defaultdomain"))
  (or *domainname*
      #+unix
      (fli:with-dynamic-foreign-objects ((buffer (:ef-mb-string
						  :external-format :ascii
						  :limit 256)))
	(and (eql (c-getdomainname buffer 256) 0)
             (setq *domainname* (fli:convert-from-foreign-string buffer))))
      ;; Try using DNS name lookup: on some machines this 
      (let* ((self-name (comm:get-host-entry (machine-instance)
					     :fields '(:name)))
	     (dot (and self-name (position #\. self-name))))
	(and dot (subseq self-name (1+ dot))))
      (if (probe-file where)
	  (with-open-file (stream where :direction :input)
	    (setq *domainname* (read-line stream))))
      (error 'www-utils:domain-resolver-error)))


(defun get-host-name-by-address (address)
  (or (comm:get-host-entry address :fields '(:name))
      (error 'www-utils:domain-resolver-error :address address)))



;;; ----------------------------------------------------------------------------------
;;; Stream code

(defclass cl-http-socket-stream (comm:socket-stream)
  ((local-port :initarg :local-port :accessor www-utils:local-port)
   (foreign-host :initarg :foreign-host :accessor www-utils:foreign-host)
   (foreign-port :initarg :foreign-port :accessor www-utils:foreign-port)
   (bytes-received :initform 0 :accessor www-utils:bytes-received)
   (bytes-transmitted :initform 0 :accessor www-utils:bytes-transmitted)
   ))

(defclass cl-http-server-socket-stream (cl-http-socket-stream)
  ((chunk-start-index)
   (chunk-function)
   (chunks-transmitted :initform nil)
   ))

(defclass cl-http-client-socket-stream (cl-http-socket-stream)
  ((chunk-length-received)
   (chunk-remaining-bytes :initform nil)
   (chunk-real-buffer-limit)))

(defmethod stream:stream-read-buffer ((stream cl-http-socket-stream)
                                      buffer start end)
  (declare (ignore buffer start end))
  (with-slots (bytes-received) stream
    (let ((len (call-next-method)))
      (declare (fixnum len))
      (when (> len 0)
	(setf bytes-received (the fixnum (+ (the fixnum bytes-received) len))))
      len)))

(defmethod stream:stream-write-buffer :after ((stream cl-http-socket-stream)
					      buffer start end)
  (declare (ignore buffer)
	   (fixnum start end))
  (with-slots (bytes-transmitted) stream
    (setf bytes-transmitted
	  (the fixnum (+ (the fixnum bytes-transmitted)
			 (the fixnum (- end start)))))))

(defun make-tcp-stream (socket-handle local-port)
  (multiple-value-bind (foreign-host foreign-port)
      (comm:get-socket-peer-address socket-handle)
    (if foreign-host
        (let ((stream (make-instance 'cl-http-server-socket-stream
				     :socket socket-handle
				     :direction :io
				     :element-type 'base-char
				     :local-port local-port
				     :foreign-host foreign-host
				     :foreign-port foreign-port)))
          stream)
      (error 'www-utils:network-error))))

(defun %listen-for-connections (port function)
  (comm::listen-and-attach-stream `(lispworks-listen-for-connection
                                    ,port
                                    ,function)
                                  port ;service
                                  nil  ;announce
                                  ))

(defun lispworks-listen-for-connection (fd port function)
  (let ((stream (make-tcp-stream fd port)))
    (when stream
      (funcall function stream  port))))

(defun http::%open-http-stream-to-host (host port timeout)
  #+LispWorks4.0 (declare (ignore timeout))
  (let ((socket-handle nil))
    (unwind-protect
	(progn
	  (setq socket-handle (comm:connect-to-tcp-server host port :errorp nil
							  #-LispWorks4.0 :timeout
							  #-LispWorks4.0 timeout))
	  (if socket-handle
	      (make-instance 'cl-http-client-socket-stream
			     :socket (shiftf socket-handle nil)
			     :direction :io
			     :element-type 'base-char
			     :foreign-host host
			     :foreign-port port)
	    (error 'www-utils:connection-error)))
      (when socket-handle
	(comm::close-socket socket-handle)))))

(defclass smtp-stream (cl-http-socket-stream)
  ((newline-p :initform t)
   (body-p :initform nil)))

(defun www-utils:tcp-service-port-number (protocol &optional error-p)
  "Returns the service port number for the TCP protocol denoted by protocol.
PROTOCOL is a keyword,, but integer and string are also supported."
  (etypecase protocol
    (integer protocol)
    (keyword (tcp-service-port-number-aux (string-downcase protocol) error-p))
    (string (tcp-service-port-number-aux protocol error-p))))

(defun tcp-service-port-number-aux (service-name error-p)
  (let ((port (comm::get-port-for-service service-name "tcp")))
    (cond (port (comm::ntohs port))
	  (error-p (error "Unknown TCP service name ~S" service-name)))))

(declaim (inline smtp::%open-mailer-stream))
(defun smtp::%open-mailer-stream (host port args)
  (let ((socket-handle nil))
    (unwind-protect
	(progn
	  (setq socket-handle (comm:connect-to-tcp-server host port
							  :errorp nil))
	  (if socket-handle
	      (apply 'make-instance 'smtp-stream
		     :socket (shiftf socket-handle nil)
		     :direction :io
		     :element-type 'base-char
		     :foreign-host host
		     :foreign-port port
		     args)
	    (error 'www-utils:connection-error)))
      (when socket-handle
	(comm::close-socket socket-handle)))))

(defmacro smtp::with-message-body-encoding ((stream output-stream) &body body)
  `(let ((,stream ,output-stream))
     (unwind-protect
	 (progn
	   (setf (slot-value ,stream 'body-p) t)
	   . ,body)
       (setf (slot-value ,stream 'body-p) nil))))

(defmethod stream:stream-write-char ((stream smtp-stream) char)
  (with-slots (newline-p body-p) stream
    ;; Convert dot at line start to dot dot.
    (when (and newline-p body-p (eql char #\.))
      (call-next-method stream #\.))
    ;; Convert newline to CRLF.
    (when (setf newline-p (eql char #\Newline))
      (call-next-method stream #\Return))
    (call-next-method)))

(defmethod stream:stream-write-string ((stream smtp-stream) string &optional (start 0) end)
  (with-slots (newline-p) stream
    (loop (let ((break (position-if #'(lambda (char)
					(or (eql char #\Newline)
					    (eql char #\.)))
				    string
				    :start start
				    :end end)))
	    (unless break
	      (setf newline-p nil)
	      (return (call-next-method stream string start end)))
	    (when (> break start)
	      (setf newline-p nil))
	    (call-next-method stream string start break)
	    (stream:stream-write-char stream (char string break))
	    (setq start (1+ break))))))


#+win32
(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant WSAENETRESET    10052)
(defconstant WSAECONNABORTED 10053)
(defconstant WSAECONNRESET   10054)
(defconstant WSAETIMEDOUT    10060)
(defconstant WSAECONNREFUSED 10061)
(defconstant WSAEHOSTDOWN    10064)
)

#+unix
(eval-when (:compile-toplevel :load-toplevel :execute)
(defconstant EPIPE 32)
(defparameter ENETRESET
  (cond ((sys:featurep :sunos4) 52)
        ((sys:featurep :svr4) 129)
        ((sys:featurep :aix) 71)
        ((sys:featurep :hp-ux) 230)
        ((sys:featurep :osf1) 52)
        ((sys:featurep :linux) 102)
        ))
(defparameter ECONNABORTED
  (cond ((sys:featurep :sunos4) 53)
        ((sys:featurep :svr4) 130)
        ((sys:featurep :aix) 72)
        ((sys:featurep :hp-ux) 231)
        ((sys:featurep :osf1) 53)
        ((sys:featurep :linux) 103)
        ))
(defparameter ECONNRESET
  (cond ((sys:featurep :sunos4) 54)
        ((sys:featurep :svr4) 131)
        ((sys:featurep :aix) 73)
        ((sys:featurep :hp-ux) 232)
        ((sys:featurep :osf1) 54)
        ((sys:featurep :linux) 104)
        ))
(defparameter ETIMEDOUT
  (cond ((sys:featurep :sunos4) 60)
        ((sys:featurep :svr4) 145)
        ((sys:featurep :aix) 78)
        ((sys:featurep :hp-ux) 238)
        ((sys:featurep :osf1) 60)
        ((sys:featurep :linux) 110)
        ))
(defparameter ECONNREFUSED
  (cond ((sys:featurep :sunos4) 61)
        ((sys:featurep :svr4) 146)
        ((sys:featurep :aix) 79)
        ((sys:featurep :hp-ux) 239)
        ((sys:featurep :osf1) 61)
        ((sys:featurep :linux) 111)
        ))
(defparameter EHOSTDOWN
  (cond ((sys:featurep :sunos4) 64)
        ((sys:featurep :svr4) 147)
        ((sys:featurep :aix) 80)
        ((sys:featurep :hp-ux) 241)
        ((sys:featurep :osf1) 64)
        ((sys:featurep :linux) 112)
        ))
)

;; Hook into error signaling
(defmethod comm::socket-error ((stream cl-http-socket-stream) error-code
                               format-control &rest format-arguments)
  (let ((class #+win32
               (case error-code
                 ((#.WSAECONNABORTED #.WSAECONNRESET)
		  'www-utils:connection-closed)
                 ((#.WSAETIMEDOUT)
                  'connection-timed-out)
                 ((#.WSAENETRESET)
                  'www-utils:connection-lost)
                 ((#.WSAECONNREFUSED)
                  'www-utils:connection-refused)
                 ((#.WSAEHOSTDOWN)
                  'www-utils:host-not-responding)
                 )
               #+unix
               (cond ((or (eql error-code EPIPE)
                          (eql error-code ECONNABORTED)
                          (eql error-code ECONNRESET))
                      'www-utils:connection-closed)
                     ((or (eql error-code ETIMEDOUT))
                      'connection-timed-out)
                     ((or (eql error-code ENETRESET))
                      'www-utils:connection-lost)
                     ((or (eql error-code ECONNREFUSED))
                      'www-utils:connection-refused)
                     ((or (eql error-code ECONNREFUSED))
                      'www-utils:host-not-responding))))
    (error (or class 'www-utils:network-error)
           :format-control "~A during socket operation: ~?"
           :format-arguments (list class format-control format-arguments))))


(define-condition www-utils:end-of-chunk-transfer-decoding (end-of-file)
  ()
  (:documentation :condition "Signalled when a complete HTTP resource has been successfully transferred.")
  (:report (lambda (condition stream)
             (format stream "End of chunk tranfer decoding on stream ~S"
                     (stream-error-stream condition)))))


(defmethod http:stream-copy-until-eof ((from-stream stream) (to-stream stream) &optional copy-mode)
  (declare (ignore copy-mode))
  (%stream-copy-bytes from-stream to-stream nil))

(defmethod http::stream-copy-bytes ((from-stream stream) (to-stream stream) n-bytes &optional (copy-mode :binary))
  (declare (ignore copy-mode))
  (%stream-copy-bytes from-stream to-stream n-bytes))

(defmethod http::stream-copy-byte-range ((from-stream stream) (to-stream stream) start last)
  (cond ((file-position from-stream start)
	 (%stream-copy-bytes from-stream to-stream (- last start)))
	(t (error "Unable to set file position for byte range copy."))))

(defun %copy-buffers (output-buffer input-buffer output-index input-index new-output-limit)
  (declare (optimize (safety 0) (fixnum-safety 0)))
  (do ((input-index input-index (1+ input-index))
       (output-index output-index (1+ output-index)))
      ((eql output-index new-output-limit))
    (setf (schar output-buffer output-index)
          (schar input-buffer input-index))))

(defmacro loop-over-stream-input-buffer ((input-buffer input-index input-limit input-bytes)
                                         (from-stream n-bytes)
                                         &body body)
  (rebinding (from-stream n-bytes)
    `(loop (when (eql ,n-bytes 0)
             (return))
           (stream:with-stream-input-buffer (,input-buffer ,input-index ,input-limit)
               ,from-stream
             (if (>= ,input-index ,input-limit)
                 (unless (handler-case (stream:stream-fill-buffer ,from-stream)
                           (www-utils:end-of-chunk-transfer-decoding
                            ()
                            nil))
                   (if ,n-bytes
                       (error "end of stream before n-bytes ~D copied" ,n-bytes)
                     (return)))
               (let* ((input-length (- ,input-limit ,input-index))
                      (,input-bytes (if (or (null ,n-bytes) (> ,n-bytes input-length))
                                        input-length
                                      ,n-bytes)))
                 ,@body
                 (when ,n-bytes
                   (decf ,n-bytes ,input-bytes))))))))

(defmethod %stream-copy-bytes ((from-stream stream:buffered-stream)
                               (to-stream stream:buffered-stream)
                               n-bytes)
  (loop-over-stream-input-buffer (input-buffer input-index input-limit input-bytes)
      (from-stream n-bytes)
    (let ((remaining-input-bytes input-bytes))
      (loop (stream:with-stream-output-buffer (output-buffer output-index output-limit)
                to-stream
              (if (>= output-index output-limit)
                  (stream:stream-flush-buffer to-stream)
                (let* ((test-output-limit (+ output-index remaining-input-bytes))
                       (new-output-limit (if (> test-output-limit output-limit)
                                             output-limit
                                           test-output-limit))
                       (output-length (- new-output-limit output-index)))
                  (%copy-buffers output-buffer input-buffer
                                 output-index input-index
                                 new-output-limit)
                  (incf input-index output-length)
                  (setf output-index new-output-limit)
                  (when (>= output-index output-limit)
                    (stream:stream-flush-buffer to-stream))
                  (decf remaining-input-bytes output-length)
                  (when (eql remaining-input-bytes 0)
                    (return))))))))
  nil)

(defmethod %stream-copy-bytes ((from-stream stream:buffered-stream)
                               (to-stream stream)
                               n-bytes)
  (loop-over-stream-input-buffer (input-buffer input-index input-limit input-bytes)
      (from-stream n-bytes)
    (let ((input-end (+ input-index input-bytes)))
      (loop for index from input-index below input-end
	    do (write-byte (char-code (schar input-buffer index)) to-stream))
      (setf input-index input-end)))
  nil)

#+comment ;; untested
(defmethod %stream-copy-bytes ((from-stream stream)
                               (to-stream stream:buffered-stream)
                               n-bytes)
  (let ((remaining-input-bytes n-bytes))
    (loop (when (eql remaining-input-bytes 0)
	    (return))
	  (loop (stream:with-stream-output-buffer (output-buffer output-index output-limit)
		    to-stream
		  (if (>= output-index output-limit)
		      (stream:stream-flush-buffer to-stream)
		    (let* ((test-output-limit (if remaining-input-bytes
						  (+ output-index remaining-input-bytes)
						output-limit))
			   (new-output-limit (if (> test-output-limit output-limit)
						 output-limit
					       test-output-limit))
			   (output-length (- new-output-limit output-index)))
		      (loop for index from output-index to new-output-limit
			    for byte = (read-byte from-stream n-bytes nil)
			    when (null byte)
			    do (progn
				 (setq new-output-limit index)
				 (setq output-length (- new-output-limit output-index))
				 (setq remaining-input-bytes 0)
				 (return))
			    do (setf (schar output-buffer index)
				     (code-char byte)))
		      (setf output-index new-output-limit)
		      (when (>= output-index output-limit)
			(stream:stream-flush-buffer to-stream))
		      (when remaining-input-bytes
			(decf remaining-input-bytes output-length))
		      (when (eql remaining-input-bytes 0)
			(return))))))))
  nil)

#+LispWorks4.0   ; post 4.0, file-stream is a stream:buffered-stream
(defmethod %stream-copy-bytes ((from-stream file-stream)
                               (to-stream stream:buffered-stream)
                               n-bytes)
  (multiple-value-bind (existing-buffer existing-index existing-limit)
      (stream-grab-existing-input-buffer from-stream n-bytes)
    (loop (when (eql n-bytes 0)
            (return))
          (stream:with-stream-output-buffer (output-buffer output-index output-limit)
              to-stream
            (if (>= output-index output-limit)
                (stream:stream-flush-buffer to-stream)
              (let ((output-length
                     (if (> existing-limit existing-index)
                         (let* ((remaining-input-bytes (- existing-limit existing-index))
                                (test-output-limit (+ output-index remaining-input-bytes))
                                (new-output-limit (if (> test-output-limit output-limit)
                                                      output-limit
                                                    test-output-limit))
                                (output-length (- new-output-limit output-index)))
                           (do ((input-index existing-index (1+ input-index))
                                (output-index output-index (1+ output-index)))
                               ((eql output-index new-output-limit))
                             (setf (schar output-buffer output-index)
                                   (char existing-buffer input-index)))
                           (incf existing-index output-length)
                           (setf output-index new-output-limit)
                           output-length)
                       (let ((output-length (sys::read-binary-bytes from-stream output-buffer
                                                                    (- output-limit output-index)
                                                                    output-index)))
                         (when (eql output-length 0)
                           (if n-bytes
                               (error "end of stream before n-bytes ~D copied" n-bytes)
                             (return)))
                         (incf output-index output-length)
                         output-length))))
                (when n-bytes
                  (decf n-bytes output-length)))))))
  nil)

#+LispWorks4.0   ; post 4.0, file-stream is a stream:buffered-stream
(defmethod %stream-copy-bytes ((from-stream stream:buffered-stream)
                               (to-stream file-stream)
                               n-bytes)
  (force-output to-stream)		; serialize, to allow unbuffered output
  (loop-over-stream-input-buffer (input-buffer input-index input-limit input-bytes)
      (from-stream n-bytes)
    (sys::write-binary-bytes to-stream input-buffer input-bytes input-index)
    (incf input-index input-bytes))
  nil)

#+LispWorks4.0   ; post 4.0, file-stream is a stream:buffered-stream
(defmethod stream-grab-existing-input-buffer ((stream file-stream)
					      n-bytes)
  (let* ((buffer (io::file-stream-buffer stream))
	 (index (io::file-stream-index stream))
	 (limit (length buffer))
	 (new-index (if n-bytes
			(min limit (the fixnum (+ index (the fixnum n-bytes))))
		      limit)))
    (declare (fixnum index limit new-index))
    (setf (io::file-stream-index stream) new-index)
    (values buffer
	    index
	    new-index)))


(defmethod http::advance-input-buffer ((stream stream:buffered-stream) &optional delta)
  (loop-over-stream-input-buffer (input-buffer input-index input-limit input-bytes)
      (stream delta)
    (incf input-index input-bytes)))


(defun www-utils:process-wait-for-stream (wait-reason stream &optional wait-function
						      timeout)
  (mp::process-wait-for-input-stream stream
				     :wait-function wait-function
                                     :wait-reason wait-reason
                                     :timeout timeout))

(defmethod http::http-input-data-available-p ((stream cl-http-socket-stream) &optional timeout-seconds)
  "Returns non-null when input data is available on the HTTP STREAM within
TIMEOUT-SECONDS.  When timeout-seconds is null, data must be immediately
available. A dead HTTP connection means no data is available.
Ports can specialize this as necessary for their stream and process implementations."
  (labels ((data-available-p (stream)
             (loop for char = (when (listen stream)
                                (peek-char nil stream nil))
                   while char                   ;clear any dangling White Space due to buggy clients.
                   when (member char '(#\return #\linefeed #\space #\tab) :test #'eql)
                     do (read-char stream t)
                   else
                     return t                   ;something still there.
                   finally (return nil)))
           (continue-p (stream)
             (or (not (www-utils:live-connection-p stream))     ;connection went dead
                 (data-available-p stream))))   ;data available
    (declare (inline data-available-p))
    (cond ((not (www-utils:live-connection-p stream)) nil)
          ((data-available-p stream) t)
	  #-LispWorks
          ((and timeout-seconds (not (zerop timeout-seconds)))
           ;; Block until there is reason to take action
           (process-wait-with-timeout
             "HTTP Request Wait" timeout-seconds #'continue-p stream)
           ;; Determine whether input data was available without consing.
           (and (www-utils:live-connection-p stream)
                (listen stream)))
	  #+LispWorks
	  ((and timeout-seconds (not (zerop timeout-seconds)))
           ;; Block until there is reason to take action
	   (loop (unless (www-utils:live-connection-p stream)
		   (return nil))
		 (when (data-available-p stream)
		   (return t))
		 (unless (www-utils:process-wait-for-stream
			  "HTTP Request Wait" stream
			  nil timeout-seconds)
		   ;; Timeout expired
		   (return nil))))
          (t nil)))) 

(defmacro www-utils:with-binary-stream ((stream direction) &body body)
  "Turns STREAM into a binary stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

(defmacro www-utils:with-text-stream ((stream direction) &body body)
  "Turns STREAM into a text stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

(defun www-utils:live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  (and (open-stream-p http-stream)
       (not (stream:stream-check-eof-no-hang http-stream))))

(declaim (inline www-utils:abort-http-stream))

(defun www-utils:abort-http-stream (http-stream)
  "Closes http-stream in abort mode.  
This will push any output in the transmit buffer and catch any network errors.
Takes care to clean up any dangling pointers."
  (handler-case 
    (close http-stream :abort t)
    (www-utils:network-error ())))

;; these definitions should be moved into the shared code -- JCMa 12/30/1994.
(defun www-utils:abort-current-connection ()
  "Aborts the computation associated with the current HTTP connection."
  (signal 'http::http-abort))

(declaim (inline www-utils:abort-if-connection-dead))

(defun www-utils:abort-if-connection-dead (http-stream)
  "Aborts the HTTP connection if the TCP/IP connection over HTTP-STREAM
has died, i.e. the remote host is no longer connected."
  (unless (www-utils:live-connection-p http-stream)
    (www-utils:abort-current-connection)))


;;; ----------------------------------------------------------------------------------
;;; Server side chunking

(eval-when (:compile-toplevel :execute)
(defmacro fixed-crlf-string (count &key prefix)
  `(load-time-value
    (coerce ',(append prefix
		      (loop repeat count
			    append '(#\Return #\Linefeed)))
	    'string)))
)

(defmethod www-utils:chunk-transfer-encoding-mode ((stream cl-http-server-socket-stream)
						   &optional function)
  (with-slots (chunk-function) stream
    (check-type function (or null function))
    (setf chunk-function function)))

(defmethod www-utils:note-first-chunk ((stream cl-http-server-socket-stream))
  (with-slots (chunk-start-index chunks-transmitted) stream
    (unless chunks-transmitted
      ;; write anything not part of the chunk
      (force-output stream)
      (stream:with-stream-output-buffer (buffer index limit) stream
        ;; Insert the CRLF part of the chunk prefix.
        (%buffer-insert-crlf buffer (the fixnum (+ index 4)))
        ;; Advance the buffer past the chunk prefix.
        (incf index 6)
        ;; Leave space in the buffer for the CRLF chunk suffix.
        (decf limit 2)
        (setf chunk-start-index index)
        ;; turn on chunking
        (setf chunks-transmitted 0)))))

(defmethod www-utils:note-last-chunk ((stream cl-http-server-socket-stream)
                                      &optional footers-plist)
  (with-slots (chunks-transmitted) stream
    (when chunks-transmitted
      ;; write the last chunk
      (force-output stream)
      ;; Restore index and limit.
      (stream:with-stream-output-buffer (buffer index limit) stream
        (decf index 6)
        (incf limit 2))
      ;; turn off chunking and write the Chunked-Body terminator if any chunks were written
      (let ((transmitted (shiftf chunks-transmitted nil)))
        (when (plusp transmitted)
          (write-string (fixed-crlf-string 1 :prefix (#\0))
		        stream)))
      (http::write-headers stream footers-plist t)
      (force-output stream))))

(defmethod stream:stream-flush-buffer ((stream cl-http-server-socket-stream))
  (with-slots (chunks-transmitted chunk-start-index) stream
    (if chunks-transmitted ; Are we chunking?
        (stream:with-stream-output-buffer (buffer index limit) stream
          (when (> index chunk-start-index)
            (let ((start (%buffer-insert-chunk-size buffer chunk-start-index index)))
              (%buffer-insert-crlf buffer index)
              (stream:stream-write-buffer stream buffer start (+ index 2)))
            (incf chunks-transmitted)
            (setf index chunk-start-index)))
      (call-next-method))))

(defun %buffer-insert-crlf (buffer index)
  (declare (fixnum index))
  (setf (schar buffer index) (code-char 13))
  (setf (schar buffer (the fixnum (1+ index))) (code-char 10)))

(defun %buffer-insert-chunk-size (buffer start end)
  (declare (fixnum start end))
  (let* ((size (- end start))
         (index (- start 2)))
    (declare (fixnum size index))
    (loop (decf index)
          (let ((digit (logand size 15)))
            (setf (schar buffer index)
                  (if (> digit 9)
                      (code-char (+ digit (- (char-code #\A) 10)))
                    (code-char (+ digit (char-code #\0))))))
          (setq size (ash size -4))
          (when (eql size 0)
            (return)))
    index))


;;; ----------------------------------------------------------------------------------
;;; Client side chunking

(defvar *debug-client-chunking* nil)

(defmethod www-utils:chunk-transfer-decoding-mode ((stream cl-http-client-socket-stream))
  (with-slots (chunk-length-received chunk-remaining-bytes chunk-real-buffer-limit) stream
    (setf chunk-length-received 0
          chunk-real-buffer-limit nil
          chunk-remaining-bytes 0 ;ensure we enter stream:stream-fill-buffer on next read
          )
    (%set-buffer-chunk-limit stream)))

(defmethod www-utils:chunk-transfer-decoding-mode-end ((stream cl-http-client-socket-stream))
  (with-slots (chunk-remaining-bytes) stream
    (setq chunk-remaining-bytes nil)))

(defmethod www-utils:chunk-transfer-content-length ((stream cl-http-client-socket-stream))
  (with-slots (chunk-remaining-bytes chunk-length-received) stream
    (if chunk-remaining-bytes
        chunk-length-received
      (error "~S is not in chunked transfer decoding mode." stream))))

(defmethod stream:stream-fill-buffer ((stream cl-http-client-socket-stream))
  (with-slots (chunk-length-received chunk-remaining-bytes chunk-real-buffer-limit) stream
    (if chunk-remaining-bytes ; Are we chunking?
        (when (if (eql chunk-remaining-bytes 0) ; end of chunk?
                  (progn
                    ;; Restore the buffer limit in case more chunk data follows.
                    (when chunk-real-buffer-limit
                      (stream:with-stream-input-buffer (buffer index limit) stream
                        (setf limit chunk-real-buffer-limit)))
                    ;; Assume another one follows.  Condition will be signaled if not.
                    (%parse-chunk-header stream
                                         #'(lambda (stream)
                                             (declare (ignore stream))
                                             (call-next-method))
                                         (not (eql chunk-length-received 0))))
                (call-next-method))
          (%set-buffer-chunk-limit stream)
          t)
      (call-next-method))))

(defun %set-buffer-chunk-limit (stream)
  (declare (type cl-http-client-socket-stream stream))
  (with-slots (chunk-remaining-bytes chunk-real-buffer-limit chunk-end-index) stream
    (stream:with-stream-input-buffer (buffer index limit) stream
      (let ((chunk-end-index (+ index chunk-remaining-bytes)))
        (if (< chunk-end-index limit)
            ;; Chunk ends within the buffer, so record the
            ;; real limit and put an artificial limit in the
            ;; stream so stream:stream-fill-buffer is called
            ;; again at the end of the chunk.
            (setf chunk-real-buffer-limit limit
                  limit chunk-end-index
                  chunk-remaining-bytes 0)
          ;; Chunk ends after the limit so allow the whole
          ;; buffer to be consumed.
          (setf chunk-real-buffer-limit nil
                chunk-remaining-bytes (- chunk-remaining-bytes (- limit index))))
        (when *debug-client-chunking*
          (format t "~&;; Client chunk section ~D~%" (- limit index)))))))

#+comment ;MJS 19Jun98: a more pernickety version
(defun %parse-chunk-header (stream buffer-fill-function skip-first-crlf)
  ;; The next bytes in the stream are supposed to be a chunk header.
  (declare (type cl-http-client-socket-stream stream))
  (macrolet ((want-char (want)
                        `(unless (char= ch ,want)
                           (error "Chunk decoding error: wanted ~S, got ~S in ~S ~S ~S"
                                  ,want ch want-cr want-lf parsing-size)))
             (adjust-size (baseline)
                          `(setq size (+ (ash size 4)
                                         (- (char-code ch) ,baseline)))))
    (let ((size 0)
          (want-char (and skip-first-crlf #\Return)))
      (block found-size
        (loop (block refill-buffer
                (stream:with-stream-input-buffer (buffer index limit) stream
                  (loop (when (>= index limit)
                          (return-from refill-buffer))
                        (let ((ch (schar buffer index)))
                          (declare (character ch))
                          (incf index)
                          (cond ((eql ch want-char)
                                 (cond ((char= ch #\Return)
                                        (setq want-char #\Newline))
                                       (t  ; must have been #\Newline
                                        (if skip-first-crlf
                                            (setq skip-first-crlf nil
                                                  want-char nil)
                                          (return-from found-size)))))
                                (want-char
                                 (error "Chunk decoding error: wanted ~S, got ~S"
                                        want-char ch))
                                (t
                                 (cond ((char<= #\0 ch #\9)
                                        (adjust-size (char-code #\0)))
                                       ((char<= #\A ch #\Z)
                                        (adjust-size (- (char-code #\A) 10)))
                                       ((char<= #\a ch #\z)
                                        (adjust-size (- (char-code #\a) 10)))
                                       ((char= ch #\Return)
                                        (setq want-char #\Newline))
                                       ((char= ch #\Space)
                                        ;; Skip space, which some servers add erroneously
                                        )
                                       (t (error "Chunk decoding error: wanted size, got ~S"
                                                 ch)))))))))
              (unless (funcall buffer-fill-function stream)
                (return-from %parse-chunk-header nil))))
      (when *debug-client-chunking*
        (format t "~&;; New client chunk ~D~%" size))
      (if (eql size 0)
          (signal 'www-utils:end-of-chunk-transfer-decoding :stream stream)
        (with-slots (chunk-length-received chunk-remaining-bytes) stream
          (setf chunk-remaining-bytes size)
          (incf chunk-length-received size)))
      t)))

(defun %parse-chunk-header (stream buffer-fill-function skip-first-crlf)
  ;; The next bytes in the stream are supposed to be a chunk header.
  (declare (type cl-http-client-socket-stream stream))
  (let ((size 0))
    (block found-size
      (loop (block refill-buffer
              (stream:with-stream-input-buffer (buffer index limit) stream
                (loop (when (>= index limit)
                        (return-from refill-buffer))
                      (let ((ch (schar buffer index)))
                        (declare (character ch))
                        (incf index)
                        (cond ((char<= #\0 ch #\9)
                               (setq size (+ (ash size 4)
                                             (- (char-code ch) (char-code #\0)))))
                              ((char<= #\A ch #\Z)
                               (setq size (+ (ash size 4)
                                             (- (char-code ch) (- (char-code #\A) 10)))))
                              ((char<= #\a ch #\z)
                               (setq size (+ (ash size 4)
                                             (- (char-code ch) (- (char-code #\a) 10)))))
                              ((char= ch #\Return)
                               ;; Skip return, assumed to be part of CRLF.
                               )
                              ((char= ch #\Newline)
                               (if skip-first-crlf
                                   (setq skip-first-crlf nil)
                                 (return-from found-size)))
                              ((char= ch #\Space)
                               ;; Skip space, which some servers add erroneously
                               )
                              (t
                               (error "Chunk decoding error got ~S"
                                      ch)))))))
            (unless (funcall buffer-fill-function stream)
              (return-from %parse-chunk-header nil))))
    (when *debug-client-chunking*
      (format t "~&;; New client chunk ~D~%" size))
    (if (eql size 0)
        (signal 'www-utils:end-of-chunk-transfer-decoding :stream stream)
      (with-slots (chunk-length-received chunk-remaining-bytes) stream
        (setf chunk-remaining-bytes size)
        (incf chunk-length-received size)))
    t))


