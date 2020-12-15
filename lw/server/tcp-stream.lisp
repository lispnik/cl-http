;;; LispWorks interface to TCP/IP streams
;;;
;;; Copyright (C) 1995-1998 Harlequin Group plc.  All rights reserved.
;;;


;; NB: Within Lisp, all addresses and port numbers are in host order.

(in-package "IPC")

(defmacro define-class-synonym (synonym-name real-name)
  `(setf (find-class ',synonym-name) (find-class ',real-name)))

(defmacro add-default-conditions (&rest types)
  `(progn
     ,@(mapcar
	#'(lambda (sname)
	    `(deftype ,(intern sname 'www-utils) () 'error))
	types)))

(add-default-conditions
 #-(or FRANZ-INC LispWorks)
 "BAD-CONNECTION-STATE"
 "CONNECTION-CLOSED"
 "CONNECTION-ERROR"
 #-(or FRANZ-INC LispWorks)
 "CONNECTION-LOST"
 "CONNECTION-REFUSED"
 #-LispWorks
 "DOMAIN-RESOLVER-ERROR"
 #-LispWorks
 "HOST-NOT-RESPONDING"
 #-LispWorks
 "HOST-STOPPED-RESPONDING"
 #-FRANZ-INC
 "LOCAL-NETWORK-ERROR"
 #-(or FRANZ-INC LispWorks)
 "NETWORK-ERROR"
 "NETWORK-PARSE-ERROR"
 "NETWORK-RESOURCES-EXHAUSTED"
 "PROTOCOL-TIMEOUT"
 #-(or FRANZ-INC LispWorks)
 "REMOTE-NETWORK-ERROR"
 #-LispWorks
 "UNKNOWN-ADDRESS"
 #-(or FRANZ-INC LispWorks)
 "UNKNOWN-HOST-NAME")

;;; When remote end aborts stream while we are transfering
;;; closing the stream can cause a Signal 13 on our end.
#+LispWorks
(deftype www-utils:bad-connection-state () 'conditions:file-stream-error)

#+LispWorks
(deftype www-utils:host-not-responding () 'conditions:file-stream-error)

#+LispWorks
(define-class-synonym www-utils:host-stopped-responding conditions:file-stream-error)

#+LispWorks
(deftype www-utils:connection-lost () 'conditions:file-stream-error)

#+LispWorks
(deftype www-utils:remote-network-error ()
  '(or ipc:ipc-network-error conditions:file-stream-error))

#+LispWorks
(deftype www-utils:network-error ()
  '(or ipc:ipc-network-error
       conditions:file-stream-error
       www-utils:network-error-mixin))

#+LispWorks 
(define-condition www-utils:network-error-mixin
		  (error)
  ()
  (:documentation "Mixin to allow ports to inherit instance variables and methods to network conditions
defined at the portable code level."))


(ffi:define-foreign-type array-array-unsigned-bytes
                         (:array (:pointer ffi::array-unsigned-bytes) 1))

#|
     	      char   *h_name;	    /* official	name of	host */
	      char   **h_aliases;   /* alias list */
	      int    h_addrtype;    /* address type */
	      int    h_length;	    /* length of address */
	      char   **h_addr_list; /* list of addresses from name server */
|#
(ffi:define-foreign-type hostent
                         (:structure
                          (name :lisp-string)
                          (aliases (:pointer :lisp-string))
                          (addrtype :int)
                          (length :int)
                          (addrlist (:pointer array-array-unsigned-bytes))))

#|	short	sin_family;
	u_short	sin_port;
	struct	in_addr sin_addr;
	char	sin_zero[8];
|#
(ffi:define-foreign-type sockaddr-in
                         (:structure
                          (family :short)
                          (port :unsigned-short)
                          (in-addr :unsigned-int)
                          (zero (:array :unsigned-byte 8))))


(ffi:define-foreign-function gethostbyname
                             ((name :simple-string))
                             :result-type hostent)

(ffi:define-foreign-function gethostbyaddr
                             ((addr :simple-array) (len :fixnum) (type :fixnum))
                             :result-type hostent)

(ffi:define-foreign-function ntohs ((n :fixnum)) :result-type :fixnum)
(ffi:define-foreign-function ntohl ((n :uinteger)) :result-type :uinteger)

(ffi:define-foreign-function (getpeername-in getpeername)
                             ((fd :fixnum)
                              (name sockaddr-in)
                              (namelen :integer :reference))
                             :result-type :fixnum)

(ffi:define-foreign-function (c-getdomainname getdomainname)
                             ((name :simple-string)
                              (namelen :fixnum))
                             :result-type :fixnum)

(ffi:define-foreign-function fstat
 			     ((fd :fixnum) (buf :simple-string))
 			     :result-type :fixnum)
 
(defconstant +stat-buffer-length+ 256)	;longer than the longest stat buffer on all systems

(ffi:read-foreign-modules)

(define-condition ipc-network-error (error)
  ())

(define-condition unknown-host-name (ipc-network-error)
  ((hostname :initarg :hostname))
  (:report (lambda (condition stream)
	     (format stream "Unknown host name ~A"
		     (slot-value condition 'hostname)))))

(define-condition unknown-address (ipc-network-error)
  ((address :initarg :address))
  (:report (lambda (condition stream)
	     (let ((address (slot-value condition 'address)))
	       (format stream "Unknown address ~A"
		       (ip-address-string address))))))

(define-condition domain-resolver-error (ipc-network-error)
  ()
  (:report (lambda (condition stream)
	     (declare (ignore condition))
	     (format stream "Cannot find current domainname"))))

(define-condition unix-socket-error (ipc-network-error)
  ((function :initarg :function)
   (errno :initarg :errno))
  (:report (lambda (condition stream)
	     (let ((errno (slot-value condition 'errno)))
	       (format stream "UNIX error '~A' (errno ~D) in ~A"
		       (get-unix-error errno) errno
		       (slot-value condition 'function))))))


(defvar *simple-string-buffer*
  (sys:in-static-area (make-string 40 :element-type 'base-character)))

(defvar *address-array*
  (sys:in-static-area (make-array 32 :element-type '(unsigned-byte 8))))

;;; Turn internet address into string format
(defun ip-address-string (address)
  (format nil "~D.~D.~D.~D"
          (ldb (byte 8 24) address)
          (ldb (byte 8 16) address)
          (ldb (byte 8 8)  address)
          (ldb (byte 8 0)  address)))

(defun string-ip-address (address-string)
  (macrolet ((when-let* (clauses &body body)
			(let ((res `(progn ,@body)))
			  (dolist (clause (reverse clauses))
			    (setq res `(let (,clause)
					 (when ,(car clause)
					   ,res))))
			  res)))
    (when-let* ((dot1 (position #\. address-string))
		(dot2 (position #\. address-string :start (1+ dot1)))
		(dot3 (position #\. address-string :start (1+ dot2)))
		(num1 (ignore-errors
			(parse-integer address-string
				       :end dot1)))
		(num2 (ignore-errors
			(parse-integer address-string
				       :start (1+ dot1)
				       :end dot2)))
		(num3 (ignore-errors
			(parse-integer address-string
				       :start (1+ dot2)
				       :end dot3)))
		(num4 (ignore-errors
			(parse-integer address-string
				       :start (1+ dot3)))))
	       (dpb num1 (byte 8 24)
		    (dpb num2 (byte 8 16)
			 (dpb num3 (byte 8 8)
			      num4))))))

(defun internet-address (name)
  (or (internet-address-domain name)
      (error 'unknown-host-name :hostname name)))

;;; Like gethostbyname but returns an internet address (long network format)
(defun internet-address-domain (name)
  (without-preemption
    (let ((simple-name
           (if (simple-string-p name)
               name
             (progn
               (replace *simple-string-buffer* name)
               (setf (schar *simple-string-buffer* (length name)) (code-char 0))
               *simple-string-buffer*))))
      (let ((hostent (gethostbyname simple-name)))
        (and (not (ffi::ffi-null hostent))
             (let ((bytes (array-array-unsigned-bytes[]
                           (hostent->addrlist hostent)
                           0)))
               (dpb (ffi::array-unsigned-bytes[] bytes 0) (byte 8 24)
                    (dpb (ffi::array-unsigned-bytes[] bytes 1) (byte 8 16)
                         (dpb (ffi::array-unsigned-bytes[] bytes 2) (byte 8 8)
                              (ffi::array-unsigned-bytes[] bytes 3))))))))))

(defvar *domainname* nil)

;;; There are more than one strategies you can use
;;; based on what OS you are using. Instead of providing
;;; each one we try one.
;;;
(defun getdomainname (&optional (where "/etc/defaultdomain"))
  (or *domainname*
      (let ((buffer (make-string 64 :element-type 'base-character)))
	(and (zerop (c-getdomainname buffer (length buffer)))
             (setq *domainname*
                   (subseq buffer 0 (position #\Null buffer)))))
      (if (probe-file where)
	  (with-open-file (stream where :direction :input)
	    (setq *domainname* (read-line stream))))
      (error 'domain-resolver-error)))


(defparameter *af-inet* 2)

(defun get-host-name-by-address-array (array)
  (let ((hostent (gethostbyaddr array 4 *af-inet*)))
    (if (ffi::ffi-null hostent)
        nil
      (hostent->name hostent))))

(defun get-host-name-by-address (address)
  (or (without-preemption
       (let ((array *address-array*))
	 (declare (type (simple-array (unsigned-byte 8) (32)) array))
	 (setf (aref array 0) (ldb (byte 8 24) address)
	       (aref array 1) (ldb (byte 8 16) address)
	       (aref array 2) (ldb (byte 8 8)  address)
	       (aref array 3) (ldb (byte 8 0)  address))
	 (get-host-name-by-address-array array)))
      (error 'unknown-address :address address)))

(defvar *sockaddr-in* nil)

(defun get-peer-host-and-port (fd)
  (without-preemption
    (let* ((sockaddr-in (or *sockaddr-in*
                            (setq *sockaddr-in* (make-sockaddr-in))))
           (res (getpeername-in fd sockaddr-in (ffi::size-of-c-type 'sockaddr-in))))
      (if (zerop res)
          (values (ntohl (sockaddr-in->in-addr sockaddr-in))
                  (ntohs (sockaddr-in->port sockaddr-in)))
        (error 'unix-socket-error
	       :function 'getpeername
	       :errno (errno-value))))))


(defstruct cl-http-stream-info
  local-port
  remote-host
  remote-port
  (bytes-received 0)
  (bytes-transmitted 0)
  chunk-prefix
  chunk-suffix
  chunk-start
  chunk-function
  chunks-transmitted
  )

;; MJS 12Aug96: Most of this is terrible hack.
(defun make-tcp-stream (fd port)
  (multiple-value-bind (in-stream out-stream)
      (comm::streams-from-fd fd)
    (multiple-value-bind (remote-address remote-port)
        (get-peer-host-and-port fd)
      (let* ((stream (make-two-way-stream in-stream out-stream))
	     (old-output-buffer-write (sys::file-stream-buffer-write out-stream))
	     (info (add-cl-http-stream-info stream port remote-address remote-port)))
	(setf (sys::file-stream-buffer-write out-stream)
	      #'(lambda (stream buffer start len  byte-len)
		  (incf (cl-http-stream-info-bytes-transmitted info)
			len)
		  (if (and (cl-http-stream-info-chunks-transmitted info) ;chunkingp
			   (compute-chunk-info info))
		      (progn
			(let ((chunk-prefix (cl-http-stream-info-chunk-prefix info)))
			  (funcall old-output-buffer-write
				   stream chunk-prefix 0 (length chunk-prefix) 1))
			(funcall old-output-buffer-write
				 stream buffer start len  byte-len)
			(let ((chunk-suffix (cl-http-stream-info-chunk-suffix info)))
			  (funcall old-output-buffer-write
				   stream chunk-suffix 0 (length chunk-suffix) 1)))
		    (funcall old-output-buffer-write
			     stream buffer start len  byte-len))))
        stream))))

(defun add-cl-http-stream-info (stream port remote-address remote-port)
  (let* ((old-misc (sys::stream-misc stream))
	 (info (make-cl-http-stream-info
		:local-port port
		:remote-host remote-address
		:remote-port remote-port)))
    (setf (sys::stream-misc stream)
	  #'(lambda (stream operation abortflag)
	      (if (eq operation 'info)
		  info
		(progn
		  (when (eq operation :close)
		    (unless abortflag
		      (force-output (two-way-stream-output-stream stream)))
		    (close (two-way-stream-input-stream stream) :abort abortflag))
		  (unless (eq operation :soft-force-output) ;maximum chunking
		    (funcall old-misc stream operation abortflag))))))
    info))

(defmacro cl-http-stream-info (stream)
  (rebinding (stream)
    `(and (open-stream-p ,stream)
	  (funcall (sys::stream-misc ,stream) ,stream 'info nil))))

(defun %listen-for-connections (port function)
  (comm::listen-and-attach-stream `(lispworks-listen-for-connection
                                    ,port
                                    ,function)
                                  port ;service
                                  nil  ;announce
                                  ))

(defun lispworks-listen-for-connection (fd port function)
  (let ((stream (ignore-errors		;handle connection errors
		 (make-tcp-stream fd port))))
    (when stream
      (funcall function stream  port))))

(defmacro find-detail-from-stream (stream detail)
  (let ((accessor (ecase detail
		    (:local-port 'cl-http-stream-info-local-port)
		    (:remote-host 'cl-http-stream-info-remote-host)
		    (:remote-port 'cl-http-stream-info-remote-port))))
    `(,accessor (cl-http-stream-info ,stream))))

(defun www-utils:local-port (http-stream)
  (find-detail-from-stream http-stream :local-port))

(defun www-utils:foreign-host (http-stream)
  (find-detail-from-stream http-stream :remote-host))

(defun www-utils:foreign-port (http-stream)
  (find-detail-from-stream http-stream :remote-port))

(declaim (inline www-utils:bytes-transmitted www-utils:bytes-received))

(defun www-utils:bytes-transmitted (stream)
  "Returns the number of bytes transmitted over STREAM."
  (let ((info (cl-http-stream-info stream)))
    (if info
	(cl-http-stream-info-bytes-transmitted info)
      0)))

(defun (setf www-utils:bytes-transmitted) (bytes stream)
  (let ((info (cl-http-stream-info stream)))
    (when info
      (setf (cl-http-stream-info-bytes-transmitted info) bytes)))
  bytes)

(defun www-utils:bytes-received (stream)
  (let ((info (cl-http-stream-info stream)))
    (if info
	(cl-http-stream-info-bytes-received info)
      0)))

(defun (setf www-utils:bytes-received) (bytes stream)
  (let ((info (cl-http-stream-info stream)))
    (when info
      (setf (cl-http-stream-info-bytes-received info) bytes)))
  bytes)

(defun http::%open-http-stream-to-host (host port timeout)
  (handler-case
      (let ((stream (comm:open-tcp-stream (get-host-name-by-address host)
					  port)))
	(when stream
	  (add-cl-http-stream-info stream nil host port))
	stream)
    (comm::connection-error
     ()
     ;; MJS 21Aug96: resignal a different condition.  May of may not be the
     ;; right one, it's not clear what the hierarchy is supposed to mean.
     (error 'conditions:file-stream-error
	    :errno (lw:errno-value)
	    :read "connecting"
	    :stream (format nil "~A:~A" host port)))))


(defconstant +stream-copy-bytes-buffer-length+ 8192)
(defvar *stream-copy-bytes-buffers* nil)

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

(defmethod http:stream-copy-until-eof ((from-stream system::two-way-stream) to-stream &optional (copy-mode :text))
  (http:stream-copy-until-eof (two-way-stream-input-stream from-stream) to-stream copy-mode))

(defmethod http:stream-copy-until-eof (from-stream (to-stream system::two-way-stream) &optional (copy-mode :text))
  (http:stream-copy-until-eof from-stream (two-way-stream-output-stream to-stream) copy-mode))

(defmethod http:stream-copy-until-eof ((from-stream file-stream) (to-stream file-stream) &optional copy-mode)
  (declare (ignore copy-mode))
  (%stream-copy-bytes from-stream to-stream nil))

(defmethod http::stream-copy-bytes ((from-stream file-stream) (to-stream file-stream) n-bytes &optional (copy-mode :binary))
  (declare (ignore copy-mode))
  (%stream-copy-bytes from-stream to-stream n-bytes))

(defmethod http::stream-copy-byte-range ((from-stream file-stream) (to-stream file-stream) start last)
  (cond ((file-position from-stream start)
	 (%stream-copy-bytes from-stream to-stream (- last start)))
	(t (error "Unable to set file position for byte range copy."))))

(defun %stream-copy-bytes (from-stream to-stream n-bytes)
  (force-output to-stream)		; serialize, to allow unbuffered output
  (let ((buffer (without-interrupts
		 (let ((bufs *stream-copy-bytes-buffers*))
		   (if bufs
		       (prog1
			   (car bufs)
			 (setq *stream-copy-bytes-buffers* (cdr bufs)))
		     (make-array +stream-copy-bytes-buffer-length+
				 :element-type '(unsigned-byte 8)))))))
    (if n-bytes
	(loop while (> n-bytes 0)
	      do
	      (let ((length (max n-bytes +stream-copy-bytes-buffer-length+)))
		(sys::read-binary-bytes from-stream buffer length)
		(sys::write-binary-bytes to-stream buffer length)
		(decf n-bytes length)))
      (loop
       (let ((length (sys::read-binary-bytes
		      from-stream buffer +stream-copy-bytes-buffer-length+)))
	 (when (<= length 0)
	   (return))
	 (sys::write-binary-bytes to-stream buffer length))))
    (without-interrupts
     (push buffer *stream-copy-bytes-buffers*))
    nil))


(declaim (inline %http-input-data-available-p))

(defun %http-input-data-available-p (stream &optional timeout-seconds)
  "Returns non-null when input data is available on the HTTP STREAM within TIMEOUT-SECONDS.
When timeout-seconds is null, data must be immediately available. A dead HTTP connection
means no data is available."
  (labels ((data-available-p (stream)
             (loop while (listen stream)        ;some data available
                   for char = (peek-char nil stream nil)
                   while char                   ;clear any dangling White Space due to buggy clients.
                   when (member char '(#\return #\linefeed #\space #\tab) :test #'eql)
                     do (read-char stream t)
                   return t                     ;something still there.
                   finally (return nil)))
           (continue-p (stream)
             (or (not (www-utils:live-connection-p stream))     ;connection went dead
                 (data-available-p stream))))   ;data available
    (declare (inline data-available-p))
    (cond ((not (www-utils:live-connection-p stream)) nil)
          ((data-available-p stream) t)
          ((and timeout-seconds (not (zerop timeout-seconds)))
           ;; Block until there is reason to take action
           #-LispWorks
           (process-wait-with-timeout
             "HTTP Request Wait" timeout-seconds #'continue-p stream)
           #+LispWorks
           (www-utils:process-wait-for-stream
             "HTTP Request Wait" stream timeout-seconds #'continue-p stream)
           ;; Determine whether input data was available without consing.
           (and (www-utils:live-connection-p stream)
                (listen stream)))
          (t nil)))) 

;; it would be useful if an LW wizard could confirm whether this works or not.   11/6/96 -- JCMa.
(defmethod http::http-input-data-available-p ((stream system::two-way-stream) &optional timeout-seconds)
 (%http-input-data-available-p stream timeout-seconds))

(defmethod http::http-input-data-available-p ((stream file-stream) &optional timeout-seconds)
 (%http-input-data-available-p stream timeout-seconds))

(defun www-utils:process-wait-for-stream (wait-reason stream &optional
						      timeout
						      predicate &rest args)
  (let ((fd (sys::file-stream-fd (two-way-stream-input-stream stream))))
    (unwind-protect
	(progn
	  (mp:notice-fd fd)
	  (apply 'mp:process-wait-with-timeout wait-reason timeout
		 predicate args))
      (mp:unnotice-fd fd))))


(defun www-utils:live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  (and (open-stream-p http-stream)
       (>= (fstat (sys::file-stream-fd
		   (two-way-stream-input-stream http-stream))
		  (load-time-value (make-string +stat-buffer-length+)))
	   0)))

(declaim (inline www-utils:abort-http-stream))

(defun www-utils:abort-http-stream (http-stream)
  "Closes http-stream in abort mode.  
This will push any output in the transmit buffer and catch any network errors.
Takes care to clean up any dangling pointers."
  (handler-case 
    (close http-stream :abort t)
    (#+ccl ccl::network-error #-ccl error ())))

;; these definitions should be moved into the shared code -- JCMa 12/30/1994.
(defun www-utils:abort-current-connection ()
  "Aborts the computation associated with the current HTTP connection."
  (signal 'http-abort))

(declaim (inline www-utils:abort-if-connection-dead))

(defun www-utils:abort-if-connection-dead (http-stream)
  "Aborts the HTTP connection if the TCP/IP connection over HTTP-STREAM
has died, i.e. the remote host is no longer connected."
  (unless (www-utils:live-connection-p http-stream)
    (www-utils:abort-current-connection)))


;;; ----------------------------------------------------------------------------------
;;; Chunk transfer handling code

(eval-when (:compile-toplevel :execute)
(defmacro fixed-crlf-string (count &key prefix)
  `(load-time-value
    (coerce ',(append prefix
		      (loop repeat count
			    append '(#\Return #\Linefeed)))
	    'string)))
)

(defmethod www-utils:chunk-transfer-encoding-mode ((stream t)
						   &optional function)
  (check-type function (or null function))
  (let ((info (cl-http-stream-info stream)))
    (setf (cl-http-stream-info-chunk-function info) function)))

(defmethod www-utils:note-first-chunk ((stream t))
  ;; write anything not part of the chunk
  (force-output stream)
  (let ((info (cl-http-stream-info stream)))
    ;; chunk starts at current position
    (setf (cl-http-stream-info-chunk-start info)
	  (cl-http-stream-info-bytes-transmitted info))
    ;; turn on chunking
    (setf (cl-http-stream-info-chunks-transmitted info) 0)))

(defmethod www-utils:note-last-chunk ((stream t) &optional footers-plist)
  ;; write the last chunk
  (force-output stream)
  ;; turn off chunking and write the Chunked-Body terminator if any chunks were written
  (let* ((info (cl-http-stream-info stream))
	 (transmitted (shiftf (cl-http-stream-info-chunks-transmitted info) nil)))
    (when (plusp transmitted)
      (write-string (fixed-crlf-string 1 :prefix (#\0))
		    stream)))
  (http::write-headers stream footers-plist t)
  (force-output stream))

(defun compute-chunk-info (info)
  (let ((chunk-length (- (cl-http-stream-info-bytes-transmitted info)
			 (cl-http-stream-info-chunk-start info))))
    (if (> chunk-length 0)		;do nothing for empty chunk
	(progn
	  ;; setup for next chunk
	  (setf (cl-http-stream-info-chunk-start info)
		(cl-http-stream-info-bytes-transmitted info))
	  (incf (cl-http-stream-info-chunks-transmitted info))
	  ;; Prefix: <HEX excluding "0"> *HEX CRLF
	  (setf (cl-http-stream-info-chunk-prefix info)
		(format nil "~X~A" chunk-length (fixed-crlf-string 1)))
	  ;; Suffix: CRLF
	  (unless (cl-http-stream-info-chunk-suffix info)
	    (setf (cl-http-stream-info-chunk-suffix info)
		  (fixed-crlf-string 1)))
	  t)
      nil)))

(define-condition www-utils:end-of-chunk-transfer-decoding (end-of-file)
  ()
  (:documentation :condition "Signalled when a complete HTTP resource has been successfully transferred."))



