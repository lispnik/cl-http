(in-package "HTTP")

;;;
;;; Add proxy for the ACL version of CL-HTTP, should be portable to other CL.
;;; Copyright (C) 1995 Olivier (OBC) all rights reserved.
;;;
;;; This is a prototype for proxy gateways access using non-persistent
;;; connections, tested only with HTTP/1.0 and Netscape 1.1N...
;;;
;;; This is only a prototype, there are still cases where the underlying
;;; substrate gets NIH errors that will block future proxy access from
;;; CL-HTTP (remaining odds with UNIX sockets not closing properly) however
;;; the local CL-HTTP services remain functional it seems safe to enable
;;; proxy gateway access for experimentation.
;;;

(defpackage "HTTP"
  (:use)
  (:export "DEFINE-PROXY-ENTRY" "UNDEFINE-PROXY-ENTRY" "ENABLE-PROXY-SERVICE" "DISABLE-PROXY-SERVICE" "DEFINE-PROXY-DIRECTS"))

(defvar *proxy-gateways* nil)

(defun enable-proxy-service ()
  "Enable the use of the proxies previously defined using DEFINE-PROXY-ENTRY.
If no proxies are defined the proxy service is disabled."
  (setq *proxy-service* (and *proxy-gateways* t)))

(defun disable-proxy-service ()
  "Disable to access to proxy gateways."
  (setq *proxy-service* nil))

(defun define-proxy-entry (name &key ports timeout)
  "Define a proxy host to forward unanswered requests from this server to.
The TIMEOUT argument is optional and defines the time allowed in seconds
to wait from a response from the proxy gateway (default 120 seconds).
Example: (define-proxy-entry \"some.proxy.site.edu\" :ports 8000)"
  (let ((redef (find name *proxy-gateways* :key #'first :test #'equal)))
    (unless redef
      (setq redef `(,name :ports nil :activity 1))
      (setf *proxy-gateways* (nconc *proxy-gateways* (list redef))))
    (if (consp ports)
	(setf (getf (rest redef) :ports) ports)
      (if (integerp ports)
	  (pushnew ports (getf (rest redef) :ports))
	(error "Invalid port(s): ~a." ports)))
    (if (and (integerp timeout) (plusp timeout))
	(setf (getf (rest redef) :timeout) timeout))
    name))

(defun undefine-proxy-entry (name &rest ignore)
  (declare (ignore ignore))
  "Removes the proxy definition for this host \"name\"."
  (setq *proxy-gateways*
    (delete name *proxy-gateways* :key #'first :test #'equal))
  t)

(defvar *proxy-directs* nil)

(defun define-proxy-directs (&rest directs)
  "Define the list of host names or domain names that can be directly
accessed by the CL-HTTP server and don't need to go throught a proxy gateway.
Examples: (DEFINE-PROXY-DIRECTS \"uni.site.edu\" \"site.edu\")
where \"site.edu\" can be accessed directly by this server.
	  (DEFINE-PROXY-DIRECTS)
disables direct accesses previously defined."
  (setq *proxy-directs* directs))

(defvar *host-proxy-address-mapping* (make-hash-table :test #'equal))

(defmethod ip-host-proxy-address ((host string))
  (or (gethash host *host-proxy-address-mapping*)
      (ipc:internet-address host)
      (let (address)
	(loop for (proxy) in *proxy-gateways*
	    thereis (and proxy
			 (setq address (ipc:internet-address proxy))))
	(if address
	    (setf (gethash host *host-proxy-address-mapping*) address)))))

(defun clear-host-proxy-address-mapping ()
  (clrhash *host-proxy-address-mapping*))

(defvar *proxy-debug* nil)

(defmacro with-debug ((&optional (n 0)) . body)
  `(when (and (numberp *proxy-debug*)
	      (>= *proxy-debug* ,n))
     ,@body))

;;; Forward unanswered requests blindly to another proxy host.
;;;
(defmethod provide-proxy-service ((server server) method http-version)
  (declare (ignore method http-version))
  (with-slots (stream url-string url) server
    #+(and Allegro (not ACL5))
    (with-debug (2)
      (progn
	(format t "~&Client Request Buffer~%")
	(princ (excl:stream-input-buffer stream))
	(terpri)))
    (with-debug (1)
      (describe server)
      (print url-string))
    (let ((direct (host-proxy-direct (url::get-host-port-info url-string))))
    (if (and direct (www-utils::record-original-context))
	;; Restore proper host for direct proxy with shadow name
	(www-utils::with-original-context
	    (setf url (intern-url url-string :if-does-not-exist :create))))
    (unless url
      (setf url (intern-url url-string :if-does-not-exist :create)))
    (with-debug () (print url))
    (multiple-value-bind (proxystream timeout record)
	(find-proxy-connection url)
      (flet ((update-proxy-activity (activity)
	       (when record
		 (setf (getf (rest record) :activity) activity))))
	;;(format t "~& PROXY STREAM: ~s~%" proxystream)
	(cond ((tcp-client-stream-p proxystream)
	       (handler-case-if
		(not *debug-server*)
		(progn
		  (proxy-forward-request server url-string proxystream :direct direct)
		  (let (proxyready error)
		    #+Allegro
		    (mp:wait-for-input-available
		     (list proxystream)
		     :whostate "Wait for proxy response"
		     :wait-function
		     #'(lambda (proxystream)
			 (multiple-value-setq (proxyready error)
			   (ignore-errors (listen proxystream)))
			 (or proxyready error))
		     :timeout timeout)
		    #-Allegro
		    (clim-sys:process-wait-with-timeout
		     "Waiting for proxy response" timeout
		     #'(lambda ()
			 (multiple-value-setq (proxyready error)
			   (ignore-errors (listen proxystream)))
			 (or proxyready error)))
		    (cond (proxyready
			   (update-proxy-activity 10)
			   #+(and Allegro (not ACL5))
			   (with-debug (2)
			     (unread-char (read-char proxystream) proxystream)
			     (format t "~&Proxy Response Buffer~%")
			     (princ (excl:stream-input-buffer proxystream))
			     (terpri))
			   #+ignore (stream-byte-transfer proxystream stream)
			   (stream-transfer-until-eof proxystream stream)
			   ;; This seems to fail on some direct proxies
			   #+ignore
			   (case (copy-mode url)
			     (:binary
			      (with-binary-stream (proxystream :output)
				(stream-copy-until-eof proxystream stream)))
			     (t
			      (stream-copy-until-eof proxystream stream)))
			   (finish-output stream)
			   (close proxystream :abort nil))
			  (t
			   (format t "~&No Proxy Ready...~%")
			   (update-proxy-activity 0)
			   (if error
			       (close stream :abort t)
			     (error "Proxy response timeout ~a" timeout))
			   (close proxystream :abort t)))))
		(error (error)
		       (update-proxy-activity 0)
		       (force-output stream)
		       (close proxystream :abort t)
		       ;; Let the outer handler process this error
		       (error error))))
	      (t
	       (error "No proxy responding at this time"))))))))

#+(and Allegro (not ACL5))
(defmethod stream-transfer-until-eof ((from-stream stream:fundamental-input-stream) (to-stream stream:fundamental-output-stream))
  (www-utils::finish-outputs to-stream) ; Fresh start
  (loop with finishing
      with buffer-size = (excl:stream-buffer-length from-stream)
      with buffer = (make-string buffer-size)
		    ;; (excl:stream-input-buffer from-stream)
		    ;; Suddenly in 4.3 using the stream buffer does not work
		    ;; as the first character gets overwritten.
      as end = (read-sequence buffer from-stream :end buffer-size)
      when (> end 0)
      do (write-sequence buffer to-stream :end end)
	 ;;(write-sequence buffer *standard-output* :end end)
      when (< end buffer-size)
      do (www-utils::finish-outputs to-stream) ; Synchronize streams
      until (and finishing (zerop end))
      when (zerop end)
      do (mp:wait-for-input-available (list from-stream) :whostate "Wait for end effective end of transmission..." :timeout 3)
	 (setq finishing t)
      else do (setq finishing nil)))
	 
#-Allegro
(defmethod stream-byte-transfer ((from stream) (to stream))
  (loop as byte = (read-byte from nil :eof)
      until (eql byte :eof)
      do (write-byte to byte)
	 #+Debug
	 (write-char (character byte))))

#+(and Allegro Debug)
(defmethod stream-byte-transfer ((from stream:fundamental-input-stream) (to stream:fundamental-output-stream))
  (loop as byte = (stream:stream-read-byte from)
      until (eql byte :eof)
	do (stream:stream-write-byte to byte)
	   (write-char (character byte))))

(defun tcp-client-stream-p (stream)
  #+Allegro
  (typep stream 'ipc::tcp-client-stream))

(defmethod proxy-forward-request ((server server) (url-string string) tostream &key direct)
  (let ((headers (header-plist)))
    ;; (remf headers :proxy-connection) ;;Not ready for persist?
    ;; 6/17/96 notice that the headers from CL-HTTP are currently
    ;; incomplete. The Accept: is reduced from orignially: 
    ;; "Accept: image/gif, image/x-xbitmap, image/jpeg, image/pjpeg"
    ;; to only: "Accept: image/gif". This may need further checking!
    ;;(print headers)
    ;; This will need reverse processing on the response from gateway
    #+ignore
    (if (and (www-utils::record-original-context)
	     (getf headers :host))
	(setf (getf headers :host) (list (second (www-utils::record-original-context)))))
    (flet ((write-proxy-headers (tostream)
	     (write-server-request server url-string tostream :direct direct)
	     (terpri tostream)
	     (write-headers tostream headers t)
	     ;;(write-char #\Null tostream)
	     (finish-output tostream)))
      (with-debug (2)
	(write-proxy-headers *standard-output*))
      (write-proxy-headers tostream))))

(defmethod write-server-request ((server server) (url-string string) stream &key direct)
  (cond (direct
	 (write-direct-server-request server url-string stream))
	(t
	 (write-string (server-request server) stream))))

(defmethod write-direct-server-request ((server server) (url url::host-port-mixin) stream)
  (let ((url-string (server-url-string server)))
    (setq url-string
      (or (url-string-subpath url-string (host-string url))
	  (url-string-strip-front url-string)
	  url-string))
    (write-string (symbol-name (server-method server)) stream)
    (write-char #\Space stream)
    (write-string url-string stream)
    (write-char #\Space stream)
    (write-string (let ((version (server-http-version server)))
		    (typecase version
		      (symbol (symbol-name version))
		      (string version)
		      (t (princ-to-string version))))
		  stream)))

;;; Since the translated direct host is not necessarily the
;;; same as the host in the url string this is fall-back that
;;; kind of works for now...
;;;
(defmethod url-string-strip-front (string)
  (let ((pos1 (position #\/ string)))
    (if (and pos1
	     (< (incf pos1) (length string))
	     (char= (elt string pos1) #\/)
	     (setq pos1 (position #\/ string :start (incf pos1))))
	(subseq string pos1))))


(defmethod write-direct-server-request ((server server) (url-string string) stream)
  (write-direct-server-request server (intern-url url-string :if-does-not-exist :create) stream))

(defun url-string-subpath (str host)
  (let (next)
    (flet ((substr= (str1 str2 &key (start 0))
	     (let ((end (+ start (length str1))))
	       (and (<= end (length str2))
		    (string= str1 str2 :start2 start :end2 end)
		    end))))
      (and (setq next (substr= "http://" str))
	   (setq next (substr= host str :start next))
	   (cond ((char= #\: (elt str next))
		  (incf next)
		  (setq next (position-if-not #'digit-char-p str :start next))
		  (subseq str next))
		 (t
		  (subseq str next)))))))
  
(defmethod host-proxy-direct ((url url::host-mixin))
  (host-proxy-direct (host-string url)))

;;; This host really does not need an intermediate proxy
;;; it can be reached directly (in principle).
;;;
(defmethod host-proxy-direct ((host string))
  (if (find-if #'(lambda (direct)
		   (let ((l1 (length host))
			 (l2 (length direct)))
		     (and (>= l1 l2)
			  (string= host direct :start1 (- l1 l2)))))
	       *proxy-directs*)
      host))

(defmethod translate-direct-host ((url url::host-mixin))
  (translate-direct-host (host-string url)))

(defvar *direct-host-translations* (make-hash-table :test #'equal))

(defun clear-direct-host-translations ()
  (clrhash *direct-host-translations*))

(defmethod translate-direct-host ((url string))
  (let ((found (gethash url *direct-host-translations*)))
    (if (consp found)
	(first found)
      (or found
	  (setf (gethash url *direct-host-translations*)
	    (host-http-name (parse-host url)))))))

(defun clear-proxy-mappings ()
  (clear-direct-host-translations)
  (clear-host-proxy-address-mapping))

;;; Typical servers use one of these for HTTP service
;;; update if your direct access servers do otherwise
;;;
(defvar *default-http-ports* '(80 8000))

(defmethod direct-host-ports-order ((url string) &rest failed-ports)
  (let ((found (gethash url *direct-host-translations*))
	ports)
    (if (consp found)
	(setq ports (rest found)))
    (cond (failed-ports
	   ;; Reorder
	   (setq ports (nconc (loop for port in (or ports *default-http-ports*)
				  unless (find port failed-ports)
				  collect port)
			      failed-ports))
	   (if (consp found)
	       (setf (rest found) ports)
	     (setf (gethash url *direct-host-translations*)
	       (cons (or found url) ports)))
	   ports)
	  (ports)
	  (*default-http-ports*))))

;;; Hunt for proxy connection strategy
;;;
(defmethod find-proxy-connection ((url url::host-port-mixin) &key (min-activity 0) (default-timeout 120))
  (find-proxyhost-connection (host-string url) :port (port url) :min-activity min-activity :default-timeout default-timeout))

(defun find-proxyhost-connection (host &key port (min-activity 0) (default-timeout 120))
  (let (connection)
    (setq host (host-proxy-direct host))
    (if (and host
	     (setq host (translate-direct-host host)))
	(when (if port
		  (setq connection
		    (find-direct-connection host :port port))
		(let (failed-ports)
		  (dolist (port (direct-host-ports-order host))
		    (setq connection
		      (find-direct-connection host :port port))
		    (if connection
			(return connection)
		      (pushnew port failed-ports)))
		  (if failed-ports
		      (apply #'direct-host-ports-order host failed-ports))
		  connection))
	  (values connection default-timeout nil))
      (find-proxy-connection nil :min-activity min-activity
			     :default-timeout default-timeout))))

;;; Use the proxy gateway entries declared by user to resolve
;;; and forward unanswered requests
;;;
(defmethod find-proxy-connection ((url null) &key (min-activity 0) (default-timeout 120))
  (let (selection connection use-timeout)
    (loop for gateway in *proxy-gateways*
	do (destructuring-bind (name &key ports activity (timeout default-timeout)) gateway
	     (when (and activity
			(if min-activity
			    (> activity min-activity)
			  t))
	       (loop until connection
		   repeat 5
		   do (setq connection
			(loop for port in ports
			    as connection = (find-direct-connection name :port port)
			    if connection
			    return connection
			    else do (setf (getf (rest gateway) :activity) 0)))
		      (clim-sys:process-sleep 2))
	       (when connection
		 (setq selection gateway use-timeout timeout)
		 (return)))))
    (if connection
	(values connection use-timeout selection)
      (if min-activity
	  (find-proxy-connection nil :min-activity nil :default-timeout default-timeout)))))
