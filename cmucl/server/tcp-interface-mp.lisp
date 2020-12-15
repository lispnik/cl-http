;;;; Server network interface.

;;;
;;; This code was written by Douglas T. Crosher, has been placed in
;;; the public domain, and is provides 'as-is'.
;;;
;;; Ideas from the other ports, and public domain CMUCL source.

(in-package :http)

;;;------------------------------------------------------------------- 
;;;

(defparameter *number-of-listening-processes* 1
  "The number of threads simultaneously listening for HTTP connections.")

;; 5 minutes in 60ths of a second.
(setq *server-timeout* (the fixnum (* 60 60 5)))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;; SET LISP ENVIRONMENT VARIABLES



;;; Resource arg required from server;server
;;;
(resources:defresource
  http-server (stream host address)
  :constructor make-server
  :initializer initialize-resourced-server
  :deinitializer deinitialize-resourced-server
  :initial-copies 0)


;;;------------------------------------------------------------------- 
;;;
;;; LISTENING STREAMS
;;;

(defvar *listener-processes* nil
  "An alist of processes awaiting incoming http connections on each port.")

(defvar *verbose-connections* nil)

(defun %start-listening-for-connections (&optional (port *STANDARD-HTTP-PORT*)
						   (timeout *server-timeout*))
  (declare (ignore timeout)
	   (optimize (speed 3)))
  "Primitive to begin listening for HTTP connections on PORT with server
  timeout, TIMEOUT."
  (flet ((listener ()
	   (declare (optimize (speed 3)))
	   (let ((fd nil))
	     (unwind-protect
		  (progn
		    ;; Try to start the listener - sleep until available.
		    (do ((retry-wait 10 (* 2 retry-wait)))
			(fd)
		      (declare (fixnum retry-wait))
		      (handler-case
		       (setf fd (ext:create-inet-listener port))
		       (error ()
		         (format t "~&Warning: unable to create listner on ~
				    port ~d; retry in ~d seconds.~%"
				 port retry-wait)
			 (sleep retry-wait))))
		    (loop
		     ;; Wait until input ready.
		     (mp:process-wait-until-fd-usable fd :input)
		     
		     ;; All connections are accepted here. If they are
		     ;; over the limit then they are quickly closed by
		     ;; listen-for-connection.
		     (let ((new-fd (ext:accept-tcp-connection fd)))
		       (when *verbose-connections*
			 (format t "* accept new connection fd=~s ~
				    new-fd=~s port-~s~%" fd new-fd port))
		       (let ((stream (www-utils::make-tcp-stream new-fd port)))
			 ;; Make it non-blocking.
			 #+nil
			 (unix:unix-fcntl new-fd unix:f-setfl unix:fndelay)
			 (listen-for-connection stream port)))))
	       ;; Close the listener stream.
	       (when fd
		 (unix:unix-close fd))))))

    ;; Make the listening thread.
    (let ((listener-process
	   (mp:make-process
	    #'listener :name (format nil "HTTP Listener on port ~d" port))))
      (push (cons port listener-process) *listener-processes*))))


(defun %provide-service (stream client-domain-name client-address)
  (flet ((log-dropped-connection (server)
           (www-utils:abort-http-stream stream)
           (set-server-status server 408)       ;client timeout status code changed from 504 -- JCMa 5/29/1995.
           (log-access server))
         (handle-unhandled-error (server error)
           (handler-case
             (progn
               (set-server-status server 500)
               (log-access server)
               (report-status-unhandled-error error stream
					      (server-request server))
               (close stream))                  ; force output
             (error ()
	       ;; Make sure it's closed.
	       (www-utils:abort-http-stream stream)))
           ;; Log the access after the output has been forced.
	   (log-access server)))
    ;; Initialise the streams' process.
    (setf (www-utils::http-stream-process stream) mp:*current-process*)
    (resources:using-resource
      (server http-server stream client-domain-name client-address)
      (let ((*server* server)) 
        (handler-case-if (not *debug-server*)
	   (provide-service server)
          ;; Catch aborts anywhere within server.
          (http-abort () (www-utils::abort-http-stream stream))
          (connection-lost () (log-dropped-connection server))
          (http-stream-error () (log-dropped-connection server))
          (sys:io-timeout () (log-dropped-connection server))
          (error (error) (handle-unhandled-error server error)))))))

(defun listen-for-connection (stream port)
  (declare (values control-keyword))
  (flet ((accept-connection-p ()
           (< *number-of-connections* *reject-connection-threshold*))
         (reject-connection (stream string)
           (when string
             (write-string string stream)
             (force-output stream))
           (close stream :abort t))
         (process-namestring (client-domain-name port)
	   (declare (type simple-base-string client-domain-name))
           (concatenate 'string "HTTP Server [" (write-to-string port :base 10.)
                        "] (" client-domain-name ")")))
    (declare (inline accept-connection-p reject-connection process-namestring))
    (cond ((accept-connection-p)
	   (let* ((client-address (www-utils::foreign-host stream))
		  (client-domain-name (host-domain-name client-address)))
	     ;; set the timeout to the connected value
	     
	     ;; Provide http service
	     (mp:make-process 
	      #'(lambda ()
		  (%provide-service stream client-domain-name client-address))
	      :name (process-namestring client-domain-name port))
	     #+mp (mp:process-yield)))
	  (t (reject-connection stream *reject-connection-message*)
	     (close stream)))))

;;;------------------------------------------------------------------- 
;;;
;;; HIGH LEVEL OPERATIONS TO CONTROL HTTP SERVICE 
;;;

;;; Typically defaults to standard port, which is normally 8000.
(defvar *http-ports* nil
  "The ports over which http service is provided.")

(define listening-on-http-ports ()
  "Returns the ports on which the server is listening for http connections."
  (declare (values http-port-numbers))
  (or *http-ports*
      (setq *http-ports* (list *standard-http-port*))))

(export 'listening-on-http-ports :http)

(defun set-listening-on-http-ports (http-port-numbers)
  (cond ((every #'integerp http-port-numbers)
	 (unless (eq http-port-numbers *http-ports*)
	   (setq *http-ports* http-port-numbers))
	 *http-ports*)
	(t (error "Every listening port must be a number."))))

(defsetf listening-on-http-ports set-listening-on-http-ports)

(define enable-http-service (&key (on-ports (listening-on-http-ports)))
  "Top-level method for starting up HTTP servers."
  ;; Shutdown any listeners that may be awake.
  (disable-http-service on-ports)
  ;; before starting a new batch
  (dolist (port (setf (listening-on-http-ports) (ensure-list on-ports)))
    (%start-listening-for-connections port 0)
    ;; Advise the user.
    (expose-log-window)
    (notify-log-window "HTTP service enabled for: http://~A:~D/"
                       (www-utils:local-host-domain-name) port))
  on-ports)

(defun disable-http-service (&optional ports)
  ;; Disable all listeners.
  (let ((rem-listeners nil))
    (dolist (port-process-cons *listener-processes*)
      (cond ((or (null ports) (member (first port-process-cons) ports))
	     (mp:destroy-process (cdr port-process-cons)))
	    (t
	     (push port-process-cons rem-listeners))))
    (setf *listener-processes* rem-listeners)))

(define http-service-enabled-p (&optional (ports `(8000)))
  "Returns the ports on which HTTP service is enabled or NIL if HTTP is
  enabled on none of PORTS. PORTS is a list of port numbers. :ANY matches
  any port." 
  (cond ((null ports)
	 (error "No ports specified  in ports."))
	((and (member :any ports) *listener-processes*)
	 ports)
	(t (loop for port in ports
		 for (p . proc) = (assoc port *listener-processes*)
		 unless (and p proc)
		 return nil
		 finally (return ports)))))


;;;------------------------------------------------------------------- 
;;;
;;; SPECIALIZED HTTP STREAM POLLING
;;;

(defmethod http-input-data-available-p (stream &optional timeout-seconds)
  (declare (type (or null fixnum) timeout-seconds)
	   (optimize (speed 3)))
  (labels ((data-available-p (stream)
             (loop while (listen stream)        ;some data available
                   for char = (peek-char nil stream nil)
                   while char                   ;clear any dangling White Space due to buggy clients.
                   when (member char '(#\return #\linefeed #\space #\tab) :test #'eql)
                     do (read-char stream t)
                   return t                     ;something still there.
                   finally (return nil))))
    (cond ((not (www-utils:live-connection-p stream)) nil)
          ((data-available-p stream) t)
          ((and timeout-seconds (not (zerop timeout-seconds)))
	   (loop
	    (cond ((not (www-utils:live-connection-p stream))
		   (return nil))
		  ((data-available-p stream)
		   (return t))
		  (t
		   ;; Block until there is reason to take action
		   (unless (mp:process-wait-until-fd-usable
			    (www-utils::http-stream-fd stream)
			    :input timeout-seconds)
		     ;; Timeout expired.
		     (return nil))))))
          (t nil))))
