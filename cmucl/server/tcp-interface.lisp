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

(defvar *listener-handlers* nil
  "An alist of handlers awaiting incoming http connections on each port.")

(defvar *verbose-connections* nil)

(defun %start-listening-for-connections (&optional (port *STANDARD-HTTP-PORT*) (timeout *server-timeout*))
  "Primitive to begin listening for HTTP connections on PORT with server
  timeout, TIMEOUT."
  (let ((fd (handler-case
	     (ext:create-inet-listener port)
	     (error ()
	       (format t "~&Warning: unable to create listner on port ~d~%"
		       port)
	       nil))))
    (when fd
      (let ((handler
	     (system:add-fd-handler fd :input
               #'(lambda (fd)
		   ;; All connections are accepted here, if they are
		   ;; over the limit then they are quickly closed by
		   ;; listen-for-connection.
		   (let ((new-fd (ext:accept-tcp-connection fd)))
		     (when *verbose-connections*
		       (format t "* accept new connection fd=~s new-fd=~s~%"
			       fd new-fd))
		     (let ((stream (www-utils::make-tcp-stream new-fd port)))
		       ;; Make it non-blocking.
		       (unix:unix-fcntl new-fd unix:f-setfl unix:fndelay)
		       (listen-for-connection stream port)))))))
	(setf *listener-handlers* (acons port handler *listener-handlers*))))))

(defvar *num-requests* 0)
(defvar *request-nesting-level* 0)

(defun %provide-service (stream client-domain-name client-address)
  (flet ((log-dropped-connection (server)
           (www-utils:abort-http-stream stream)
           (set-server-status server 408)
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
	      ;; Make sure it's closed
	      (www-utils:abort-http-stream stream)))
           ;; log the access after the output has been forced.
	   (log-access server)))
    
    (atomic-incf *number-of-connections*)
    
    (let* ((server (resources:allocate-resource
		    'http-server stream client-domain-name client-address))
	   (line-buffer (resources:allocate-resource
			 'line-buffer *line-buffer-size*))
	   (port (local-port stream))
	   (local-port-context (local-port-context port))
	   (fd (system:fd-stream-fd stream))
	   (handler nil)
	   (handler-fn nil))
      (setq handler-fn
	    #'(lambda (fd)
		(system:remove-fd-handler handler)
		(incf *request-nesting-level*)
		(when *verbose-connections*
		  (when (> *request-nesting-level* 1)
		    (format t "*** nested request, level=~s fd=~s~%"
			    *request-nesting-level* fd))
		  (format t "* start request fd=~s~%" fd))
		(cond ((not (live-connection-p stream))
		       (when *verbose-connections*
			 (format t "* dead connection on fd=~s~%" fd))
		       (close stream :abort (not (live-connection-p stream)))
		       (resources:deallocate-resource
			'line-buffer line-buffer)
		       (resources:deallocate-resource 'http-server server)
		       (atomic-decf *number-of-connections*)
		       (incf *request-nesting-level*)
		       (when *verbose-connections*
			 (format t "* dropped connection fd=~s~%" fd)))
		      (t
		       (let ((*server* server)
			     (*server-line-buffer* line-buffer)
			     (*read-eval* nil)
			     (*print-pretty* nil)
			     (*standard-http-port* port)
			     (*local-context* local-port-context))
			 (clear-white-space stream)
			 (incf *num-requests*)
			 (handler-case-if (not *debug-server*)
			  (progn
			    (%process-request server stream)
			    (cond ((not (persistent-connection-p server nil))
				   (close stream
					  :abort (not (live-connection-p stream)))
				   (log-access server)
				   (resources:deallocate-resource
				    'line-buffer line-buffer)
				   (resources:deallocate-resource 'http-server server)
				   (atomic-decf *number-of-connections*)
				   (when *verbose-connections*
				     (format t "* close connection fd=~s~%" fd))
				   )
				  (t
				   (log-access server)
				   ;; Reset the server instance to avoid
				   ;; confusion accross transactions in a
				   ;; persistent connection
				   (reset-transaction-state server)
				   (setq handler (system:add-fd-handler fd :input handler-fn))
				   (when *verbose-connections*
				     (format t "* ready for new connection on fd=~s~%" fd)))))
			  (http-abort ()
			     (www-utils:abort-http-stream stream)
			     (resources:deallocate-resource
			      'line-buffer line-buffer)
			     (resources:deallocate-resource
			      'http-server server)
			     (atomic-decf *number-of-connections*)
			     (when *verbose-connections*
			       (format t "* dropped connection fd=~s~%" fd)))
			  (connection-lost ()
			     (close stream :abort t)
			     (log-dropped-connection server)
			     (resources:deallocate-resource
			      'line-buffer line-buffer)
			     (resources:deallocate-resource
			      'http-server server)
			     (atomic-decf *number-of-connections*)
			     (when *verbose-connections*
			       (format t "* dropped connection fd=~s~%" fd)))
			  (error (error)
			     (let ((fun (conditions::condition-function-name
					 error)))
			       (case fun
				 ('lisp::do-output
				     (let ((stream
					    (first
					     (conditions::simple-condition-format-arguments
					      error))))
				       (when *verbose-connections*
					 (format t "~%* Error in do-output: stream=~s~%" stream))
				       (close stream :abort (not (live-connection-p stream)))
				       (log-dropped-connection server)
				       (resources:deallocate-resource
					'line-buffer line-buffer)
				       (resources:deallocate-resource 'http-server server)
				       (atomic-decf *number-of-connections*)
				       (when *verbose-connections*
					 (format t "* dropped connection fd=~s~%" fd))))
				 ('lisp::do-input
				     (let ((stream
					    (first
					     (conditions::simple-condition-format-arguments
					      error))))
				       (when *verbose-connections*
					 (format t "~%* Error in do-input: stream=~s~%" stream))
				       (close stream :abort (not (live-connection-p stream)))
				       (log-dropped-connection server)
				       (resources:deallocate-resource
					'line-buffer line-buffer)
				       (resources:deallocate-resource
					'http-server server)
				       (atomic-decf *number-of-connections*)
				       (when *verbose-connections*
					 (format t "* dropped connection fd=~s~%" fd))))
				 (t
				  (format t "* ERROR~%")
				  (describe error)
				  (handle-unhandled-error server error)))))))))
		
		(when *verbose-connections*
		  (format t "* exit request fd=~s~%" fd))
		(decf *request-nesting-level*)))
      
      (setq handler (system:add-fd-handler fd :input handler-fn)))))

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
           (concatenate 'string "HTTP Server [" (write-to-string port :base 10.)
                        "] (" client-domain-name ")")))
    (declare (inline accept-connection-p reject-connection process-namestring))
    (cond ((accept-connection-p)
	   (let* ((client-address (www-utils::foreign-host stream))
		  (client-domain-name (host-domain-name client-address)))
	     ;; set the timeout to the connected value
	     
	     ;; Provide http service
	     (%provide-service stream client-domain-name client-address)))
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
  (declare (ignore ports))
  ;; Disable all listeners.
  (dolist (cons *listener-handlers*)
	  (let ((handler (cdr cons)))
	    (system:remove-fd-handler handler)
	    (unix:unix-close (lisp::handler-descriptor handler))))
  (setf *listener-handlers* nil))

(define http-service-enabled-p (&optional (ports `(8000)))
  "Returns the ports on which HTTP service is enabled or NIL if HTTP is
  enabled on none of PORTS. PORTS is a list of port numbers. :ANY matches
  any port." 
  (cond ((null ports)
	 (error "No ports specified  in ports."))
	((and (member :any ports) *listener-handlers*)
	 ports)
	(t
	 (loop for port in ports
	       for (p . handler) = (assoc port *listener-handlers*)
	       unless (and p handler)
	       return nil
	       finally (return ports)))))
