;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

;;; Copyright John C. Mallery,  1995-1997, 1999.
;;; All rights reserved.

;;;------------------------------------------------------------------- 
;;;
;;; MULTI-THREADED SERVER INTERFACE 
;;;

;;; Modification History
;;;
;;; 02/20/97 jcma  merged digitool changes into cl-http source. Removed decisions about recycling connections
;;;                which are better done at lower levels. Rationalized code. Source compatible with 68k again.
;;; 12/22/96 akh   initialize-http-stream doesnt check state - let stream-release-connection do it.
;;; 12/14/96 akh   %listen-for-connections can time out - we were still getting hung there
;;;                 in :dataxfer with nothing happening - log the timeout
;;;                also log the %forcibly-shutdown in listen-for-connection
;;; 12/02/96 bill  initialize-http-stream does stream-passive-reconnect if it makes sense.
;;;                make-http-stream sets the stream-io-buffer slot to NIL.
;;;                launch-process binds (new special variable) *http-process-p* to T.
;;;                (method ccl::stream-close :around (ccl:basic-tcp-stream))
;;;                calls ccl::stream-release-connection instead of the next method
;;;                if *http-process-p* and *leave-streams-open-p* are true.
;;;                release-connection in %listen-for-connections does ccl::stream-passive-reconnect
;;;                after ccl::stream-release-connection.
;;; 11/28/96 bill  Use new task-process mechanism in make-http-process,
;;;                deinitialize-http-process, launch-process.
;;;                listen-for-connection sets the tcp-stream-process to NIL
;;;                before %forcibly-shutdown-listening-stream, so that
;;;                the process won't be reset.
;;; 11/22/96 bill  listen-for-connection does a process-wait before anything else.
;;;                This one change radically speeds up the whole server.
;;;                This change hasn't yet been tested with xmactcp, only OpenTransport.
;;; 11/18/96 bill  deinitialize-http-stream & listen-for-connection call
;;;                http-stream-connection-state instead of ccl::stream-connection-state.
;;;                initialize-mac-tcp-maximum-number-of-connections calls
;;;                (setf ccl::tcp-maximum-number-of-connections) instead of
;;;                frobbing ccl::*tcp-maximum-number-of-connections*
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;;  STREAM AND SERVER RESOURCES
;;;

(defparameter *number-of-listening-processes* 5.
  "The number of threads simultaneously listening for HTTP connections.")

(export '*number-of-listening-processes* :http)

(define-parameter *command-timeout-for-listeners* 5
   "Number of seconds before a command times out on a TCP stream listening for connections.")

(defparameter *listener-process-priority* 0
  "The process priority of HTTP listener processes.")

(defparameter *server-run-interval* 12.
  "The interval of execution quanta allocated for each scheduling of an HTTP thread.")

;; 2 minutes in 60ths of a second.
(setq *server-timeout* (the fixnum (* 60. 60. 2.)))

;; buffer cache in the HTTP.
(defparameter *http-stream-buffer-size* 4096
  "Controls the size of the HTTP stream buffer.
The default size for MACTCP is 1024, but 2048 is what Netscape uses.") 

(declaim (fixnum *http-stream-buffer-size*)) 

(defun set-http-stream-buffer-size (integer)
  (check-type integer integer)
  (setq *http-stream-buffer-size* integer
        ccl:*elements-per-buffer* integer))     ; match file size to TCP buffer size

(export 'set-http-stream-buffer-size :http)

(set-http-stream-buffer-size 4096)      ;  8192 , default is 1024, 4096 improves standalone performance.

(defun initialize-mac-tcp-maximum-number-of-connections ()
   (let ((max (+ 5 (the fixnum *maximum-number-of-connections*)
                        (the fixnum *number-of-listening-processes*))))
      (setf (ccl::tcp-maximum-number-of-connections) max)))

(ccl::advise set-maximum-number-of-connections
             (initialize-mac-tcp-maximum-number-of-connections)
             :when :after
             :name "Set TCP connection reservoir size.") 

;;;------------------------------------------------------------------- 
;;;
;;; 
;;; SET LISP ENVIRONMENT VARIABLES

;; how long to wait for wait-next-event when no event
(setq ccl::*idle-sleep-ticks* 1)

;; Same when lisp is a background MAC application.
(setq ccl::*background-sleep-ticks* 1) 

;;;------------------------------------------------------------------- 
;;;
;;;  RESOURCED HTTP PROCESSES
;;;

(defvar *process-index* 0) 

(declaim (notinline %register-process))

(defun %register-process (process connection-spec)
   (let ((plist (www-utils::%process-property-list process))) 
      (setf (getf plist :http-process) t)        ; mark it as an http process
      (when connection-spec
          (destructuring-bind (type address port) connection-spec
             (setf (getf plist :connection-type) type
                      (getf plist :connection-address) address
                      (getf plist :connection-port) port)))
      (setf (www-utils::%process-property-list process) plist))) 

(declaim (notinline %unregister-process))

(defun %unregister-process (process)
   (let ((plist (www-utils::%process-property-list process)))
      (setf (getf plist :http-process) nil
               (getf plist :http-process-name) nil)))

(declaim (notinline %http-process-p))

(defun %http-process-p (process)
   (and (typep process 'ccl::process)
           (getf (www-utils::%process-property-list process)  :http-process)))

;; loses printing on window due to MCL problems   4/13/99 -- JCMa.
#+ignore
(defun write-http-process-name (p stream)
   (case (get-value p :connection-type)
      (:server
        (write-string "HTTP Server [" stream)
        (ccl::tcp-addr-to-str (get-value p :connection-address) stream)
        (format stream"] (~D)" (get-value p :connection-port)))
      (:listener
        (format stream"HTTP Listener (~D)" (get-value p :connection-port)))
      (:client 
        (format stream "HTTP Client (~A)" (get-value p :connection-address)))
      (t (ccl::process.name p))))

(defun write-http-process-name (p stream)
  (case (get-value p :connection-type)
    (:server
      (format stream "HTTP Server [~A] (~D)" 
	      (ccl::tcp-addr-to-str (get-value p :connection-address) nil)
	      (get-value p :connection-port)))
    (:listener
      (format stream"HTTP Listener (~D)" (get-value p :connection-port)))
    (:client 
      (format stream "HTTP Client (~A)" (get-value p :connection-address)))
    (t (ccl::process.name p))))

;; MCL would be smarter to have methods defined on processes. -- JCMa 4/4/1999.
(let* ((ccl::*warn-if-redefine* nil)
       (ccl::*warn-if-redefine-kernel* nil))
  (defun ccl::process-name (p)
    (if (%http-process-p p)
      (or (get-value p :http-process-name)
          (setf (get-value p :http-process-name) (write-http-process-name p nil)))
      (ccl::process.name p))))

(defmethod print-object :around ((p ccl::process) s)
   (if (%http-process-p p)
      (print-unreadable-object (p s :type t :identity t)
         (write-http-process-name p s))
      (call-next-method)))

(defun make-http-process (resource connection-spec)
   (declare (ignore resource connection-spec) 
                 (values process))
   (macrolet ((default-process-segment-size ()   ; introduced by name change in MCL3b3 -- JCMa 5/18/1995.
	               (if (boundp 'ccl::*dflt-process-stackseg-size*)
                          'ccl::*dflt-process-stackseg-size*
		          'ccl::*default-process-stackseg-size*)))
       (let* ((idx (incf (the fixnum *process-index*)))
	         (mac-name (concatenate 'string "HTTP Process " (write-to-string idx :escape nil :base 10.))))
          (ccl:make-task-process mac-name :stack-size (default-process-segment-size) :background-p t))))

(defun initialize-http-process (resource process connection-spec)
   (declare (ignore resource))
   (%register-process process connection-spec)
   process)

(defun http-process-shutdown (process)
   (when (%http-process-p process)            ; prevents multiple deallocation
       #|(notify-log-window "~&Shutdown Process ~A" process)|#
       (resources:deallocate-resource 'http-process process)
       process)) 

(defun deinitialize-http-process (resource process)
   (declare (ignore resource))
   (%unregister-process process)
   (case (ccl:task-process-state process)
      (:idle)
      (t (ccl::process-reset process)))
   process) 

(defun launch-process (connection-spec keywords function &rest args)
   (declare (values process)
                 #|(arglist &key priority quantum)|#)
   (flet ((http-process-toplevel (process function args)
	       (unwind-protect
	          (ccl::with-standard-abort-handling "Exit HTTP Process"
                     (apply function args))
	          (http-process-shutdown process)))) 
      (destructuring-bind (&key (priority 0) (quantum *server-run-interval*)) keywords
         (ccl:require-type priority 'fixnum)
         (ccl:require-type quantum 'fixnum)
         (let ((process (resources:allocate-resource 'http-process connection-spec)))
            (setf (ccl:process-priority process) priority
                     (ccl::process.quantum process) quantum)
	    (ccl:task-process-run-function process #'http-process-toplevel process function args)
	    process)))) 

;;;------------------------------------------------------------------- 
;;;
;;; Resourced HTTP Streams 
;;;

(declaim (inline http-stream-connection-state))

(defun http-stream-connection-state (stream)
  "Returns a Standard keyword describing the connection state of stream."
  (ccl::stream-connection-state-name stream))

#+Open-Transport 
(defun make-http-stream (resource host &optional port timeout process)
   (declare (ignore resource host port timeout process))
   (let ((stream (ccl::allocate-instance  (ccl::find-class 'ccl:modal-ascii-or-binary-tcp-stream))))
      (setf (ccl::stream-io-buffer stream) nil)         ;ensure that ccl::stream-connection-state-name won't error
      stream))

#-Open-Transport 
(defun make-http-stream (resource host &optional port timeout process)
   (declare (ignore resource host port timeout process))
   (ccl::allocate-instance  (ccl::find-class 'ccl:modal-ascii-or-binary-tcp-stream)))

(defun initialize-http-stream (resource http-stream host &optional (port 80.) (timeout *server-timeout*)  (process ccl::*current-process*))
   (declare (ignore resource))
   (flet ((do-it (http-stream host port timeout)
               (initialize-instance http-stream
                                             :host host
                                             :port port
                                             :element-type '(unsigned-byte 8)
                                             :commandtimeout timeout
                                             :writebufsize *http-stream-buffer-size*)))
      (declare (inline do-it))
      (do-it http-stream host port (floor (the fixnum timeout) 60))     ; convert sixtieths of a second to seconds 
      ;; initialize the process slot here in 3.0a but move into initialize-instance
      ;; later   2/23/95 -- JCMa.
      (setf (ccl:tcp-stream-process http-stream) process)
      http-stream))                                  ; return the stream 

(defun deinitialize-http-stream (resource stream)
   (case (http-stream-connection-state stream)
      ((:closed  #+Open-Transport :idle))
      ((:syn-received) (close stream :abort t))
      (t (notify-log-window  "While deallocating a ~S, unknown tcp stream state, ~S (~D),  on ~S."
                                         resource (http-stream-connection-state stream) 
                                         (http-stream-connection-state  stream) stream)
          (close stream :abort t)))
   ;; Reset some instance variables on deallocation
   (setf (ccl::stream-bytes-transmitted stream) 0
            (ccl::stream-bytes-received stream) 0)
   ;; deallocate process
   (shutdown-http-stream-process stream)
   (setf (ccl:tcp-stream-process stream) nil)
   ;; reset chunking data
   stream) 

(defun clear-http-stream-resource ()
   (resources:clear-resource 'http-stream))

(defmethod deinitialize-resourced-server :before (resource (server basic-server-mixin))
  (declare (ignore resource))
  (let ((stream (server-stream server)))
    (when stream
      (resources:deallocate-resource 'http-stream stream))))

(resources:defresource
  http-stream (host port timeout process)
  :constructor make-http-stream
  :initializer initialize-http-stream
  :deinitializer deinitialize-http-stream
  :initial-copies 0)

(resources:defresource
  http-server (stream host address)
  :constructor make-server
  :initializer initialize-resourced-server
  :deinitializer deinitialize-resourced-server
  :initial-copies 0)

(resources:defresource
  http-process (name priority quantum)
  :constructor make-http-process
  :initializer initialize-http-process
  :deinitializer deinitialize-http-process
  :initial-copies 0)


;;;------------------------------------------------------------------- 
;;;
;;; THREAD MECHANISM 
;;;

(defmethod shutdown-http-stream-process ((tcp-stream ccl:modal-ascii-or-binary-tcp-stream))
  (with-slots (ccl::process) tcp-stream
    (when ccl::process
      (http-process-shutdown ccl::process))))

(defmethod enable-server ((server basic-server-mixin)) 
  (with-slots (stream) server
    (with-slots (ccl::process) stream
      (when (and stream ccl::process)
        (ccl::process-enable ccl::process)))))

(defmethod disable-server ((server basic-server-mixin))
  (with-slots (stream) server
    (with-slots (ccl::process) stream
      (when (and stream ccl::process)
        #-Open-Transport
        (ccl::process-disable ccl::process)))))

;;;------------------------------------------------------------------- 
;;;
;;; LISTENING STREAMS
;;;

(defvar *server-control-alist* nil
  "A property list of  streams awaiting incoming http connections on each port.")

(defun %register-listening-stream (stream &optional (port (www-utils::foreign-port stream)))
  (www-utils::with-atomic-execution
    (let ((entry (assoc port *server-control-alist* :test #'eql)))
      (cond (entry (push stream (cdr entry)))
            (t (push `(,port ,stream) *server-control-alist*))))))

(defun %swap-listening-stream (new-stream old-stream &optional (port (www-utils:foreign-port old-stream)))
  (www-utils::with-atomic-execution
    (let ((entry (assoc port *server-control-alist* :test #'eql)))
      (cond (entry
             (loop for l = entry then (cdr l)
                   while l
                   when (eq (second l) old-stream)
                     do (setf (second l) new-stream)
                        (return-from %swap-listening-stream entry)
                   finally (push new-stream (cdr entry))))
            (t (push `(,port ,new-stream) *server-control-alist*))))))

(defun %unregister-listening-stream (stream)
  (declare (values unregistered-p))
  (flet ((delete-from-entry (s entry)
           (when (member s (cdr entry) :test #'eq)
             (if (cddr entry)
                 (setf (cdr entry) (delete s (cdr entry) :test #'eq))
                 (setq *server-control-alist* (delete entry *server-control-alist* :test #'eq))))
           t))
    (let  ((port (www-utils:local-port stream)))
      (www-utils::with-atomic-execution
        (cond (port
               (let ((entry (assoc port *server-control-alist* :test #'eql)))
                 (delete-from-entry stream entry)))
              ;; port may be null when the stream is closed, so search for it.
              (t (loop for entry in *server-control-alist*
                       when (delete-from-entry stream entry)
                         do (return t)
                       finally (return nil))))))))

(declaim (inline %listening-streams))

(define %listening-streams (&optional (port 80))
  "Returns the streams listening on PORT."
  (cdr (assoc port *server-control-alist* :test #'eql)))

(define %map-listening-streams (function &optional (ports :all))
  "Maps FUNCTION over streams listening on PORTS,
which is either :ALL or a list of port numbers."
  (loop for (port . streams) in *server-control-alist*
        do (when (and streams 
                      (or (eq ports :all) (member port ports :test #'eql))) 
             (mapc function streams))))

(define http-service-enabled-p (&optional (ports `(80.)))
   "Returns the ports on which HTTP service is enables or NIL if HTTP is enabled on none of PORTS.
PORTS is a list of port numbers. :ANY matches any port." 
   (cond ((null ports) (error "No ports specified  in ports."))
            ((and (member :any ports) *server-control-alist*) ports)
            (t (loop for port in ports
                        for (p . streams) = (assoc port *server-control-alist*)
                        unless (and streams p)
                        return nil
                        finally (return ports))))) 

(declaim (inline %listening-on-port-p))

(define %listening-on-port-p (&optional (port 80))
   "Returns non-null when listening for HTTP connections on PORT."
   (not (null (%listening-streams port))))

(declaim (inline %listening-on-ports))

(define %listening-on-ports ()
  "Returns the port numbers on which there is listening for HTTP connections."
  (mapcar #'car *server-control-alist*))

(defun %forcibly-shutdown-listening-stream (stream &optional state)
  ;; unregister it
  (%unregister-listening-stream stream)
  (unless (member (or state (http-stream-connection-state stream)) '(:closed))
    (close stream :abort t))
  (resources:deallocate-resource 'http-stream stream))

(defun %shutdown-idle-listening-stream (stream)
  (declare (values shutdown-p))
  ;; shut down only when idle
  (www-utils::with-atomic-execution             ; keep this atomic
    (let ((state (http-stream-connection-state stream)))
      (case state
        ;; already running, so let it complete.
        (:established nil)
        ;; shut it down and deallocate.
        (t (%forcibly-shutdown-listening-stream stream state)
           t)))))

;; Calling this function within a stream thread requires issuing a
;; process-run-function 2/23/95 -- JCMa.
(defun %stop-listening-for-connections (&optional (port 80.) &aux more-streams-p)
  (flet ((careful-shutdown (stream)
           (unless (%shutdown-idle-listening-stream stream)
             (setq more-streams-p t))))
    (declare (dynamic-extent #'careful-shutdown))
    (when (%listening-on-port-p port)
      (let ((ports (list port)))
        (declare (dynamic-extent ports))
        (loop doing (%map-listening-streams #'careful-shutdown ports)
                    (if more-streams-p
                        (setq more-streams-p nil)
                        (return)))
        ;; Advise the user. 
        (expose-log-window)
        (notify-log-window "HTTP service disabled for: http://~A:~D/"
                           (www-utils:local-host-domain-name) port)))
    (values t)))

(defun %enforce-number-of-listeners-for-connection (number &optional (port 80.) 
                                                           (timeout *server-timeout*))
  (let* ((s (%listening-streams port))
         (len (length s)))
    (cond ((> len number)
           (loop with idx = (- len number)
                 for streams = s then (%listening-streams port)
                 do (loop for stream in streams
                          when (%shutdown-idle-listening-stream stream)
                            do (when (zerop (decf idx))
                                 (return-from %enforce-number-of-listeners-for-connection number)))))
          ((< 0 len number)
           (dotimes (idx (- number len))
             (%start-listening-for-connections port timeout))))
    number))

(define-variable *connection-releases* 0
                 "Counter for the number of connections that have been released due to errors in TCP streams.")

(define-parameter *maximum-restarts-per-listener* 30
		  "Threshold for the maximum number of restarts to try on any one TCP listening stream.
If the number is less than zero, infinite restarts are allowed.") 

(defun %listen-for-connections (port timeout &aux (restarts 0))  ; timeout is 0 in practice
   (flet ((allocate-stream (&optional old-stream)    ;;allocate an http stream
	       (let ((new-stream (resources:allocate-resource 'http-stream nil port *command-timeout-for-listeners* ccl::*current-process*)))
	          ;; set the control stream so we know that we are listening for new connections.
	          (if old-stream
		     (%swap-listening-stream new-stream old-stream port)
		     (%register-listening-stream new-stream port))
	          new-stream))
	    (report-release (err stream restarts abort-p)
	       (report-bug *bug-http-server* (format nil "HTTP Listener Error: ~S" (type-of err))
		                  "~&Error in the HTTP listening stream ~S.~
                                    ~&Error Type: ~S~
                                    ~&Releases ~D (~D)~
                                    ~&~:[Restarting~;Aborting~]"
		                  stream (type-of err) restarts *connection-releases* abort-p))
	    (start-up-timeout-p (start-tick start-timeout)
	       (> (- (#_tickcount) start-tick) start-timeout)))
      (declare (inline allocate-stream ))
      (macrolet ((release-connection (stream &optional err)
		          `(let ((restart-p (or (< *maximum-restarts-per-listener* 0)
				                         (< restarts *maximum-restarts-per-listener*))))
		              (atomic-incf *connection-releases*)
		              (incf restarts)
		              (when ,err (report-release ,err ,stream restarts restart-p))
		              ;; Trap when limits exceeded.
		              (unwind-protect
			         (unless restart-p
			            (cerror "Reset counter and restart connection"
				                 "~D connection restarts per listener exceeds the limit, ~D."
				                 restarts *maximum-restarts-per-listener*)
			            (setf restarts 1))
		                 ;; always release bad connections
		                 (ccl::stream-release-connection ,stream))
		              ;; restart
		              (setf (ccl:tcp-stream-process ,stream) nil)	; don't free this process
		              (go start))))
          (tagbody
	     start 
	     (let ((stream (allocate-stream))
		     (*server-timeout* timeout)	; one needs to stop the listening streams to change the timeout
		     (start-timeout (* 60 (the fixnum ccl::*tcp-read-timeout*))))
	        (unwind-protect
		   (handler-case-if 
		      (not *debug-server*) 
		      (loop with start-tick 
			       doing (ecase (listen-for-connection stream port)
				           (:continue (setq start-tick nil))
				           (:block ;; blocking occurs when the stream is neither in :LISTEN or :ESTABLISHED
				              (unless start-tick (setq start-tick (#_tickcount)))	; set timeout out first time around
				              (cond
				                ((start-up-timeout-p start-tick start-timeout)	; assume stream wedged once timeout exceeded.
				                  (notify-log-window "Shutting down HTTP listener ~S because it failed to respond within ~D seconds." 
							                         stream (float (/ start-timeout 60. )))
				                  (release-connection stream))
				                (t (ccl::suspend-current-process "Listen Wait"))))
				           (:new-stream	;Standard hand off of listening stream to service connection
				             (setq start-tick nil
					              stream (allocate-stream stream)))
				           (:replace-stream	;Non-standard replacement of stream shutdown by error.
				             (setf (ccl:tcp-stream-process stream) nil)	; don't free this process
				             (go start))
				           (:quit
				             (setq stream nil)	;%shutdown-idle-listening-stream already does the forceable shutdown
				             (return-from %listen-for-connections))))
		      ;; This error occurs because we can't get the connection into a listening state.
		      ;; Try releasing the connection and firing up a new one.
		      (ccl::tcp-invalid-data-structure (err) (release-connection stream err))
		      (ccl::tcp-connection-state-timeout (err) (release-connection stream err)))
	           (when stream
		       (%forcibly-shutdown-listening-stream stream))))))))

(defun %start-listening-for-connections (&optional (port 80.) (timeout *server-timeout*)
                                                                               (process-priority *listener-process-priority*))
   "Primitive to begin listening for HTTP connections on PORT with server timeout, TIMEOUT."
   ;; run the listening thread
   (let ((connection-spec `(:listener nil ,port))
           (keywords `(:priority ,process-priority)))
      (declare (dynamic-extent connection-spec keywords))
      (launch-process connection-spec keywords #'%listen-for-connections port timeout)))

(defun %provide-service (stream client-address client-host)
   (flet ((log-dropped-connection (server)
               (www-utils::abort-http-stream stream)
               (set-server-status server 408)       ;client timeout status code changed from 504 -- JCMa 5/29/1995.
               (log-access server))
            (handle-unhandled-error (server error)
               (handler-case
                  (progn
                     (set-server-status server 500)
                     (log-access server)
                     (report-status-unhandled-error error stream (server-request server))
                     (close stream))                  ; force output
                  (ccl:network-error ()))
               ;; log the access after the output has been forced.
               (log-access server)))
      ;; make sure we know our process.
      (setf (ccl:tcp-stream-process stream) ccl:*current-process*) 
      ;; client-host is NIL because we only parse it on demand.
      (resources:using-resource (server http-server stream client-host client-address)
         ;; keep other processes from tampering with our TCP state.
         (let ((*server* server))
            (handler-case-if (not *debug-server*)
                                      (provide-service server)
               ;; catch aborts anywhere within server.-- JCMa 12/30/1994.
               ;; log aborts as dropped connections by client. -- JCMa 5/24/1999.
               (http-abort () (log-dropped-connection server))
               (ccl:protocol-timeout () (log-dropped-connection server))
               (ccl:connection-lost () (log-dropped-connection server))
               (ccl:host-stopped-responding () (log-dropped-connection server))
               (ccl:bad-connection-state () (log-dropped-connection server))
               (error (error) (handle-unhandled-error server error))))))) 

(define listen-for-connection (stream port)
   (declare (values control-keyword))
   (flet ((accept-connection-p ()
	       (< *number-of-connections* *reject-connection-threshold*))
	    (reject-connection (stream string)	;counts as a connection because we have wait for close to timeout.
	       (atomic-incf (the fixnum *number-of-connections*))
	       (unwind-protect
	          (progn (when string
                                 (write-string string stream)
			         (force-output stream))
                             (close stream))
	          (atomic-decf (the fixnum *number-of-connections*))))
	    (incoming-connection-p (stream)
               (not (eq :listen (http-stream-connection-state stream)))))
      (declare (inline accept-connection-p reject-connection process-namestring))
      ;;  Listen for connections
      ;;(process-wait "Listen" #'incoming-connection-p stream)
      (ccl::process-poll "Listen" #'incoming-connection-p stream)       ; reduce response latency -- JCMa 5/15/1999.
      (let ((state (http-stream-connection-state stream)))
         (case state
	    (:listen (if (%listening-on-port-p port)
                           :continue			;Keep up the good work
		           :quit))			;Quit listening in response to command
	    ((:established :syn-received :syn-sent)
              (cond ((null (ccl::stream-listen stream))	;Data available?
                         (if (%listening-on-port-p port)
                            :block			;Wait for data
		            :quit))			;Quit listening in response to command
	               ((accept-connection-p)
                         ;; don't resolve domain names because you'll crawl at the rate of DNS   3/8/97 -- JCMa.
                         (let* ((client-address (www-utils::foreign-host stream))
                                   (client-host (www-utils:ip-address-for-parsed-ip-address client-address))
                                   (connection-spec `(:server ,client-address ,port)))
                            (declare (dynamic-extent connection-spec))
		            ;;Set the timeout to the connected value
		            (setf (ccl::stream-read-timeout stream) (floor (the fixnum *server-timeout*) 60)	;convert to seconds
                                     (ccl:tcp-stream-process stream) nil)	;set to NIL pending launch
		            ;;Start a thread to provide http service
                            (launch-process connection-spec '(:priority 0) #'%provide-service stream client-address client-host)
		            :new-stream))			;request new listening stream
	               (t (reject-connection stream *reject-connection-message*)
                           :continue)))			;Continue running 
	    ;;Handle random states.
	    (t (notify-log-window "Shutting down HTTP listener ~S because it went into the state ~S." stream state)
                :replace-stream)))))			;replace listening stream 

;;;------------------------------------------------------------------- 
;;;
;;; HIGH LEVEL OPERATIONS TO CONTROL HTTP SERVICE 
;;;

(defvar *http-ports* nil   ; typically defaults to standard port, which is normally 80
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

(define enable-http-service (&key (on-ports (listening-on-http-ports)) (listeners *number-of-listening-processes*))
  "Top-level method for starting up HTTP servers."
  ;; Shutdown any listeners that may be awake.
  (disable-http-service on-ports)
  ;; before starting a new batch
  (dolist (port (setf (listening-on-http-ports) (ensure-list on-ports)))
    ;; Provide multiple listening streams so that MACTCP is ready to accept the connection.
    (dotimes (idx (ceiling listeners (length on-ports)))
      (%start-listening-for-connections port 0))
    ;; Advise the user.
    (expose-log-window)
    (notify-log-window "HTTP service enabled for: http://~A:~D/"
                       (www-utils:local-host-domain-name) port))
  on-ports)

(define start (&key (listeners *number-of-listening-processes*))
  "Short version of ENABLE-HTTP-SERVICE."
  (enable-http-service :listeners listeners))

(export 'start :http)

(define disable-http-service (&optional (on-ports :all))
  "Top-level method for shutting down HTTP servers."
  (mapc #'%stop-listening-for-connections
        (case on-ports
          (:all (%listening-on-ports))
          (t on-ports))))

(defmethod shutdown-server ((server basic-server-mixin))
  "Forcibly shutdown a running (or idle) HTTP server."
  (with-slots (stream) server
    (when stream
      (close stream :abort t))
    (resources:deallocate-resource 'http-server server)))

(define map-http-resource (keyword allocation function)
  "Maps FUNCTION over the resource denoted by KEYWORD with ALLOCATION. 
KEYWORD can be any of :STREAM, :SERVER, or :PROCESS
ALLOCATION can be any of :ALL, :FREE, or :ALLOCATED."
  (flet ((fctn (item allocated-p resource)
           (declare (ignore resource))
           (when (ecase allocation
                   (:all t)
                   (:free (not allocated-p))
                   (:allocated allocated-p))
             (funcall function item))))
    (resources:map-resource (ecase keyword
                              (:stream 'http-stream)
                              (:server 'http-server)
                              (:process 'http-process))
                            #'fctn)))

(define all-servers (&aux servers)
  (flet ((collect (server)
           (push server servers)))
    (map-http-resource :server :allocated #'collect)
    servers))

(define %clear-http-process-resource ()
  "Deallocates all resourced HTTP processes."
  (map-http-resource :process :free #'ccl::process-kill)
  (resources:clear-resource 'http-process))

#|(defun kill-http-processes ()
   (flet ((fctn (p)
               (when (string-equal  "HTTP" (ccl::process-name p) :start1 0 :end2 3 :start2 0 :end2 3)
                   (ccl::process-kill-and-wait p))))
      (mapc #'fctn ccl::*all-processes*)))|#

(define shutdown-http-service ()
  "Forcibly shutdown all HTTP servers."
  (prog1 (disable-http-service :all)
         (map-http-resource :server :allocated #'shutdown-server)
         (close-all-logs)))

(define stop ()
  (disable-http-service))

(export 'stop :http)

;; force shutdown of all servers and free resources on exit from lisp.
(pushnew #'shutdown-http-service ccl:*lisp-cleanup-functions*
         :key #'ccl:function-name :test #'eq)

; Advise the lisp environment that we have MAC-CL-HTTP loaded.
(pushnew :mac-cl-http *features*)

;;;------------------------------------------------------------------- 
;;;
;;; debugging 
;;;

#|

(in-package :inspector)

(defparameter *process-name-function* #'ccl::process-name)

(defun inspect-processes-print-function (p s)
  (if (stringp p)
      (format s p)
      (let ((idle-time (if (eq p *current-process*)
                           0.0
                           (/ (ccl::%tick-difference
                                (ccl::get-tick-count)
                                (process-last-run-time p))
                              60.0)))
            (idle-units "s"))
        (when (>= idle-time 60)
          (setq idle-time (/ idle-time 60)
                idle-units "m")
          (when (>= idle-time 60)
            (setq idle-time (/ idle-time 60)
                  idle-units "h")
            (when (>= idle-time 24)
              (setq idle-time (/ idle-time 24)
                    idle-units "d"))))
        (format s "~a~30t~a~52t~3d~56t~8,2f~a~71t~5,1f"
                (funcall *process-name-function* p)
                (process-whostate p)
                (process-priority p)
                idle-time idle-units
                (* 100.0
                   (let ((ticks (- (#_TickCount) (process-creation-time p))))
                     (/ (process-total-run-time p)
                        (if (eql ticks 0) 1 ticks))))))))

(setq *process-name-function* #'http::http-process-name)

(in-package :http)

|#
