;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

;;; Copyright John C. Mallery,  1995.
;;; All rights reserved.
;;;
;;; Copyright (C) 1995, OBC.
;;; All rights reserved on changes for portability to non MCL platforms.
;;;
;;; LispWorks enhancements Copyright (c) The Harlequin Group Limited, 1995.
;;;

;;;------------------------------------------------------------------- 
;;;
;;; MULTI-THREADED SERVER INTERFACE 
;;;

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;;  STREAM AND SERVER RESOURCES
;;;

(defparameter *number-of-listening-processes* #-MCL 1. #+MCL 5.
  "The number of threads simultaneously listening for HTTP connections.")

(defparameter *listener-process-priority* 0
  "The process priority of HTTP listener processes.")

(defparameter *server-run-interval* 12.
  "The interval of execution quanta allocated for each scheduling of an HTTP thread.")

;; 5 minutes in 60ths of a second.
(setq *server-timeout* (the fixnum (* 60 60 5)))

;; buffer cache in the HTTP.
(defparameter *http-stream-buffer-size* 4096 ;;  8192 , default is 1024
  "Controls the size of the HTTP stream buffer.
The default size for MACTCP is 1024, but 2048 is what Netscape uses.")

(declaim (fixnum *http-stream-buffer-size*))

(declaim (inline http-stream-connection-state))


;;;------------------------------------------------------------------- 
;;;
;;; 
;;; SET LISP ENVIRONMENT VARIABLES

;; how long to wait for wait-next-event when no event
#+CCL
(setq ccl::*idle-sleep-ticks* 1)

;; Same when lisp is a background MAC application.
#+CCL
(setq ccl::*background-sleep-ticks* 1)


;;;------------------------------------------------------------------- 
;;;
;;;  Resourced HTTP Processes
;;;

(defvar *process-index* 0)

(defvar *process-name-alist* nil)

;;; End CLIM-SYS bridge
;;;

#-(or LispWorks lcl4.2)
(defun %register-process-name (process mac-name name)
  (clim-sys:without-scheduling
    (push `(,process ,name ,mac-name) *process-name-alist*)))

#-(or LispWorks lcl4.2)
(defun %unregister-process-name (process)
  (clim-sys:without-scheduling
    (setq *process-name-alist* (delete process *process-name-alist* :key #'car)))
  process)

#-(or LispWorks lcl4.2)
(declaim (inline %http-process-name))

#-(or LispWorks lcl4.2)
(defun %http-process-name (process)
  (second (assoc process *process-name-alist*)))

(defun http-process-name (process)
  (or #-(or LispWorks lcl4.2) (%http-process-name process)
      (clim-sys:process-name process)))

#-(or LispWorks lcl4.2)
(defun make-http-process (resource name)
  (declare (ignore resource name) (values process))
  (let* ((idx (incf *process-index*))
	 (mac-name (concatenate 'string "HTTP Process " (with-output-to-string (str)
							  (write idx :escape nil :base 10. :stream str)))))
    (clim-sys:make-process-loop :name mac-name)))

#-(or LispWorks lcl4.2)
(defun initialize-http-process (#+ignore resource process name)
  (%register-process-name process (clim-sys:process-name process) name)
  process)

(defun http-process-shutdown (process)
  #-(or LispWorks lcl4.2)
  (when (%http-process-name process)            ; prevents multiple deallocation
    #|(notify-log-window "~&Shutdown Process ~A" process)|#
    (clim-sys:deallocate-resource 'http-process process)
    process)
  #+(or LispWorks lcl4.2)
  (clim-sys:destroy-process process)) 

#-(or LispWorks lcl4.2)
(defun deinitialize-http-process (#+ignore resource process)
  (%unregister-process-name process)
  (clim-sys:destroy-process process)
  process)

#+CCL
(defun launch-process (name keywords function &rest args)
  (declare (values process)
           #|(arglist &key priority quantum)|#)
  (destructuring-bind (&key (priority 0) (quantum *server-run-interval*)) keywords
    (setq priority (ccl::require-type priority 'fixnum)
          quantum (ccl:require-type quantum 'fixnum))
    (let ((process (clim-sys:allocate-resource 'http-process name)))
      (ccl::process-preset 
        process
        #'(lambda (process function args)
            (unwind-protect
                (let* ((tag (ccl::process-reset-tag process)))
                  (catch tag
                    (ccl::with-standard-abort-handling "Exit HTTP Process"
                                                       (apply function args))))
              (http-process-shutdown process)))
        process function args)
      (ccl::process-enable process)
      (values process))))

#+CLIM-SYS ;; Using MINIPROC this would deadlock
(defun launch-process (name keywords function &rest args)
  (declare (ignore keywords))
  (clim-sys:make-process #'(lambda ()
			     (unwind-protect (apply function args)
			       (http-process-shutdown (clim-sys:current-process))))
			 :name name))


;;;------------------------------------------------------------------- 
;;;
;;; Resourced HTTP Streams 
;;;

(declaim (inline http-stream-connection-state))

(defun http-stream-connection-state (stream)
  "Returns a Standard keyword describing the connection state of stream."
  #+CCL
  (ccl::tcp-state-name (ccl::tcp-connection-state stream))
  #-CCL ;Only two states required - see callers
  (cond ((live-connection-p stream) :established)
	(t :closed)))

#-CCL
(defun tcp-connection-state (stream)
  (cond ((listen stream) 8)
	((live-connection-p stream) 0)
	(t 2)))

#+CCL
(defun make-http-stream (resource host &optional port timeout process)
  (declare (ignore resource host port timeout process))
  (ccl::allocate-instance  (ccl::find-class 'ccl:modal-ascii-or-binary-tcp-stream)))

#+CCL
(defmacro http-stream-process (stream)
  `(ccl:tcp-stream-process ,stream))

#+Allegro
(defclass http-stream ()
  ((host :initarg :host)
   (port :initarg :port)
   (effective-stream :initarg :stream)
   (element-type :initarg :element-type)
   (commandtimeout :initarg :commandtimeout)
   (writebufsize :initarg :writebufsize)
   (process :accessor http-stream-process)))

#-(or CCL LispWorks lcl4.2)
(defun make-http-stream (resource host &optional port timeout process)
  (declare (ignore resource host port timeout process))
  (allocate-instance (find-class 'http-stream)))

#-(or LispWorks lcl4.2)
(defun initialize-http-stream (http-stream host &optional (port *STANDARD-HTTP-PORT*) (timeout *server-timeout*) (process (clim-sys:current-process)))
  (initialize-instance http-stream
                       :host host
                       :port port
                       :element-type '(unsigned-byte 8)
                       :commandtimeout timeout
                       :writebufsize *http-stream-buffer-size*)
  ;; initialize the process slot here in 3.0a but move into initialize-instance
  ;; later   2/23/95 -- JCMa.
  (setf (http-stream-process http-stream) process)
  http-stream)                                  ; return the stream

#-(or LispWorks lcl4.2)
(defun deinitialize-http-stream (stream)
  (case #+CCL (ccl::tcp-connection-state  stream) #-CCL (tcp-connection-state stream)
    (0)
    ((2) #+CCL (ccl::stream-close stream) #-CCL (close stream :abort t))
    (t (notify-log-window  "While deallocating a ~S, unknown tcp stream state, ~S (~D),  on ~S."
                           'http-stream
			   (http-stream-connection-state stream) 
                           #+CCL (ccl::tcp-connection-state  stream)
			   #-CCL (tcp-connection-state stream) stream)
       (close stream :abort t)))
  ;; deallocate process
  (shutdown-http-stream-process stream)
  (setf (http-stream-process stream) nil)
  stream)

;;; Resource arg required from server;server
;;;
(defmethod deinitialize-resourced-server :before (resource (server basic-server-mixin))
  (declare (ignore resource))
  (with-slots (stream) server
    (when stream
      #+(or Allegro LispWorks lcl4.2)
      (multiple-value-bind (what error)
	  (ignore-errors (close stream))
	(cond (error
	       (setf s stream)
	       (break "Error ~s" stream)
	       (warn "Ignoring error")
	       (describe error #-LispWorks *trace-output*))
	      (t what)))
      #-(or Allegro LispWorks lcl4.2)
      (clim-sys:deallocate-resource 'http-stream stream))))

#-(or LispWorks lcl4.2)
(clim-sys:defresource
  http-stream (host port timeout process)
  :constructor make-http-stream
  :initializer initialize-http-stream
  :deinitializer deinitialize-http-stream
  :initial-copies 0)

;;; From server;server.lisp
#||
(define-generic initialize-resourced-server (server set-stream set-host set-address))

(defmethod initialize-resourced-server ((server basic-server-mixin) set-stream set-host set-address)
  (declare (ignore resource))
  (with-slots (stream address host) server
    (setq stream set-stream
          address set-address
          host set-host)
    server))

(define-generic deinitialize-resourced-server (server))

(defmethod deinitialize-resourced-server ((server basic-server-mixin))
 (declare (ignore resource))
  (with-slots (stream address host request method url status) server
    (setq stream nil
          address nil
          host nil))
  server)


(defmethod deinitialize-resourced-server :after ((server server-logging-mixin))
 (declare (ignore resource))
  (with-slots (request method url url-string status
                       access-log-stream error-log-stream) server
    (setq request nil
          method nil
          url-string nil
          url nil
          status nil
          access-log-stream nil
          error-log-stream nil))
  server)

(defmethod deinitialize-resourced-server :after ((server server-authentication-mixin))
  (declare (ignore resource))
  (with-slots (rfc-931-response-type rfc-931-response user-name) server
    (setq rfc-931-response-type nil
          rfc-931-response nil
          user-name nil)
    server))
||#

(defun patch-initialize-resourced-server (server set-stream set-host set-address)
  (initialize-resourced-server 'http-server server set-stream set-host set-address))

(defun patch-deinitialize-resourced-server (server)
  (deinitialize-resourced-server 'http-server server))

(clim-sys:defresource
  http-server (stream host address)
  :constructor make-server
  :initializer patch-initialize-resourced-server
  :deinitializer patch-deinitialize-resourced-server
  :initial-copies 0)

#-(or LispWorks lcl4.2)
(clim-sys:defresource
  http-process (name priority quantum)
  :constructor make-http-process
  :initializer initialize-http-process
  :deinitializer deinitialize-http-process
  :initial-copies 0)


;;;------------------------------------------------------------------- 
;;;
;;; thread mechanism 
;;;

#-(or LispWorks lcl4.2)
(defmethod shutdown-http-stream-process ((tcp-stream #+CCL ccl:modal-ascii-or-binary-tcp-stream #-CCL http-stream))
  (with-slots #+CCL ((process ccl::process)) #-CCL (process) tcp-stream
    (when process
      (http-process-shutdown process))))

(defmethod enable-server ((server basic-server-mixin)) 
  (with-slots (stream) server
    (with-slots #+CCL ((process ccl::process)) #-CCL (process) stream
      (when (and stream process)
        (clim-sys:enable-process process)))))

(defmethod disable-server ((server basic-server-mixin))
  (with-slots (stream) server
    (with-slots #+CCL ((process ccl::process)) #-CCL (process) stream
      (when (and stream process)
        (clim-sys:disable-process process)))))

;;;------------------------------------------------------------------- 
;;;
;;; LISTENING STREAMS
;;;

(defvar *server-control-alist* nil
  "A property list of  streams awaiting incoming http connections on each port.")

(defun %register-listening-stream (stream &optional  (port (www-utils:foreign-port stream)))
  (clim-sys:without-scheduling
    (let ((entry (assoc port *server-control-alist* :test #'eql)))
      (cond (entry (push stream (cdr entry)))
            (t (push `(,port ,stream) *server-control-alist*))))))

(defun %swap-listening-stream (new-stream old-stream 
                                          &optional (port (www-utils:foreign-port old-stream)))
  (clim-sys:without-scheduling
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
             (clim-sys:without-scheduling
               (if (cddr entry)
                   (setf (cdr entry) (delete s (cdr entry) :test #'eq))
                   (setq *server-control-alist* (delete entry *server-control-alist* :test #'eq))))
             t)))
    (let  ((port (www-utils:foreign-port stream)))
      (cond (port
             (let ((entry (assoc port *server-control-alist* :test #'eql)))
               (delete-from-entry stream entry)))
            ;; port may be null when the stream is closed, so search for it.
            (t (loop for entry in *server-control-alist*
                     when (delete-from-entry stream entry)
                       do (return t)
                     finally (return nil)))))))

(declaim (inline %listening-streams))

(defun %listening-streams (&optional (port (round *STANDARD-HTTP-PORT*)))
  "Returns the streams listening on PORT."
  (cdr (assoc port *server-control-alist* :test #'eql)))

(defun %map-listening-streams (function &optional (ports :all))
  "Maps FUNCTION over streams listening on PORTS,
which is either :ALL or a list of port numbers."
  (loop for (port . streams) in *server-control-alist*
        do (when (and streams 
                      (or (eq ports :all) (member port ports :test #'eql))) 
             (mapc function streams))))

(declaim (inline %listening-on-port-p))

(defun %listening-on-port-p (&optional (port (round *STANDARD-HTTP-PORT*)))
  "Returns non-null when listening for HTTP connections on PORT."
  (%listening-streams port))

(declaim (inline %listening-on-ports))

(defun %listening-on-ports ()
  "Returns the port numbers on which there is listening for HTTP connections."
  (mapcar #'car *server-control-alist*))

(defun %shutdown-idle-listening-stream (stream)
  (declare (values shutdown-p))
  ;; shut down only when idle
  (clim-sys:without-scheduling                       ; keep this atomic
    (let ((state (http-stream-connection-state stream)))
      (case state
        ;; already running, so let it complete.
        (:established nil)
        ;; shut it down and deallocate.
        (t ;; unregister it
          (%unregister-listening-stream stream)
          (unless (member state '(:closed))
            (close stream :abort t))
	  #-(or LispWorks lcl4.2)
          (clim-sys:deallocate-resource 'http-stream stream)
          t)))))

;; Calling this function within a stream thread requires issuing a
;; process-run-function 2/23/95 -- JCMa.
(defun %stop-listening-for-connections (&optional (port *STANDARD-HTTP-PORT*) &aux more-streams-p)
  (flet ((careful-shutdown (stream)
           (unless (%shutdown-idle-listening-stream stream)
             (setq more-streams-p t))))
    (when (%listening-on-port-p port)
      (let ((ports (list port)))
        (declare (dynamic-extent ports))
        (loop doing 
          (%map-listening-streams #'careful-shutdown ports)
          (if more-streams-p
              (setq more-streams-p nil)
              (return)))
        ;; Advise the user. 
        (expose-log-window)
        (notify-log-window "HTTP service disabled for: http://~A:~D/"
                           (www-utils:local-host-domain-name) port)))
    (values t)))

(defun %enforce-number-of-listeners-for-connection (number &optional (port *STANDARD-HTTP-PORT*) 
                                                           (timeout *server-timeout*))
  (let* ((s (%listening-streams port))
         (len (length s)))
    (cond ((> len number)
           (loop with idx = (- number len)
                 for streams = s then (%listening-streams port)
                 do (loop for stream in streams
                          when  (%shutdown-idle-listening-stream stream)
                            do (when (zerop (decf idx))
                                 (return-from %enforce-number-of-listeners-for-connection number)))))
          ((< 0 len number)
           (dotimes (idx (- number len))
             (%start-listening-for-connections port timeout))))
    number))

#+MCL
(defun %listen-for-connections (port timeout)
  (flet ((allocate-stream (&optional old-stream)    ;;allocate an http stream
           (let ((new-stream (clim-sys:allocate-resource 
                               'http-stream nil port 0 (clim-sys:current-process))))
             ;; set the control stream so we know that we are listening for new connections.
             (if old-stream
                 (%swap-listening-stream new-stream old-stream port)
                 (%register-listening-stream new-stream port))
             new-stream)))
    (declare (inline allocate-stream))
    (loop with stream = (allocate-stream)
          doing (let ((*server-timeout* timeout)) 
                  (loop doing (ecase (listen-for-connection stream port)
                                (:continue)
                                (:block
				    #+CCL
                                  (ccl::suspend-current-process "Listen Wait"))
                                (:new-stream
                                  (setq stream (allocate-stream stream))
                                  (return))
                                (:quit (return-from %listen-for-connections))))))))

#+Allegro
(defvar *acl-tcp-servers* nil)

#+Allegro
(defun %listen-for-connections (port timeout)
  (declare (ignore timeout))
  (let ((stream (open "" :class 'ipc::tcp-server-stream :port port)))
    (push stream *acl-tcp-servers*)
    (unwind-protect (loop
		      (clim-sys:process-wait "Wait for new connection"
					     #'(lambda () (listen stream)))
		      (listen-for-connection stream port))
      (close stream :abort t))))

#+(or LispWorks lcl4.2)
(defun %listen-for-connections (port timeout)
  (declare (ignore timeout))
  (ipc:%listen-for-connections port 'listen-for-connection))


#+(or Allegro LispWorks lcl4.2)
(defvar *clim-sys-http-processes* nil)

(defun %start-listening-for-connections (&optional (port *STANDARD-HTTP-PORT*) (timeout *server-timeout*)
                                                   (process-priority *listener-process-priority*))
  "Primitive to begin listening for HTTP connections on PORT with server timeout, TIMEOUT."
  ;; run the listening thread
  (flet ((process-namestring ()
           (concatenate 'string "HTTP Listen [" (write-to-string port :base 10.) "]")))
    (declare (inline process-namestring))
    (let ((keywords `(:priority ,process-priority))
	  process)
      (declare (dynamic-extent keywords))
      (setq process (launch-process (process-namestring) keywords #'%listen-for-connections port timeout))
      #+(or Allegro LispWorks lcl4.2)
      (push process *clim-sys-http-processes*))))

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
               (report-status-unhandled-error error stream (server-request server))
               (close stream))                  ; force output
             (#+CCL ccl:network-error #-CCL error ()))
           ;; log the access after the output has been forced.
           (log-access server)))
    ;; make sure we know our process.
    #+Allegro
    (setf (ipc::tcp-stream-process stream) (clim-sys:current-process))
    #-(or Allegro LispWorks lcl4.2)
    (setf (http-stream-process stream) (clim-sys:current-process))
    (clim-sys:using-resource
     ;; Notice the resource name HTTP-SEVER is NOT quoted
      (server http-server stream client-domain-name client-address)
      (let ((*server* server)) 
        (handler-case-if
            (not *debug-server*)
           (progn 
             (provide-service server)
             (close stream))
          ;; catch aborts anywhere within server.-- JCMa 12/30/1994.
          (http-abort () (www-utils:abort-http-stream stream))
          #+CCL (ccl:protocol-timeout () (log-dropped-connection server))
          #+CCL (ccl:connection-lost () (log-dropped-connection server))
          #+CCL (ccl:host-stopped-responding () (log-dropped-connection server))
          #+CCL (ccl:bad-connection-state () (log-dropped-connection server))
          (error (error) (handle-unhandled-error server error)))))))

#+Allegro
(defvar *acl-listens* nil)

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
    (cond 
     (#+CCL
      (ccl::stream-listen stream)
      #-(or CCL LispWorks lcl4.2) (listen stream)
      #+LispWorks t ;LW has already made the connection to the peer
      #+lcl4.2 t
      #+Allegro
      (setq stream (ipc::stream-read stream)) ;Get new connection stream
      ;;#+Allegro
      ;;(pushnew stream *acl-listens*) ;Debugging only
      (case #+CCL (ccl::tcp-connection-state stream)
            #+(or LispWorks lcl4.2) 8
	    #-(or CCL LispWorks lcl4.2) (tcp-connection-state stream)
         (8 ;; :established
           (cond ((accept-connection-p)
                  (let* ((client-address (www-utils::foreign-host stream))
                         (client-domain-name (host-domain-name client-address)))
		    ;; set the timeout to the connected value
		    #+CCL
                    (setf (ccl:timeout stream) *server-timeout*
                          (http-stream-process stream) nil)  ;set to NIL pending launch
		    ;;#+Allegro
		    ;;(format *trace-output* "~&Launching new HTTP listener...~%")
                    ;; start a thread to provide http service
                    (launch-process (process-namestring client-domain-name port)
                                    '(:priority 0)
                                    #'%provide-service stream client-domain-name client-address)
                    :new-stream))               ; request new listening stream
                 (t (reject-connection stream *reject-connection-message*)
                    :block)))                   ; Continue running 
         (t :continue)))
      ((%listening-on-port-p port) :block)
      (t :quit))))

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

#+CCL
(define disable-http-service (&optional (on-ports :all))
  "Top-level method for shutting down HTTP servers."
  (mapc #'%stop-listening-for-connections
        (case on-ports
          (:all (%listening-on-ports))
          (t on-ports))))

#+(or Allegro LispWorks lcl4.2)
(defun disable-http-service (&optional ignore)
  (declare (ignore ignore))
  #+Allegro
  (mapc #'(lambda (x)
	    (ignore-errors (close x)))
	*acl-tcp-servers*)
  #+Allegro
  (setq *acl-tcp-servers* nil)
  (mapc #'(lambda (x)
	    (ignore-errors (clim-sys:destroy-process x)))
	*clim-sys-http-processes*)
  (setq *clim-sys-http-processes* nil))

(defmethod shutdown-server ((server basic-server-mixin))
  "Forcibly shutdown a running (or idle) HTTP server."
  (with-slots (stream) server
    (when stream
      (close stream :abort t))
    (clim-sys:deallocate-resource 'http-server server)))

;;; I think function as first argument makes more
;;; sense for all MAP type functions.
;;; Notice CLIM-SYS:MAP-RESOURCE args.
;;;
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
    (clim-sys:map-resource #'fctn
		  (ecase keyword
		    #-(or LispWorks lcl4.2)
		    (:stream 'http-stream)
		    (:server 'http-server)
		    #-(or LispWorks lcl4.2)
		    (:process 'http-process)))))

(define all-servers (&aux servers)
  (flet ((collect (server)
           (push server servers)))
    (map-http-resource :server :allocated #'collect)
    servers))

#-(or LispWorks lcl4.2)
(define %clear-http-process-resource ()
  "Deallocates all resourced HTTP processes."
  (map-http-resource :process :free #'clim-sys:destroy-process)
  (clim-sys:clear-resource 'http-process))

(define shutdown-http-service ()
  "Forcibly shutdown all HTTP servers."
  (prog1 (disable-http-service :all)
         (map-http-resource :server :allocated #'shutdown-server)
         (close-all-logs)))

(define stop ()
  (disable-http-service))

;; force shutdown of all servers and free resources on exit from lisp.
#+CCL
(pushnew #'shutdown-http-service ccl:*lisp-cleanup-functions*
         :key #'ccl:function-name :test #'eq)
