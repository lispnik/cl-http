;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

;;; Copyright John C. Mallery,  1995.
;;; All rights reserved.

(in-package :http)

;;;------------------------------------------------------------------- 
;;;
;;; SINGLE THREAD SERVER INTERFACE 
;;;

(defun schedule-task (name function run-interval)
   (declare (ignore name run-interval))
   (loop while (funcall function)))

(defun unschedule-task (name)
   (declare (ignore name)))

;;;------------------------------------------------------------------- 
;;;
;;;  STREAM AND SERVER RESOURCES
;;;

(defparameter *server-run-interval* 10.
   "The interval of execution quanta allocated for each scheduling of an HTTP thread.")

;; 5 minutes in 60ths of a second.
(setq *server-timeout* (the fixnum (* 60 60 5)))

;; buffer cache in the HTTP.
(defparameter *http-stream-buffer-size* 8192)   ; formerly 4096, default is 1024

(proclaim '(fixnum *http-stream-buffer-size*))

(defun http-stream-connection-state (stream)
   (ccl::tcp-state-name (ccl::tcp-connection-state stream)))

;; based on ccl::open-tcp-stream
#|(defun make-http-stream (resource &optional (port 80.) (timeout *server-timeout*))
   (declare (ignore resource))
   (let ((host nil)
           (element-type '(unsigned-byte 8))
           (tcpbufsize 8192)
           (rdsentries 6)
           (writebufsize *http-stream-buffer-size*)              ; default is 1024
           (notify-proc nil)
           (commandtimeout timeout))    ; default is 30
      (make-instance 'ccl:modal-ascii-or-binary-tcp-stream
          :element-type element-type
          :host host :port port
          :tcpbufsize tcpbufsize
          :rdsentries rdsentries
          :writebufsize writebufsize 
          :notify-proc notify-proc
          :commandtimeout commandtimeout)))|#

(defun make-http-stream (resource host &optional  port timeout)
   (declare (ignore resource host port timeout))
   (ccl::allocate-instance  (ccl::find-class 'ccl:modal-ascii-or-binary-tcp-stream)))

(defun initialize-http-stream (resource http-stream host &optional (port 80.) (timeout *server-timeout*))
   (declare (ignore resource)) 
   (initialize-instance http-stream
                                 :host host
                                 :port port
                                 :element-type '(unsigned-byte 8)
                                 :commandtimeout timeout
                                 :writebufsize *http-stream-buffer-size*)
   http-stream)                                  ; return the stream

(defun deinitialize-http-stream (resource stream)
   (case (http-stream-connection-state stream)
      (:closed)
      (t (close stream :abort t)
          (www-utils::notify-log-window  "~&While deallocating a ~S, unknown tcp stream state, ~S,  on ~S."
                                                             resource (http-stream-connection-state stream) stream)))
   stream)

(defmethod deinitialize-resourced-server :before (resource (server basic-server-mixin))
   (declare (ignore resource))
   (with-slots (stream) server
       (when stream
           (resources:deallocate-resource 'http-stream stream))))

(resources:defresource
   http-stream (host port timeout)
   :constructor make-http-stream
   :initializer initialize-http-stream
   :deinitializer deinitialize-http-stream
   :initial-copies 0)

(resources:defresource
   http-server ( stream host address)
   :constructor make-server
   :initializer initialize-resourced-server
   :deinitializer deinitialize-resourced-server
   :initial-copies 0)

(defvar *server-control-plist* nil
   "A property list of  streams awaiting incoming http connections on each port.")

(defun %register-listening-stream (stream &optional  (port (www-utils::foreign-port stream)))
   (setf (getf *server-control-plist* port) stream))

(defun %unregister-listening-stream (stream)
   (let  ((port (www-utils::foreign-port stream)))
      (when (eq stream (getf *server-control-plist* port))
          (setf (getf *server-control-plist* port) nil))))

(defun %listening-stream (&optional (port 80))
   (getf *server-control-plist* port))

(defun %stop-listening-stream (&optional (port 80))
   (let ((stream (%listening-stream port)))
      (when stream
          (resources:deallocate-resource 'http-stream (%listening-stream port))
          (%unregister-listening-stream stream))))

(defun %listen-for-connections (stream port timeout)
   (let ((*server-timeout* timeout))
      (loop while (listen-for-connections stream port))))

(defun %start-listening-for-connections (&optional (port 80.) (timeout *server-timeout*))
   ;; deallocate any stream that might be present.
   (%stop-listening-stream port)
   ;;allocate an http stream
   (let ((stream (resources:allocate-resource 'http-stream nil port 0)))
      ;; set the control stream so we know that we are listening for new connections.
      (%register-listening-stream stream port)
      ;; run the listening task
      (ccl:eval-enqueue `(%listen-for-connections ,stream ,port ,timeout))))

(defun %stop-listening-for-connections (&optional (port 80.))
   (let ((stream (%listening-stream port)))
      (when stream                                ; got a listening stream there?
          ;; unregister it
          (%unregister-listening-stream stream)
          ;; clean up appropriately.
          (case (http-stream-connection-state stream)
             ;; already running, so let it complete.
             (:established)
             ;; shut it down and deallocate.
             (t (resources:deallocate-resource 'http-stream stream))))))

(defun %provide-service (stream client-domain-name client-address)
   (flet ((log-dropped-connection (server)
               (www-utils::abort-http-stream stream)
               (set-server-status server 504)       ;time out status code
               (log-access server))
            (handle-unhandled-error (server error)
               (handler-case
                  (progn
                     (set-server-status server 500)
                     (log-access server)
                     (report-status-unhandled-error error stream (server-request
                                                                                             server))
                     (close stream))                  ; force output
                  (ccl:network-error ()))
               ;; log the access after the output has been forced.
               (log-access server)))
      ;; make sure we know our process.
      ;;Karsten 5/17/95 ccl:*current-process* is not defined in MCL 2.01, use #'ccl:toplevel-loop instead
      (setf (ccl:tcp-stream-process stream) #'ccl:toplevel-loop)
      (resources:using-resource
         (server http-server stream client-domain-name client-address)
         (let ((*server* server))
            (handler-case-if
              (not *debug-server*)
              (provide-service server)
              ;; catch aborts anywhere within server.-- JCMa 12/30/1994.
              (http-abort () (www-utils::abort-http-stream stream))
              (ccl:protocol-timeout () (log-dropped-connection server))
              (ccl:connection-lost () (log-dropped-connection server))
              (ccl:host-stopped-responding () (log-dropped-connection server))
              (ccl:bad-connection-state () (log-dropped-connection server))
              (error (error) (handle-unhandled-error server error)))))))

(define listen-for-connections (stream port)
   (declare (values continue-runing-p))
   (flet ((accept-connection-p ()
               (< *number-of-connections* *reject-connection-threshold*))
            (reject-connection (stream string)
               (when string
                   (write-string string stream)
                   (force-output stream))
               (close stream :abort t)))
      (declare (inline accept-connection-p reject-connection launch-listener))
      (cond ((listen stream)
                 ;; eventually elminitate translation to keyword.
                 (case (http-stream-connection-state stream)
                    (:established
                      (let* ((client-address (www-utils::foreign-host stream))
                                (client-domain-name (host-domain-name client-address)))
                         (cond ((accept-connection-p)
                                    ;; Remove from control plist.
                                    (%unregister-listening-stream stream)
                                    ;; set the timeout to the connected value
                                    (setf (ccl:timeout stream) *server-timeout*)
                                    ;; enqueue the service provision
                                    (ccl:eval-enqueue `(%provide-service ,stream ,client-domain-name ,client-address))
                                    ;; Launch another listening stream
                                    (ccl:eval-enqueue `(%start-listening-for-connections ,port ,*server-timeout*))
                                    nil)                    ; Done running this thread, deallocate thread.
                                  (t  (reject-connection stream *reject-connection-message*)
                                       t))))                ; Continue running 
                    ((:closed :listen :fin-wait-1) t)
                    (t (www-utils:notify-log-window "~&Unknown tcp stream state, ~S,  on ~S."
                                                                        stream (http-stream-connection-state stream))
                        nil)))
               ((%listening-stream port) t)          ; continue while stream control for port
               (t nil))))                            ; other stop listening

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

(define enable-http-service (&optional (on-ports (listening-on-http-ports)))
   "Top-level method for starting up HTTP servers."
   (dolist (port (setf (listening-on-http-ports) (ensure-list on-ports)))
      (ccl:eval-enqueue `(%start-listening-for-connections ,port 0)))
   on-ports)

(define start ()
   "Short version of ENABLE-HTTP-SERVICE."
   (enable-http-service))

(export 'start :http)

(define disable-http-service (&optional (on-ports :all))
   "Top-level method for shutting down HTTP servers."
   (case on-ports
      (:all
        ;; make sure no additional ports get started while shutting down.
        (ccl:without-interrupts
          (loop for port in *server-control-plist* by #'cddr
                   do (%stop-listening-for-connections port))))
      ;; Otherwise shut down each port specified
      (t (mapc #'%stop-listening-for-connections on-ports)))
   on-ports)

(defmethod shutdown-server ((server basic-server-mixin))
   "Forcibly shutdown a running (or idle) HTTP server."
   (with-slots (stream) server
       (when stream
           (close stream :abort t))
       (resources:deallocate-resource 'http-server server)))

(define map-http-servers (function)
   "Maps FUNCTION over all active HTTP servers."
   (flet ((fctn (server free-p resource)
               (declare (ignore resource))
               (unless free-p
                  (funcall function server))))
      (resources:map-resource 'http-server #'fctn)))

(define all-servers (&aux servers)
   (flet ((collect (server)
               (push server servers)))
      (map-http-servers #'collect)
      servers))

(define shutdown-http-service ()
   "Forcibly shutdown all HTTP servers."
   (disable-http-service :all)
   (map-http-servers #'shutdown-server))

(define stop ()
   (shutdown-http-service))

(export 'stop :http)

;; force shutdown of all server on exit from lisp
(pushnew #'shutdown-http-service ccl:*lisp-cleanup-functions*
               :key #'ccl:function-name :test #'eq)

; Advise the lisp environment that we have MAC-CL-HTTP loaded.
(pushnew :mac-cl-http *features*)
