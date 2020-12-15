;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10; Default-character-style: (:fix :roman :normal) -*-

;;;
;;; (c) Copyright  1996-99, John C. Mallery
;;;     All Rights Reserved.
;;;
;;;------------------------------------------------------------------- 
;;;
;;; CLIENT PERSISTENT CONNECTIONS
;;;

(in-package :http) 

(define-macro with-current-connection ((client &key (http-version '*client-http-version*) (connection-var 'connection)) &body body)
  "Establishes the current connection context and maintains consistent connections."
  `(let* ((,connection-var (ensure-client-connection ,client ,http-version))
	  (*connection* ,connection-var)
	  (incomplete-transaction-p t))
     (declare (special *connection*))
     (unwind-protect
	 (multiple-value-prog1
	   (progn . ,body)
	   (setq incomplete-transaction-p nil))
       ;; When an HTTP transaction fails to terminate normally or it
       ;; needs to be closed, do it here. Otherwise, recheck on next
       ;; time around the loop in ensure-client-connection.
       (when (or incomplete-transaction-p (connection-close-p ,connection-var))
	 (deallocate-connection ,client)))))

(declaim (inline current-connection))

(define current-connection ()
  "Returns the current connection object for a client HTTP transaction."
  (symbol-value '*connection*)) 

;;;------------------------------------------------------------------- 
;;;
;;; PERSISTENT CONNECTION CLASSES
;;;

(defclass connection-pool-mixin
          ()
    ((free-since :initform 0 :initarg :free-since :accessor connection-free-since)
     (timeout :initform 0 :initarg :timeout :accessor connection-timeout)
     (close-time :initform 0 :initarg :close-time :accessor connection-close-time)
     (requests-completed :initform 0 :accessor connection-requests-completed)
     (requests-allowed :initform 0 :accessor connection-requests-allowed)
     (next :initform nil :accessor connection-next))
  (:documentation "A mixin that manages when connections close or are garbage collected."))

(defclass basic-connection
          (connection-pool-mixin)
    ((stream :initarg :stream :accessor connection-stream)
     (close-p :initform nil :initarg :close-connection-p :accessor connection-close-p)
     (domain-name :initarg :domain-name :accessor connection-domain-name)
     (host :initarg :host :accessor connection-host)
     (port :initarg :port  :accessor connection-port)
     (state :initform :closed :initarg :state :accessor %connection-state)
     (version :initform nil :initarg server-version :accessor connection-version))
  (:documentation "The basic persistent connection infrastrcture for the client side."))

(defclass connection
          (basic-connection)
    ()
  (:documentation "A client-side HTTP connection."))

(defmethod print-object ((connection basic-connection) stream)
  (with-slots (domain-name port state) connection
    (print-unreadable-object (connection stream :type t :identity t)
      (format stream "{~D} " (connection-requests-completed connection))
      (cond-every
        (domain-name (write-string domain-name stream))
        (port
          (format stream ": ~D" port))
        (state
          (format stream " [~A]" state)))       
      connection)))

;;;------------------------------------------------------------------- 
;;;
;;; ALLOCATION AND DEALLOCATION
;;;

(declaim (inline %make-connection))

(defun %make-connection (resource host port stream &optional domain-name)
  (declare (ignore resource))
  (make-instance 'connection
                 :host host
                 :port port
                 :domain-name domain-name
                 :stream stream))

;; Only resourcing in genera and MCL pass in the resource to the initializer.
(defun %initialize-connection (resource connection host port stream &optional domain-name)
  (declare (ignore resource))
  (setf (connection-stream connection) stream
        (connection-host connection) host
        (connection-port connection) port
        (connection-domain-name connection) domain-name)
  connection)

(defun %match-connection-p (resource connection host port stream &optional domain-name)
  (declare (ignore resource connection host port stream domain-name))
  t) 

(defresource http-connection (host port stream &optional domain-name)
  :matcher %match-connection-p
  :constructor %make-connection
  :initializer %initialize-connection)

(define clear-connection-resource ()
  "Clears the resource of HTTP connection objects."
  (clear-resource 'http-connection)) 

(define allocate-connection (host port &optional domain-name)
  "Allocates a new connection to HOST on PORT with DOMAIN-NAME."
  (declare (values connection stream new-connection-p))
  (let* ((stream (open-http-stream-to-host host port))
         (conn (allocate-resource 'http-connection host port stream domain-name)))
    (setf (%connection-state conn) :open
	  (connection-close-p conn) nil
	  (connection-timeout conn) *client-persistent-connection-timeout*
	  (connection-requests-allowed conn) *client-persistent-connection-number*)
    (atomic-incf *connections-allocated*)
    (client-trace "~&Allocate Connection (~D): ~S" *connections-allocated* conn)
    (values conn stream t)))

(defun %deallocate-client-http-stream (stream &optional (abort-p t))
  (close stream :abort abort-p)
  (prog1 (deallocate-client-http-stream stream)
         (atomic-incf *connections-deallocated*)
	 (client-trace "~&Deallocate Stream (~D): ~S" *connections-deallocated* stream)))

(define-generic deallocate-connection (connection-or-client)
  (declare (values connection))
  (:documentation  "Deallocates CONNECTION-OR-CLIENT to the connection resoure, ensuring that stream is closed."))

(defmethod deallocate-connection ((connection basic-connection))
  (with-slots (stream state host port close-p domain-name version) connection
    ;; make sure that open streams are deallocated
    (ecase state
      ((:open :closed)
       (setq state :deallocated)
       (when stream
         (%deallocate-client-http-stream stream)
         (setq stream nil))
       ;; reset instance variables for GC and fast allocation.
       (setq host nil
             port 80
             close-p nil
             domain-name nil
             version nil))
      (:deallocated))
    (when stream (error "Stream not Dellocated"))))

(defmethod deallocate-connection :around ((connection basic-connection))
  (client-trace "~&Deallocate: ~S" connection)
  (call-next-method)
  (deallocate-resource 'http-connection connection)
  connection)

(defmethod deallocate-connection :after ((connection connection-pool-mixin))
  (with-slots (free-since timeout close-time requests-completed requests-allowed next) connection
    (setq free-since 0
          timeout 0
          close-time 0
          requests-completed 0
          requests-allowed 0
          next nil)))

;;;------------------------------------------------------------------- 
;;;
;;; OPENNING AND CLOSING CONNECTIONS
;;;

(define-generic open-connection (connection)
  (declare (values connection))
  (:documentation "Ensures that CONNECTION has an open HTTP connection,
reopenning it as necessary."))

(defmethod open-connection ((connection basic-connection))
  (with-slots (host port state stream) connection
    (ecase state
      (:closed
        (unless stream
          (error "Closed connections should have no stream."))
        (setq stream (open-http-stream-to-host host port)
              state :open))
      (:open
        (unless (and stream (live-connection-p stream))
          (when stream
            (%deallocate-client-http-stream stream))
          (setq stream (open-http-stream-to-host host port)))))
    connection))

(define-generic close-connection (connection &optional abort)
  (:documentation "Ensures that connection is closed.
If open, abort controls whether it is closed in abort mode or not."))

(defmethod close-connection ((connection basic-connection) &optional abort)
  (with-slots (stream state) connection
    (ecase state
      (:open
        (setf state :closed)
        (when stream
          (%deallocate-client-http-stream stream abort)
          (setf stream nil)))
      (:closed))
    connection))

(define-generic connection-version-string (connection)
  (:documentation "Returns the HTTP version of CONNECTION as a string."))

(defmethod connection-version-string ((connection basic-connection))
  (let ((version (connection-version connection)))
    (when version
      (symbol-name version)))) 

;;;------------------------------------------------------------------- 
;;;
;;; ASCERTAINING CONNECTION STATE
;;;

(define-generic connection-state (connection)
  (declare (values state-keyword))
  (:documentation "Returns the connection state of connection,
which can be any of :OPEN, :CLOSED, or :DEALLOCATED"))

(defmethod connection-state ((connection basic-connection))
  (with-slots (state stream) connection
    (ecase state
      (:open
        (cond ((null stream) (setq state :closed))
              ((live-connection-p stream) :open)
              (t (setq state :closed)
                 (%deallocate-client-http-stream stream)
                 (setq stream nil)
                 :closed)))
      (:closed :closed)
      (:deallocated :deallocated))))

;;;------------------------------------------------------------------- 
;;;
;;; MATCHING CONNECTIONS
;;;

(declaim (inline %connection-to-host-port-p))

(defun %connection-to-host-port-p (connection host port)
  (and (equal (connection-host connection) host)
       (= (connection-port connection) port)))

(define-generic connection-to-host-port-p (connection host port)
  (:documentation "Returns non-null if CONNECTION is an http connection to HOST on PORT."))

(defmethod connection-to-host-port-p ((connection basic-connection) host port)
  (%connection-to-host-port-p connection host port))

;;;------------------------------------------------------------------- 
;;;
;;; MAINTAIN A POOL OF OPEN HTTP CONNNECTIONS TO HOSTS
;;;

(defvar *connection-table* (make-hash-table :test #'equal))

(defvar *connection-lock* (www-utils:make-lock "Client Connection")
  "The lock used to avoid thread collisions in the connection pool.")

(defmacro with-connection-pool-lock ((&key (mode :write)) &body body)
  `(www-utils:with-lock-held (*connection-lock* ,mode "HTTP Connection Wait") ,@body))

(defun map-connection-pool (function)
  (loop for entry being the hash-values of *connection-table*
        when entry
          do (loop for conn = entry then (connection-next conn)
                   while conn
                   do (funcall function conn))))

(defun push-connection-pool (connection)
  (let ((host (connection-host connection))
	(table *connection-table*))
    (with-connection-pool-lock (:mode :write)
      (let ((entry (gethash host table)))
	(when entry
	  (setf (connection-next connection) entry))
	(setf (gethash host table) connection)))))

(defun %deallocate-host-connections (connection)
  (let ((next (connection-next connection)))
    (when next
      (%deallocate-host-connections next))
    (deallocate-connection connection)))

(defun deallocate-all-connections (&aux (table *connection-table*))
  (with-connection-pool-lock (:mode :write)
    (loop for conn being each hash-value in table
          do (%deallocate-host-connections conn)
          finally (clrhash table))))

(defun %pop-connection-from-pool (connection port)
  (declare (values connection new-top-conn))
  (loop with new-top-conn and prev
        for conn = connection then next
        for next = (and conn (connection-next conn))
        while conn
        do (cond ((= port (connection-port conn))
		  (if prev
		      (setf (connection-next prev) next)
		      (setq new-top-conn (or next :clear-entry)))
		  (ecase (connection-state conn)
		    (:open
		      (return-from %pop-connection-from-pool
			(values conn new-top-conn)))
		    (:closed
		      (deallocate-connection conn))))
		 (t (setq prev conn)))
        finally (return (values nil new-top-conn))))

(define get-connection (host port &optional domain-name &aux (table *connection-table*))
  "Returns a client connection to a server from the connection pool."
  (declare (values connection stream new-connection-p))
  (when *client-persistent-connections*
    (with-connection-pool-lock (:mode :write)
      (multiple-value-bind (entry found-p)
          (gethash host table) 
        (cond (entry
               (multiple-value-bind (conn new-top-conn)
                   (%pop-connection-from-pool entry port)
                 (cond-every
                   (new-top-conn
                     (case new-top-conn
		       (:clear-entry (remhash host table))
		       (t (setf (gethash host table) new-top-conn))))
                   (conn
                     (let ((stream (connection-stream conn)))
                       (clear-input stream)
                       (clear-output stream)
                       (setf (connection-domain-name conn) domain-name
			     (connection-next conn) nil
			     (connection-free-since conn) 0)	;mark as in use
                       (return-from get-connection (values conn stream)))))))
              (found-p
               (remhash host table))))))
  ;; If no live connection available allocate a new one.
  (allocate-connection host port domain-name))

(defmethod note-free-connection ((connection connection-pool-mixin))
  (with-slots (requests-completed requests-allowed free-since close-time timeout) connection
    (cond ((zerop (connection-free-since connection))
	   (let* ((time (get-universal-time))
		  (close (+ time (or timeout *client-persistent-connection-timeout*))))
	     (setf free-since time
		   close-time close)
	     (push-connection-pool connection)))
	  (t (error "Attempt to note free connection that is already free for ~S." connection)))))

(defmethod return-connection ((connection connection-pool-mixin))
  (if (or (not *client-persistent-connections*)	;use persistent connections?
	  (eql :closed (connection-state connection))	;already closed?
	  (connection-close-p connection)	;instructions to close?
	  (> (incf (connection-requests-completed connection))	;exceeding http 1.0 request limit?
	     (connection-requests-allowed connection)))
      (deallocate-connection connection)
      (note-free-connection connection)))

(define update-connection-status-from-headers (version &optional (headers *headers*)
                                                       (conn (current-connection)))
  "Updates the current client connection according to the VERSION  and HEADERS sent by the server."
  (case (setf (connection-version conn) version)
    (:http/0.9
      (setf (connection-close-p conn) t))
    (:http/1.0
      (let ((connection (get-header :connection headers))
            (keep-alive (get-header :keep-alive headers)))
        (cond ((member :close connection)
	       (setf (connection-close-p conn) t))
	      ((member :keep-alive connection)
	       (if keep-alive
		   (destructuring-bind (&key timeout max)
		       keep-alive
		     (cond-every
		       ((and timeout (< timeout *client-persistent-connection-timeout*))
			(setf (connection-timeout conn) timeout))
		       ((and max (< max *client-persistent-connection-number*))
			(setf (connection-requests-allowed conn) max))))
		   (setf (connection-close-p conn) t))))))
    (t (let ((connection (get-header :connection headers)))
	 (when (member :close connection)
	   (setf (connection-close-p conn) t))))))

;;;------------------------------------------------------------------- 
;;;
;;; CONNECTION SCAVENGER
;;;

(defun %gc-host-connections (connection time)
  (declare (values next-live-connection))
  (let ((next (connection-next connection))
        new-next)
    (when next
      (setq new-next (%gc-host-connections next time)))
    (ecase (connection-state connection)
      (:open
        (cond ((> (connection-close-time connection) time)
               (setf (connection-next connection) new-next)
               connection)
              (t (deallocate-connection connection)
                 new-next)))
      (:closed
        (deallocate-connection connection)
        new-next)
      (:deallocated new-next))))

(define gc-connection-pool (&aux (table *connection-table*))
  "Maps over the connection pool and deallocates any connections that have timeout."
  (when (and table (not (zerop (hash-table-count table))))
    (let ((replace-top-level nil)
          (remove-list nil)
          (time (get-universal-time)))
      (declare (dynamic-extent replace-top-level remove-list))
      (with-connection-pool-lock (:mode :write)
        (loop for conn being each hash-value in table
                       using (hash-key host)
              do (cond (conn
                        (let ((n-conn (%gc-host-connections conn time)))
                          (cond ((and n-conn (equal n-conn conn)))
                                (n-conn
                                 (push n-conn replace-top-level)
                                 (push host replace-top-level))
                                (t (push host remove-list)))))
                       (t (push host remove-list))))
        (cond-every
          (remove-list
            (dolist (key remove-list)
              (remhash key table)))
          (replace-top-level
            (loop for (host value) on replace-top-level by #'cddr
                  do (setf (gethash host table) value))))))))

(defvar *connection-scavenger* nil
  "A process that cleans up hanging connections to hosts.")

(defun scavenge-connections-main-loop ()
  (flet ((connections-to-scavenge-p ()
           (not (and *connection-scavenger-on*
                     (zerop (hash-table-count *connection-table*)))))
         (false ()
           (not *connection-scavenger-on*)))
    (loop with process = *connection-scavenger* 
          doing (process-wait
                  "Scavenge Wait" #'connections-to-scavenge-p)
                (process-wait-with-timeout
                  "Scavenge Wait" *client-persistent-connection-timeout* #'false)
                (handler-case-if (not *debug-client*)
                   (gc-connection-pool)
                  (network-error () (sleep *client-retry-sleep-seconds*)))
          unless *connection-scavenger-on*
            do (setq *connection-scavenger* nil)
               (process-kill process))))

(define start-connection-scavenger ()
  "Starts the client connection scavenger."
  (let ((process *connection-scavenger*))
    (setq *connection-scavenger-on* t)
    (cond (process
           (process-preset process #'scavenge-connections-main-loop)
           (process-enable process))
          (t (let ((process-name "HTTP Connection Scavenger"))
               (setq process (make-process process-name
                                           :background-p t
                                           :restart-after-reset t
                                           :warm-boot-action :delayed-restart))
               (setq *connection-scavenger* process)
               (process-preset process #'scavenge-connections-main-loop)
               (process-enable process))))
    process))

(define stop-connection-scavenger ()
  "Stops the client connection scavenger."
  (when *connection-scavenger*
    (setq *connection-scavenger-on* nil)))

#+Multi-Threaded
(eval-when (load eval)
  (start-connection-scavenger))
