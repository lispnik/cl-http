;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: IPC; Base: 10 -*-

;; ACL 5.0 port for the tcp classes

(defpackage "IPC"
  (:use :socket :cl :excl)
  (:export "INTERNET-ADDRESS"
           "IP-ADDRESS-STRING"        ; this are used with ipc:: doesn't hurt to export it
           "GET-HOST-NAME-BY-ADDRESS" ; ditto
           "PROTOCOL-ERROR"
           "*domainname*"             ; ditto
           "FRANZ-HOSTNAME"
           "GETDOMAINNAME"
           "TCP-CLIENT-STREAM"        ; used as ipc:: in places
           "TCP-STREAM-PROCESS"       ; ditto
           "TCP-CLIENT-ALIVE"         ; ditto
           "TCP-SERVER-STREAM"
           "STREAM-READ"
	   
	   #:compute-host-domain-name 
	   ))

(in-package "IPC")

(eval-when (load eval)
  ;; the terms hostname and domain name are used in seemingly many 
  ;; ways in the cl-http code.
  ;; In this module the meaning is this:
  ;; given a fully qualified dns name for a machine, e.g foo.bar.com
  ;; the
  ;;   *hostname* is the part before the first .
  ;;   *domainname* is the part after the first . including the .
  ;;   *local-hostdomainname* is the whole thing.
  (defvar *hostname* nil)
  (defvar *domainname* nil)
  (defvar *local-hostdomainname* nil)
)

;; A few functions for converting between IP address representations

; DNS symbolic name -> 32-bit address
(defmethod internet-address ((name string))
   (if (equal name *local-hostdomainname*)
      (lookup-hostname *hostname*)
      (socket:lookup-hostname name)))

; 32-bit address -> dotted string "#.#.#.#"
(defmethod ip-address-string ((address integer))
   (socket:ipaddr-to-dotted address))

; 32-bit address -> DNS symbolic name
(defmethod get-host-name-by-address ((address integer))
   (socket:ipaddr-to-hostname address))

;; this is shadow-imported into www-utils
(deftype unknown-host-name ()  'excl::socket-error)

(define-condition protocol-error (error)
  ((stream :initform nil :initarg :stream)))

(defun franz-hostname ()
  (cond (*hostname*)
        ((gethostdomainname) *hostname*)))

(defun getdomainname (&optional (where t))
  (typecase where
    (string
     (setq *domainname* where))
    (null
     (setq *domainname* nil))
    (pathname
     ;; Use different file path for ACLPC
     (gethostdomainname)))
  (or *domainname*
      (setq *domainname* (user::prompt-user-string "Enter domain name: "))))

(defun gethostdomainname (&optional (where "HTTP:acl;acl5;hostdomain"))
  (or *local-hostdomainname*
      (if (probe-file where)
	  (with-open-file (stream where :direction :input)
	    (if (setq *local-hostdomainname* (read-line stream nil nil))
		(let ((point (position #\. *local-hostdomainname*)))
		  (when point
		    (setq *hostname* (subseq *local-hostdomainname* 0 point)
			  *domainname* (subseq *local-hostdomainname* (1+ point)))
		    *local-hostdomainname*)))))
      (with-open-file (stream where :direction :output :if-exists :supersede)
	(setq *hostname* (user::prompt-user-string "Enter host name: ")
	      *local-hostdomainname* (concatenate 'string *hostname* "." (getdomainname)))
	(write-string *local-hostdomainname* stream)
	(terpri stream)
	*local-hostdomainname*)))


(defun compute-host-domain-name (given-host)
  ;; we are starting cl-http and the user has specied that host
  ;; is the host we are serving.
  ;; if it's nil the we have to figure it out for ourself
  
  (if* given-host
     then ; check to see if it's a dotted ip address
	  (let ((ipaddr (dotted-to-ipaddr given-host :errorp nil)))
	    (if* ipaddr
	       then (setq given-host (ipaddr-to-hostname ipaddr)))))
  
  (if* given-host
     then (multiple-value-bind (ok whole host domain)
	      (match-regexp "\\([^.]+\\)\\(.*\\)" given-host)
	    (declare (ignore whole))
	    (if* ok
	       then
		    (setq *hostname* host
			  *domainname* domain
			  *local-hostdomainname* given-host)
	       else (error "~s isn't a valid host name" given-host)))
     else ; must compute it.
	  (let ((name (long-site-name)))
	    (if* (null (position #\. name))
	       then ; will have to see if we can find the domain
		    (let ((ipaddr (socket:lookup-hostname name)))
		      (let ((newname 
			     (socket:ipaddr-to-hostname ipaddr
							:ignore-cache t)))
			(if* (null (position #\. newname))
			   then (error 
				 "you must specify a :host argument to start"))
			(setq name newname))))
		
	    ;; ok.. fully qualified
	    (compute-host-domain-name name)
	    (format t "computed host: ~s, domain ~s~%" 
		    *hostname*
		    *domainname*))))
	           
		    
		    
			    
  
  
;;;
;;; TCP classes
;;;

(defclass tcp-client-stream (socket::socket-stream-internet-active-bivalent)
    ((listen-sockaddr :initform nil :initarg :listen-sockaddr)
     (process :initform nil :accessor tcp-stream-process)
     (alive   :initform t   :accessor tcp-client-alive)
     ))

(defmethod tcp-client-alive ((stream t))
  nil)

(defmethod (setf tcp-client-alive) (value (stream t))
  value)

(defclass tcp-server-stream (socket::socket-stream-internet-passive)
    ((listen-socket-fd :initarg :fn-in)
     (listen-sockaddr :initarg :listen-sockaddr)
     (client-streams :initform nil)
     (process :initform :initializing :accessor tcp-stream-process)))

(defvar *sync-stream* T)

(defmethod stream-read ((stream tcp-server-stream))
   (change-class (socket:accept-connection stream) 'tcp-client-stream))

;; like listen, but for passive sockets (which aren't streams)
;; returns a client stream if a connection is pending, nil otherwise
(defmethod server-listen ((stream tcp-server-stream))
   (let ((new-connection (socket:accept-connection stream :wait nil)))
      (if new-connection
         (change-class new-connection 'tcp-client-stream)
         nil)))

(defmethod close :around ((stream tcp-server-stream) &key (abort t))
  (with-slots (process) stream
    (setq process nil))
  (clim-sys:process-yield)
  (call-next-method))





