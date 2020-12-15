;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: IPC; Base: 10 -*-

;; ACL 5.0 port for the tcp classes

(defpackage "IPC"
  (:use :socket :cl)
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
           "STREAM-READ"))

(in-package "IPC")

(eval-when (load eval)
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
(deftype unknown-host-name () #-acl5 'socket::socket-error #+acl5 'excl::socket-error)

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
      (setq *domainname* (user::prompt-user-string "Enter domain name (without host): "))))

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
	(setq *hostname* (user::prompt-user-string "Enter host name (without domain): ")
	      *local-hostdomainname* (concatenate 'string *hostname* "." (getdomainname)))
	(write-string *local-hostdomainname* stream)
	(terpri stream)
	*local-hostdomainname*)))

;;;
;;; TCP classes
;;;

(defclass tcp-client-stream (socket::socket-stream-internet-active-text)
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
  (declare (ignore abort))
  (with-slots (process) stream
    (setq process nil))
  (clim-sys:process-yield)
  (call-next-method))
