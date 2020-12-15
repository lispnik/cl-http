;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: IPC; Base: 10 -*-

(defpackage "IPC"
  (:use "SOCKET" "COMMON-LISP")
  (:export "FRANZ-HOSTNAME" "GETDOMAINNAME" "INTERNET-ADDRESS" "INTERNET-ADDRESS-DOMAIN" "PROTOCOL-ERROR" "TCP-CLIENT-ALIVE" "TCP-CLIENT-STREAM" "TCP-SERVER-STREAM" "STREAM-READ"))

(in-package "IPC")

(deftype unknown-host-name () 'error)

(defvar *local-hostdomainname* nil)

(defun internet-address (name)
  (if (equal name *local-hostdomainname*)
      (lookup-hostname *hostname*)
    (lookup-hostname name)))

(defvar *static-sstring* (ct:ccallocate (char 256)))

(defconstant *winsock-dll-library* 
    #-ACLNT "C:\\nfs\\winsock.dll"
    #+ACLNT "C:\\windows\\system32\\wsock32.dll")

;;; Can someone figure out why this does not work in 3.0?
;;; This worked fine in ACLPC 2.0 and now it always return -1.
;;; Please send your fix to acl-bug-cl-http@ai.mit.edu - thanks!
;;;
(ct:defun-dll gethostname ((name (:char *))
			   (namelen :short))
  :entry-name "gethostname"
  :library-name *winsock-dll-library*
  :16-bit #+ACLNT nil #-ACLNT t
  :call-mode :c
  :return-type :short)

;;; This allocates a string for hostname
(defun %franz-hostname ()
  (let ((string *static-sstring*))
    (when (zerop (gethostname string 256))
      (let ((length (position '#.(character 0) string)))
	(if length
	    (subseq string 0 length))))))

(defvar *hostname* nil)

(defun franz-hostname ()
  (cond (*hostname*)
	((setq *hostname* (%franz-hostname)))
	((gethostdomainname)
	 *hostname*)))

(defvar *domainname* nil)

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

(defun gethostdomainname (&optional (where "HTTP:acl;aclpc;hostdomain"))
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
      
(setf (fdefinition 'ip-address-string) (fdefinition 'ipaddr-to-dotted))

(setf (fdefinition 'get-host-name-by-address) (fdefinition 'ipaddr-to-hostname))

(defvar *sync-stream* T)

(defun stream-read (stream)
  (let ((new (accept-connection stream)))
    ;; Fix socket bug where some extra characters
    ;; may trail after accept-connection returns
    (when new
      (if (listen stream)
	  (read-char stream nil nil))
      (case *sync-stream*
	(loop repeat 256
	    while (listen stream)
	    do (read-char stream nil nil)))
      (if (listen stream)
	  (warn "STREAM-READ stream in LISTEN mode may block.~%Select Socket->Abort Current Network Operation if it does."))
      new)))

(defun stream-closed-p (stream &key poke)
  (if (socket::sd-eofp stream)
      t
    (multiple-value-bind (open error)
	(ignore-errors (listen stream))
      (if error
	  (setf (socket::sd-eofp stream) t)
	(if open
	    nil
	  (if poke
	      (progn (write-char #\Space stream)
		     (socket::sd-eofp stream))))))))

(defun stream-listen (stream)
  (if (stream-closed-p stream)
      nil
    (listen stream)))
