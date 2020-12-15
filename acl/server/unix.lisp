;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*-

;;; (C) Copyright 1994-1995, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;; (C) Copyright 1995, Olivier (OBC).
;;;	All Rights Reserved -- Allegro UNIX & PC extensions.

(defpackage "WWW-UTILS"
  (:use)
  ;; a series of network conditions that we would like to be able to handle
  ;; within portable code.
  ;; Need a description of what these errors mean - OBC.
  (:shadowing-import-from "IPC" "UNKNOWN-HOST-NAME")
  (:import-from "RESOURCES"
		"ALLOCATE-RESOURCE" "CLEAR-RESOURCE" "DEALLOCATE-RESOURCE" "DEFRESOURCE"
		"MAP-RESOURCE" "USING-RESOURCE")
  (:export "ALLOCATE-RESOURCE" "CLEAR-RESOURCE" "DEALLOCATE-RESOURCE" "DEFRESOURCE"
	   "MAP-RESOURCE" "USING-RESOURCE")
  ;; Extensions
  (:import-from "CLIM-SYS" "PROCESS-ACTIVE-P" "PROCESS-PRESET"
		"PROCESS-WHOSTATE" "CURRENT-PROCESS")
  (:export "MAKE-PROCESS" "PROCESS-RUN-TIME" "PROCESS-IDLE-TIME" "CURRENT-PROCESS")
  #+Allegro
  (:shadowing-import-from "MP" "PROCESS-RUN-FUNCTION")
  (:export "PROCESS-ACTIVE-P" "PROCESS-RUN-FUNCTION" "PROCESS-RESET" "PROCESS-PRESET" "PROCESS-KILL"
	   "PROCESS-WHOSTATE" "PROCESS-ENABLE" "PROCESS-DISABLE" "PROCESS-WAIT" "PROCESS-WAIT-WITH-TIMEOUT")
  (:export "BAD-CONNECTION-STATE"
	   "CONNECTION-CLOSED"
	   "CONNECTION-ERROR"
	   "CONNECTION-LOST"
	   "CONNECTION-REFUSED"
	   "DOMAIN-RESOLVER-ERROR"
	   "HOST-NOT-RESPONDING"
	   "HOST-STOPPED-RESPONDING"
	   "LOCAL-NETWORK-ERROR"
	   "NETWORK-ERROR"
	   "NETWORK-PARSE-ERROR"
	   "NETWORK-RESOURCES-EXHAUSTED"
	   "PROTOCOL-TIMEOUT"
	   "REMOTE-NETWORK-ERROR"
	   "UNKNOWN-ADDRESS"
	   "UNKNOWN-HOST-NAME")
  (:export "FILE-NOT-FOUND" "DEFAULT-PATHNAME" "ABORT-HTTP-STREAM" "LOG-WINDOW"
           "NOTIFY-LOG-WINDOW" "EXPOSE-LOG-WINDOW" "COMMON-LOGFILE-NOTIFY" "BYTES-TRANSMITTED" "BYTES-RECEIVED")
  ;; Faster version
  (:export "%CHAR-EQUAL")
  (:export "WITH-OPTIMAL-STREAM-BUFFER" "GET-LINE-ARGUMENT")
  ;; New need
  #+Allegro
  (:shadowing-import-from "CLOS" "GENERIC-FUNCTION-METHODS" "METHOD-SPECIALIZERS" "CLASS-DIRECT-SUPERCLASSES" "CLASS-PRECEDENCE-LIST")
  (:export  "GENERIC-FUNCTION-METHODS" "METHOD-SPECIALIZERS" "CLASS-DIRECT-SUPERCLASSES" "CLASS-PRECEDENCE-LIST")
  (:export "REPORT")
  #+ACLPC
  (:export "SPECIAL-OPERATOR-P")
  #+(and Allegro (not ACL5))
  (:shadowing-import-from "EXCL" "CHUNK-TRANSFER-ENCODING-MODE" "NOTE-FIRST-CHUNK" "NOTE-LAST-CHUNK")
  (:export "CHUNK-TRANSFER-ENCODING-MODE" "NOTE-FIRST-CHUNK" "NOTE-LAST-CHUNK")
  (:export "NETWORK-ERROR-MIXIN")
  (:export "TCP-SERVICE-PORT-NUMBER" "STREAM-TYI" "STREAM-TYO" "OPEN-MAILER-STREAM" "FIND-DIRECT-CONNECTION")
)


;;;------------------------------------------------------------------- 
;;;
;;; MAC AND LISPM FILES COMPATABILITY CODE ADAPTED FOR ALLEGRO CL
;;;

(in-package :www-utils)

#+ACLPC
(setf (fdefinition 'special-operator-p) (fdefinition 'acl::special-operator-p))

#+ACLPC
(defpackage "COMMON-LISP" (:use) (:export "ARGLIST"))

#+ACLPC
(proclaim '(declaration common-lisp::arglist))

#+ACLPC
(proclaim '(declaration values))

(define-condition future-network-error (error)
  ((host :initform "" :initarg :host)
   (port :initform 0 :initarg :port))
  (:default-initargs :format-control "for host ~S and port ~S."))

#+Franz-Inc
(defmethod simple-condition-format-control ((error future-network-error))
  (slot-value error #+Allegro 'excl::format-control #+ACLPC 'acl::format-control))

#+Franz-Inc
(defmethod simple-condition-format-arguments ((error future-network-error))
  (slot-value error #+Allegro 'excl::format-arguments #+ACLPC 'acl::format-arguments))

(defmethod initialize-instance :after ((error future-network-error) &key format-arguments host port &allow-other-keys)
  (if (and (or host port) (null format-arguments))
      (reinitialize-instance error :format-arguments (list host port))))

(defmethod print-object ((error future-network-error) stream)
  (print-unreadable-object (error stream :type t :identity t)
    (apply #'format stream (simple-condition-format-control error)
	   (simple-condition-format-arguments error))))
  
(defmacro add-default-conditions (&rest types)
  `(progn
     ,@(mapcar
	#'(lambda (sname)
	    `(define-condition ,(intern sname) (future-network-error) ()))
	types)))

(add-default-conditions
 #-Allegro
 "BAD-CONNECTION-STATE"
 "CONNECTION-CLOSED"
 "CONNECTION-ERROR"
 #-FRANZ-INC
 "CONNECTION-LOST"
 "CONNECTION-REFUSED"
 "DOMAIN-RESOLVER-ERROR"
 "HOST-NOT-RESPONDING"
 "HOST-STOPPED-RESPONDING"
 #-Allegro
 "LOCAL-NETWORK-ERROR"
 #-Allegro
 "NETWORK-ERROR"
 "NETWORK-PARSE-ERROR"
 "NETWORK-RESOURCES-EXHAUSTED"
 "PROTOCOL-TIMEOUT"
 #-Allegro
 "REMOTE-NETWORK-ERROR"
 "UNKNOWN-ADDRESS"
 #-FRANZ-INC
 "UNKNOWN-HOST-NAME")

(defmacro define-pseudo-condition (name supers slots &rest options)
  (declare (ignore slots options))
  `(progn (deftype ,name () ',(first supers))
	  (setf (find-class ',name) (find-class ',(first supers)))))

;;; When remote end aborts stream while we are transfering
;;; closing the stream can cause a Signal 13 on our end.
#+Allegro
(define-pseudo-condition bad-connection-state (excl::synchronous-operating-system-signal future-network-error) ())

#+Allegro
(define-pseudo-condition remote-network-error (excl::synchronous-operating-system-signal future-network-error) ())

#+Allegro
(define-pseudo-condition network-error (excl::synchronous-operating-system-signal future-network-error) ())

;;; This should be more specific: really a FILE-ERROR while trying
;;; to write to a TCP-CLIENT-STREAM!
#+FRANZ-INC
(define-pseudo-condition connection-lost (file-error future-network-error) ())

#+Allegro
(define-pseudo-condition local-network-error (ipc:protocol-error future-network-error) ())

(define-condition network-error-mixin
                  (network-error)
  ()
  (:documentation "Mixin to allow ports to inherit instance variables and methods to network conditions
defined at the portable code level."))

; Advise the lisp environment that we have MAC-CL-HTTP loaded.
#+CCL
(pushnew :mac-cl-http *features*)

#+CCL
(declaim (inline report-condition))

(define report-condition (condition stream)
  "Prints the report string for CONDITION onto STREAM."
  #+CCL
  (ccl::report-condition condition stream)
  #-CCL
  (handler-case (format stream "~A" condition)
    ;; Cannot guaranty all errors are printable.
    (error ()
      (describe condition stream))))

;;; For Web Walker?
;;;
(setf (fdefinition 'report) (fdefinition 'report-condition))

#+CCL
(declaim (inline report-string))

(define report-string (condition)
  "Returns the report string for CONDITION."
  (with-output-to-string (stream)
    (report-condition condition stream)))

;; Define equivalence mapping to the MCL case.
(deftype file-not-found () 
  "Specialization of Common Lisp File-error in which the file was not found on open."
  '(and condition file-error))

#+ignore
(export 'file-not-found :www-utils)

; with-tcp-port-for-protocol not used !

(defmacro with-array-registers (bindings &body body)
  `(let ,bindings ,@body))

#+CCL
(define-macro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(#+CCL ccl:without-interrupts #+CLIM-SYS clim-sys:without-scheduling
     (incf ,reference ,delta)))

#+CLIM-SYS
(define-macro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  #+(and Allegro CLIM-2)
  `(minp:atomic-incf ,reference ,delta)
  #-(and Allegro CLIM-2)
  `(clim-sys:atomic-incf ,reference ,delta))

#+CCL
(define-macro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(#+CCL ccl:without-interrupts #+CLIM-SYS clim-sys:without-scheduling
     (decf ,reference ,delta)))

#+CLIM-SYS
(define-macro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  #+(and Allegro CLIM-2)
  `(minp:atomic-decf ,reference ,delta)
  #-(and Allegro CLIM-2)
  `(clim-sys:atomic-decf ,reference ,delta))

(define-macro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(#+CCL ccl:without-interrupts #+CLIM-SYS clim-sys:without-scheduling
          (push ,item ,reference)))

(define-macro atomic-pop (reference)
  "Atomically pops an item off REFERENCE."
  `(#+CCL ccl:without-interrupts #+CLIM-SYS clim-sys:without-scheduling
	  (pop ,reference)))

(declaim (inline arglist))

(defun arglist (function)
  "Returns the arglist for FUNCTION."
  (declare (values (arglist values type arglist-types value-types)))
  (#+CCL ccl:arglist #+Allegro excl:arglist #+ACLPC acl:lambda-list function))

;;;------------------------------------------------------------------- 
;;;
;;; OBTAINING THE DOMAIN NAME FOR AN IP ADDRESS
;;;

#||
(defun ip-address-string-p (string &aux (count 0))
  "Returns non-null if string is a well-form IP address string."
  (flet ((good-char-p (char)
           (or (digit-char-p char)
               (and (char= char #\.) (incf count)))))
    (declare (inline good-char-p))
    (and (stringp string)
         (loop for idx upfrom 0 below (length string)
               unless (good-char-p (aref string idx))
                 do (return-from ip-address-string-p nil)
               finally (return-from ip-address-string-p (eql count 3))))))

(defun %parse-host-address (address)
  (cond (http:*resolve-ip-addresses* (ccl::tcp-host-address address))
        ((integerp address) address)
        ((ip-address-string-p address)
         )
        ( (error  'ccl:tcp-unknown-domain-name 
                  "Can't resolve unknown domain names with domain resolution turned off.~
                            Please use IP addresses only or turn domain resolution on."))))||#

(declaim (inline %parse-host-address))

(defvar http::*shadow-host-domain-name* nil
  "Set this variable to a replacement for local-host-domain-name.
This name must be a valid name in the NIS directory and will be used
to shadow the name normally returned by local-context.")

(defun %parse-host-address (address)
  "Returns an IP-NUMBER which is integer denoting the address of host."
  (declare (values ip-number))
  #+CCL (ccl::tcp-host-address  address)
  #+FRANZ-INC
  (etypecase address
    (integer address)
    (string (if (or (and http::*shadow-host-domain-name*
			 (equal http::*shadow-host-domain-name*
				address))
		    (equalp address "localhost"))	; RFC 1738 says defines this string   5/3/96 -- JCMa.
		(local-host)
	      (if http::*proxy-service*
		  (http::ip-host-proxy-address address)
		(ipc:internet-address address))))))

(declaim (inline ip-address-for-parsed-ip-address))

(define ip-address-for-parsed-ip-address (ip-number)
  "Returns an IP address as a string from, IP-NUMBER, the parsed address."
  (#+CCL ccl::tcp-addr-to-str #-CCL ipc::ip-address-string ip-number))

#+CCL
(define domain-name-for-parsed-ip-address (ip-number &optional (no-error-p t))
  "Given the parsed IP address (an integer), IP-NUMBER, this returns the domain name or NIL.
When no-error-p is T, this returns the IP Address string when a domain error occurs."
  (check-type ip-number integer)
  (flet ((kludge-around-mcl201-bug (ip-number)
           (let* ((name (ccl::tcp-host-cname ip-number))
                  (last-elt (1- (the fixnum (length name)))))
             (if (eql (aref name last-elt) #\.)
                 (subseq name 0 last-elt)
                 name))))
    (declare (inline kludge-around-mcl201-bug))
    (cond 
      ((or (null http:*resolve-ip-addresses*) (zerop ip-number)) 
       (ip-address-for-parsed-ip-address ip-number))
      (no-error-p
       (handler-case
         (kludge-around-mcl201-bug ip-number)
         ;; formerly tcp-domain-server-not-found
         (ccl:domain-error () (ip-address-for-parsed-ip-address ip-number))))
      (t (kludge-around-mcl201-bug ip-number)))))

;;; Attempting to get/set DNS domain names portably on UNIX
;;;
#+FRANZ-INC
(defun default-domain-name (&key (where #+Allegro #.(pathname "HTTP:acl;defaultdomain") #+ACLPC (load-time-value (translate-logical-pathname "HTTP:acl;defaultdomain"))))
  (or (ipc:getdomainname where)
      (ipc:getdomainname)
      (flet ((dnp (dn)
	       (and (stringp dn)
		    ;; Need at least one of #\. or this may not be DNS!
		    ;; Number may vary with contries?
		    (find #\. dn))))
	(warn "Unknown DNS domain name.")
	(format *query-io* "~&Enter you local DNS domain name 'local.institution.type': ")
	(peek-char t *query-io*)
	(let ((dn (read-line *query-io*)))
	  (cond ((dnp dn)
		 (ipc:getdomainname dn)
		 (with-open-file (stream where :direction :output
				  :if-does-not-exist :create
				  :if-exists :supersede)
		   (write-string dn stream)
		   (terpri stream))
		 dn)
		(t
		 "the.unknown.domain"))))))

#+FRANZ-INC
(defvar *domain-name-lookup* (make-hash-table :test
					      #+(version>= 4 3) #'eql					                      ;; ACL bug reported 28/6/95
					      #-(version>= 4 3) #'equal))

#+FRANZ-INC
(defun forget-domain-names ()
  (ipc:getdomainname nil)
  (clrhash *domain-name-lookup*))

;;; Need to figure out how to do this portably for non CCL (e.g. UNIX)
;;; This returns one of "host.site.entity.type" or an IP address - OBC
#+FRANZ-INC
(define domain-name-for-parsed-ip-address (ip-number &optional (no-error-p t))
  (or (if (and http::*shadow-host-domain-name*
	       (eql ip-number (local-host)))
	  http::*shadow-host-domain-name*
	(gethash ip-number *domain-name-lookup*))
      (let ((domain (default-domain-name)))
        (setf (gethash ip-number *domain-name-lookup*)
          (let ((host-name (if no-error-p
                               (handler-case (ipc::get-host-name-by-address ip-number)
				 (unknown-host-name ()))
                             (ipc::get-host-name-by-address ip-number))))
            (if host-name
		(if (stringp host-name)
		    (if (not (find #\. host-name)) ;Solaris 1.x or 2.x with NIS
		      	(concatenate 'string host-name "." domain)
		      host-name)	;Solaris 2.x with DNS
		  (if no-error-p
		      host-name
		    (error "Unexpected host name ~s." host-name)))
              ;; No error fall-back
              (ipc::ip-address-string ip-number)))))))

(declaim (inline domain-name-for-ip-address))

(define domain-name-for-ip-address (address &optional (no-error-p t))
  "Given the IP address, ADDRESS, this returns the domain name or NIL."
  (domain-name-for-parsed-ip-address (%parse-host-address address) no-error-p))

(declaim (inline ip-address-for-host-domain-name))

(define ip-address-for-host-domain-name (domain-name)
  "Returns the IP address string for domain-name."
  (ip-address-for-parsed-ip-address (%parse-host-address domain-name))) 

(declaim (notinline %local-host-parsed-ip-number))

#+CCL
(defun %local-host-parsed-ip-number ()
  (handler-case
    (ccl::%tcp-getaddr)
    (ccl:domain-error () 0)
    ;; when a powerbook is disconnected, an untyped error is signalled
    ;; within CCL::TCP-DRIVER-REFNUM when it tries to open TCP.
    ;; MACTCP should signal a typed error here. -- JCMa 1/20/1995.
    (error () 0)))

;;; HTTP Add-ons
;;;
(defpackage "HTTP"
  (:use)
  (:export "*SHADOW-HOST-NAME*"))

(defvar http:*shadow-host-name* nil
  "Set this variable to a replacement hostname. This name must be
a valid name in the NIS directory and will be used to shadow the
name normally returned by gethostname.")

(define %local-host-name ()
  (or http:*shadow-host-name* (ipc:franz-hostname)))

(defun host-machine-domain-port (hoststring)
  (let ((pos (position #\. hoststring))
	(pos2 (position #\: hoststring :from-end t))
	port)
    (if pos2
	(if (< (1+ pos2) (length hoststring))
	    (setq port (read-from-string hoststring nil nil :start (1+ pos2))))
      (setq pos2 (length hoststring)))
    (if pos
	(values (if (> pos 0)
		    (subseq hoststring 0 pos))
		(if (< (incf pos) pos2)
		    (subseq hoststring pos pos2))
		port)
      (if (> pos2 0)
	  (values (subseq hoststring 0 pos2) nil port)
	(values nil nil port)))))

;;; Experimental - for intranet shadow
;;; e.g. (install-symbolic-host-shadow "fakename.net")
;;;      (install-symbolic-host-shadow "realname.real.domain.name" :port 8000 :reset-port-p t)
;;;
(defun install-symbolic-host-shadow (hostdomain &key (port 80) (update-context-p t) reset-port-p)
  (declare (special http::*fast-remap-p*))
  (multiple-value-bind (name domain newport)
      (host-machine-domain-port hostdomain)
    (let ((shadow domain)
	  (orighost (%local-host-name))
	  (origdomain (ipc:getdomainname)))
      (if (null name)
	  (setq name orighost))
      (if (null domain)
	  (setq domain origdomain)
	(ipc:getdomainname domain))
      (if newport
	  (setq port newport))
      (let ((lastcontext (http::local-context)))
	(if update-context-p
	    (record-original-context lastcontext :host orighost :domain origdomain))
	(if (equal name (ipc:franz-hostname))
	    (setq http:*shadow-host-name* nil)
	  (setq http:*shadow-host-name* name))
	(if shadow
	    (setq http::*shadow-host-domain-name*
	      (make-host-domain-name :host name :domain domain)))
	(local-host-domain-name t)
	(when update-context-p
	  (let ((newcontext (http::%make-local-context port)))
	    (setf (http::local-context) newcontext)
	    (install-local-port-context-shadow newcontext lastcontext)
	    (unless (and lastcontext
			 (equal lastcontext newcontext))
	      (url::update-urls newcontext lastcontext :fast-remap-p http::*fast-remap-p*))))
	(if reset-port-p
	    (reset-standard-http-port port))))))

(defun make-host-domain-name (&key host domain)
  (if (and host domain)
      (concatenate 'string host "." domain)
    (if host
	host
      (error "Unknown host"))))

(defvar *original-context-record* nil)

(defun record-original-context (&optional context &key host domain reset)
  (if context
      (unless (and *original-context-record* (null reset))
	(multiple-value-bind (hostdomainname port)
	    (url::context-host-port context)
	  (multiple-value-bind (hostname domainname)
	      (host-machine-domain-port hostdomainname)
	    (setq host (or host hostname)
		  domain (or domain domainname)
		  hostdomainname (make-host-domain-name :host host :domain domain))
	    (setq context (if port
			      (concatenate 'string "http://" hostdomainname ":" (princ-to-string port))
			    (concatenate 'string "http://" hostdomainname)))
	    (setq *original-context-record*
	      (list context hostdomainname (or domain domainname) (or host hostname)))
	    (forget-domain-names)
	    (values *original-context-record* t))))
    *original-context-record*))

;;; Used for Proxy (tunnel) connection when a shadow host is used
;;; to restore the true context before reaching direct proxy gateways. Test:
;;; (with-original-context (values (http:local-context) (%local-host-domain-name) (ipc:getdomainname) (%local-host-name)))
;;;
(defmacro with-original-context (&rest body)
  `(if (consp (record-original-context))
       (destructuring-bind (#1=#:context #2=#:hostdomain #3=#:domain #4=#:host)
	   (record-original-context)
	 (let ((http:*shadow-host-name* #4#)
	       (http::*shadow-host-domain-name* #2#)
	       (ipc::*domainname* #3#))
	   (http::with-local-context (#1#)
	     ,@body)))
     (error "No original context recorded")))
	       
;;; Update the local port context table used by PROVIDE-SERVICE
;;; to think it's running off the new host and port...
;;; e.g. (install-local-port-context-shadow
;;;        "http://testing.mit.edu:8000" "http://fake.net")
(defun install-local-port-context-shadow (newcontext &optional (lastcontext (http::local-context)))
  (unless (and lastcontext
	       (equal lastcontext newcontext))
    (multiple-value-bind (host port) (url::context-host-port lastcontext)
      (declare (ignore host))
      (let ((match (assoc port http::*local-port-context-alist*)))
	(if match
	    (setf (rest match) newcontext)
	  (push `(,port . ,newcontext) http::*local-port-context-alist*))))))

#+FRANZ-INC
(defun %local-host-parsed-ip-number ()
  (ipc:internet-address (%local-host-name)))

(define local-host ()
  "The host object for the local host on which we are running."
  (or http::*local-host-address*
      (setq http::*local-host-address* (%local-host-parsed-ip-number))))

(define local-host-ip-address (&optional recache-p)
  "Returns the IP address of the local host."
  (cond ((and (not recache-p) http::*local-host-ip-address*))
        (t (setq http::*local-host-ip-address* (ip-address-for-parsed-ip-address 
                                                 (%local-host-parsed-ip-number))))))

(define local-host-parsed-ip-address (&optional recache-p)
  "Returns the parsed IP address of the local host."
  (cond ((and (not recache-p) http:*local-host-address*))
        (t (setq http:*local-host-address* (%local-host-parsed-ip-number)))))

(define %local-host-domain-name ()
  (or http::*shadow-host-domain-name*
      (let ((ip-number (%local-host-parsed-ip-number)))
	(if (zerop ip-number)
	    ;; zerop means no network connection and no DNS
	    (ip-address-for-parsed-ip-address ip-number)
	  ;; normal case wth network and DNS
	  (domain-name-for-parsed-ip-address ip-number)))))

(defun local-host-domain-name (&optional recache-p)
  "Returns the local host domain name."
  (cond ((and (not recache-p) http::*local-host-domain-name*))
        (t (setq http::*local-host-domain-name* (%local-host-domain-name)))))

(defvar *export-pathnames* nil)

(defun add-export-pathname (name)
  #+ignore ;Surprisingly this even worked:
  (pushnew (pathname name) *export-pathnames* :key #'namestring :test #'equal)
  ;; Need to nconc for user extensions
  (let ((path (pathname name)))
    (unless (find (namestring path) *export-pathnames* :key #'namestring :test #'equal)
      (setq *export-pathnames*    ;Required to catch user additions
	(nconc *export-pathnames* (list path))))))

(defun export-pathnames ()
  *export-pathnames*)

(defun clear-export-pathnames ()
  (setq *export-pathnames* nil))

;;; Exports that will need reloading from SERVER;ACL;SYSDCL
;;; This now loaded from the above one, can't have both defined as exports
;;;(add-export-pathname "http:examples;exports")
(add-export-pathname "http:acl;examples;exports")

(defvar *compiling-pathnames* nil)

(defun compile-exports (&key (skip 0) (exports (nthcdr skip (export-pathnames))))
  (unless *compiling-pathnames*
    (if exports
	(format t "~&Compiling exports.~%"))
    (loop for paths on exports by #'rest    ;Required to catch user additions
	as (path) = paths
	do (let ((*compiling-pathnames* (list *compile-file-pathname*)))
	     (compile-if-needed path)))))

(defvar *hide-export-errors* t)

(defmacro hide-export-errors ((warning) &rest body)
  `(if *hide-export-errors*
       (multiple-value-bind (#1=#:result #2=#:error)
	   (ignore-errors ,@body)
	 (if #2#
	     (warn "~a: ~a" ,warning #2#)
	   #1#))
     (progn ,@body)))

(defun compile-if-needed (path)
  (unless (pathname-type path)
    (setq path (make-pathname :type "lisp"
			      :defaults path)))
  (setq path (translate-logical-pathname path))

  (let ((compiling *compiling-pathnames*)
	binpath)
    (setq binpath (make-pathname :type #+ACLPC "fsl" #-ACLPC "fasl"
				 :defaults path))
    (cond ((and (consp compiling) (first compiling)
		(find path compiling :test #'equal))
	   (format t "~&; File already being compiled ~s~%" path)
	   path)
	  ((null (probe-file path))
	   (format t "~&; File not found ~s~%" path))
	  ((or (null (probe-file binpath))
	       (< (file-write-date binpath) (file-write-date path)))
	   (when (consp compiling)
	     (if (first compiling)
		 (nconc compiling (list path))
	       (setf (first compiling) path))
	     (if *compile-file-pathname*
		 (nconc compiling (list *compile-file-pathname*))))
	   (hide-export-errors
	    ("Compiling")
	    (compile-file path)))
	  (t
	   (format t "~&; File already compiled ~s~%" path)
	   path))))

(defvar *loading-pathnames* nil)

(defun load-exports (&key (skip 0) (exports (nthcdr skip (export-pathnames))))
  (unless *loading-pathnames*
    (if exports
	(format t "~&Loading exports.~%"))
    (loop for paths on exports by #'rest    ;Required to catch user additions
	as (path) = paths
	do (let ((*loading-pathnames* (list *load-pathname*)))
	     (or (hide-export-errors ("Loading") (load-if-needed path))
		 (warn "Load-exports of ~a is incomplete." path))))))

(defun load-if-needed (path)
  (unless (pathname-type path)
    (setq path (make-pathname :type #+ACLPC "fsl" #-ACLPC "fasl"
			      :defaults path)))
  (setq path (translate-logical-pathname path))
  (let ((loading *loading-pathnames*))
    (cond ((and (consp loading) (first loading)
		(find path loading :test #'equal))
	   (format t "~&; File already being loaded ~s~%" path)
	   path)
	  ((not (probe-file path))
	   (format t "~&; File not found ~s~%" path))
	  ((>= (file-write-date path) (http-image-date))
	   (when (consp loading)
	     (if (first loading)
		 (nconc loading (list path))
	       (setf (first loading) path))
	     (if *load-pathname*
		 (nconc loading (list *load-pathname*))))
	   (load path))
	  (t
	   (format t "~&; File already loaded ~s~%" path)
	   path))))

(defvar *last-port-change* nil)

(defun reset-http-server-location (&key (reload-exports (export-pathnames)) standard-port-change (force-reload (> (http-image-date) 0)) (reset-location t))
  (declare (special http::*fast-remap-p*))
  (if reset-location
      (reset-http-location))
  (let ((lastcontext (http::local-context))
	newcontext
	(same-host-name-p (equal (local-host-domain-name)
				 (local-host-domain-name t)))
	(same-address-p (equal (local-host-ip-address)
			       (local-host-ip-address t))))
    (unless (and (null standard-port-change)
		 same-host-name-p same-address-p)
      (when (and (integerp standard-port-change)
		 (eql *last-port-change* standard-port-change)
		 same-host-name-p same-address-p)
	(setq reload-exports nil))
      (reset-standard-http-port standard-port-change)
      (when reload-exports
	(setq newcontext (http::local-context))
	(unless (and lastcontext
		     (equal lastcontext newcontext)
		     same-address-p)
	  (url::update-urls newcontext lastcontext :fast-remap-p http::*fast-remap-p*))
	(if force-reload
	    (load-exports :exports reload-exports))
	t))))

(defun reset-standard-http-port (standard-port-change)
  (declare (special http::*http-ports*))
  (forget-domain-names)
  (if http::*proxy-service*
      (http::clear-proxy-mappings))
  (when (integerp standard-port-change)
    (setq http:*standard-http-port* standard-port-change)
    (setq *last-port-change* standard-port-change)
    ;; This is needed after all - see acl;server;tcp-interface
    (setq http::*http-ports* (list standard-port-change)))
  (http::reset-server-local-host-variables))

(defvar *http-image-place* #+Franz-Inc (make-pathname :directory '(:relative "acl") :name "http_image"))

(defun image-place-up-count (image-place)
  (max (1- (length (pathname-directory image-place)))
       0))

(defun http-image-location (&optional (image-place *http-image-place*))
  (let ((image-path #+Allegro (sys:command-line-argument 0) #+ACLPC (first (user::command-line-arguments))))
    (if image-path
	(let ((end (search (namestring image-place) image-path)))
	  (cond ((or (null end) (eql end 0))
		 user::*http-directory*)
		((probe-file image-path)
		 (values (pathname image-path) t))
		(t
		 (let ((nextpath (merge-pathnames (pathname (subseq image-path end)))))
		   (cond ((probe-file nextpath)
			  (values nextpath t))
			 (t
			  (warn "HTTP image not found: ~a or ~a" image-path nextpath)
			  user::*http-directory*))))))
      user::*http-directory*)))

(defun http-image-date (&optional (image-place *http-image-place*))
  (multiple-value-bind (place really) (http-image-location image-place)
    (if really
	(file-write-date place)
      0)))

(defun http-image-directory (&optional (image-place *http-image-place*))
  (multiple-value-bind (place really) (http-image-location image-place)
    (if really
	(pathname-up-directory (http-image-location image-place)
			       (image-place-up-count image-place))
      place)))

(defun reset-http-location (&optional (newpath (http-image-directory)))
  (logical-host-url-location "HTTP" newpath))

(defmethod pathname-up-directory ((pathname string) &optional (nthup 0))
  (pathname-up-directory (pathname pathname) nthup))

(defmethod pathname-up-directory ((pathname pathname) &optional (nthup 0))
  (cond ((or (pathname-name pathname)
	     (> nthup 0))
         (make-pathname :host (pathname-host pathname)
                        :device (pathname-device pathname)
                        :directory (butlast (pathname-directory pathname) nthup)))
        (t 
	 pathname)))

;;; Note that on UNIX the logical-host may be case sensitive
;;;
(defun logical-host-url-location (logical-host &optional (newpath (http-image-directory)) &key (redefine-p t) (urls-p t))
  ;; For ACL ports, the image is one level down from main directory
  (setq newpath (pathname newpath))
  (let ((lastpath (ignore-errors
		   (and (logical-pathname-translations logical-host)
			(translate-logical-pathname
			 (concatenate 'string logical-host ":"))))))
    (unless (if redefine-p
		(equal lastpath newpath)
	      lastpath)
      (load-logical-host-translations
       logical-host
       :location newpath
       :defaults (merge-pathnames *http-image-place* newpath))
      (if (and urls-p
	       lastpath
	       (not (equal lastpath newpath)))
	  (url::update-urls-pathname newpath lastpath)
	logical-host))))

(defun load-logical-host-translations (logical-host &key location (defaults *default-pathname-defaults*))
  (let ((*default-pathname-defaults* defaults))
    (multiple-value-bind (translations error)
	(ignore-errors (user::load-logical-pathname-translations-patch logical-host :all t))
      (cond (error
	     (warn "Logical host ~s translations not loaded.~% Using default definition.~%~a" logical-host error)
	     (setf (logical-pathname-translations logical-host)
	       `(("**;*.*.*" ,location)
		 ("*.*.*" ,location))))
	    (t
	     translations)))))

;;; Assume line arguments passed are of the form key=value
;;; e.g. (get-line-argument "port=" 'fixnum)
;;;
(defun get-line-argument (keystr type &optional (line-arguments (rest #+Allegro (sys:command-line-arguments) #+ACLPC (user::command-line-arguments))))
  (let ((match (find-if #'(lambda (str)
			    (and (>= (length str) (length keystr))
				 (string= keystr str :end2 (length keystr))))
			line-arguments))
	(pos (length keystr)))
    (when match
      (if (< pos (length match))
	  (if (char= (elt match pos) #\=)
	      (incf pos)))
      (let ((value (subseq match pos)))
	(values (if #-ACLPC (subtypep type '(or number symbol list))
		  #+ACLPC (or (subtypep type '(or fixnum symbol))
			      (subtypep type '(or number symbol))
			      (subtypep type '(or symbol list))
			      (subtypep type '(or number symbol list)))
		    (read-from-string value)
		  value)
		match)))))

;; pass this function back to tcp-stream.lisp where it is used in ccl::tcp-host-cname
#+CCL
(setf (symbol-function 'ccl::local-host-domain-name) #'local-host-domain-name)

;;;------------------------------------------------------------------- 
;;;
;;; HOST RELATED
;;;

(define parse-host (address &optional no-error-p)
  "Top-level method for parsing a host ADDRESS."
  (declare (values ip-number))
  (cond (no-error-p
         (handler-case
           (%parse-host-address address)
           (network-error () nil)))
        (t (%parse-host-address address))))

(declaim (inline host-mail-name))

(define host-mail-name (host)
  "The internet mail name for HOST."
  (domain-name-for-ip-address host t))

(define host-eq (host1 host2)
  "Returns non-null if HOST1 is equal to HOST2."
  (cond ((and (integerp host1) (integerp host2))
         (= host1 host2))
        (t (= (%parse-host-address host1)
              (%parse-host-address host2)))))

(define host-http-name (host)
  "Returns the internet host name for HOST."
  (host-mail-name host))

(declaim (inline %host-log-name))

(define %host-log-name (address host &optional resolve-ip-address-p)
  "Returns a string for use in logging server access."
  (declare (ignore host))
  (if resolve-ip-address-p
      (domain-name-for-parsed-ip-address address t)
      (ip-address-for-parsed-ip-address address))) 

;;; These need definitions in non CCL case!

#+CCL
(define local-port (http-stream)
  "Returns the local host port for the remote connection via http-stream."
  (let* ((conn (slot-value http-stream 'ccl::conn))
         (pb (when conn (ccl::conn-pb conn))))
    (when pb
      (ccl:rref pb tcpiopb.status.localport))))

#+CCL
(define foreign-port (http-stream)
  "Returns the foreign host port for the remote connection via http-stream."
  (let* ((conn (slot-value http-stream 'ccl::conn))
         (pb (when conn (ccl::conn-pb conn))))
    (when pb
      (ccl:rref pb tcpiopb.status.remoteport))))

#+CCL
(define foreign-host (http-stream)
  "Returns the foreign host for the remote connection via http-stream."
  (let* ((conn (slot-value http-stream 'ccl::conn))
         (pb (when conn (ccl::conn-pb conn))))
    (when pb
      (ccl:rref pb tcpiopb.status.remotehost))))

#+(and Allegro (not ACL5))
(defmethod local-port ((http-stream ipc:tcp-client-stream))
  (slot-value http-stream 'ipc::server-port))

#+(and Allegro (not ACL5))
(defmethod local-port ((http-stream ipc:tcp-server-stream))
  (slot-value http-stream 'ipc::port))

#+(and Allegro (not ACL5))
(defmethod foreign-host ((http-stream ipc:tcp-client-stream))
  (or (if (slot-boundp http-stream 'ipc::host-address)
	  (slot-value http-stream 'ipc::host-address)) ;What seems wanted
      (slot-value http-stream 'ipc::host)))

#+(and Allegro (not ACL5))
(defmethod foreign-port ((http-stream ipc:tcp-client-stream))
  (slot-value http-stream 'ipc::port))

#+ACL5
(defmethod local-port ((http-stream ipc:tcp-client-stream))
   (socket:local-port http-stream))

#+ACL5
(defmethod local-port ((http-stream ipc:tcp-server-stream))
   (socket:local-port http-stream))

#+ACL5
(defmethod foreign-host ((http-stream ipc:tcp-client-stream))
  ;;SOCKET::REMOTE-HOST returns greater than 32 bits on Alpha
  (ldb (byte 32. 0) (socket:remote-host http-stream)))

#+ACL5
(defmethod foreign-port ((http-stream ipc:tcp-client-stream))
   (socket:remote-port http-stream))

#+ACLPC
(setf (fdefinition 'local-port) (fdefinition 'socket:local-port))

#+ACLPC
(setf (fdefinition 'foreign-port) (fdefinition 'socket:remote-port))

#+ACLPC
(setf (fdefinition 'foreign-host) (fdefinition 'socket:remote-host))

;;;------------------------------------------------------------------- 
;;;
;;; FILE RELATED OPERATIONS
;;;

(define file-stream-creation-date (file-stream)
  "Returns the creation date in universal time for FILE-STREAM's source file."
  (file-write-date file-stream))

(declaim (inline file-stream-length-in-bytes))

(define file-stream-length-in-bytes (file-stream)
  "Returns the length in bytes for FILE-STREAM's source file."
  (file-length file-stream))

;;; Update from http:mac;mcl
#-:CCL-3
(defmethod file-length-in-bytes ((pathname pathname) &optional new-length)
   (declare (ignore new-length))
   (with-open-file (file-stream pathname)
       (file-length file-stream)))

#+:CCL-3
(defmethod file-length-in-bytes ((pathname pathname) &optional new-length)
  (declare (ignore new-length))
  (ccl::file-data-size pathname))

#+(and CCL (not :CCL-3))
(defmethod file-length ((pathname pathname) &optional new-length)
  (declare (ignore new-length))
  (with-open-file (file-stream pathname)
    (file-length file-stream)))

#+:CCL-3
(defmethod file-length ((pathname pathname) &optional new-length)
  (declare (ignore new-length))
  (ccl::file-data-size pathname))

#+(and CCL (not :CCL-3))
(defmethod file-creation-date ((pathname pathname))
  (with-open-file (file-stream pathname)
    (file-write-date file-stream)))

#+:CCL-3
(defmethod file-creation-date ((pathname pathname))
  (ccl::file-write-date pathname))

#-(or Genera CCL)
(defmethod file-creation-date ((pathname pathname))
  (file-write-date pathname))

(defmethod file-modification-date ((pathname pathname))
  (file-write-date pathname))

(defun file-stream-modification-date (stream)
  (file-write-date stream))

(declaim (inline file-stream-version))

(defun file-stream-version (file-stream)
  (file-stream-creation-date file-stream))

(declaim (inline file-version))

(defun file-version  (pathname)
  (when (probe-file pathname)
    (file-creation-date pathname)))

#-(or :CCL-3 Allegro ACLPC)
(define file-properties (pathname)
  "Returns the length in bytes  and the creation in in universal time 
for FILE-STREAM's source file."
  (declare (values length-in-bytes creation-date version))
  (with-open-file (file-stream pathname)
    (values (file-stream-length-in-bytes file-stream)
            (file-stream-creation-date file-stream)
            (file-stream-version file-stream))))

#+:CCL-3
(declaim (inline file-properties))

#+allegro ;Ken Anderson
(define file-properties (pathname)
  "Returns the length in bytes  and the creation in in universal time 
for FILE-STREAM's source file."
  (declare (values length-in-bytes creation-date version))
  (let* ((pathname (namestring pathname))
	(date (excl::filesys-write-date pathname)))
    (values (excl::filesys-size pathname)
	    date
	    date)))

#+ACLPC
(define file-properties (pathname)
  "Returns the length in bytes  and the creation in in universal time 
for FILE-STREAM's source file."
  (declare (values length-in-bytes creation-date version))
  (let ((date (file-write-date pathname)))
    (values (if (pathname-directory-p pathname)
		0
	      (file-length pathname))
	    date
	    date)))

#+:CCL-3
(defun file-properties (pathname)
  "Returns the length in bytes  and the creation in in universal time 
for FILE-STREAM's source file."
  (declare (values length-in-bytes creation-date version))
  (values (ccl::file-data-size pathname)
          (ccl::file-write-date pathname)
          (file-version pathname)))

(declaim (inline pathname-directory-p))

(defun pathname-directory-p (pathname)
  "Returns non-null if PATHNAME denotes a directory."
  #+CCL
  (ccl::directoryp pathname)
  #+Allegro ;OBC
  (excl::file-directory-p pathname)
  #+ACLPC
  (if (member (pathname-name pathname) '(nil :wild))
      (if (member (pathname-type pathname) '(nil :wild))
	  (acl::directory-exists-p pathname)))
  #+(and UNIX (not Allegro)) ;poorfellow's version - OBC
  (and (unix-sh-test "-d " pathname) t))

;;; When it's unclear what shell is used by CL, check the shell argument SHELL
;;; -- OBC
(defun system (arg)
  #+LispWorks
  (sys::call-system arg)
  #+Allegro
  (excl:shell arg)
  #+KCL
  (sys::system arg))

;;; OBC added
(defun unix-sh-test (cond path &aux (strpath (cond ((stringp path) path)
                                                   ((pathnamep path)
                                                    (namestring path)))))
    #+UNIX
    (if strpath
        (= (system (format nil "test ~a \"~a\" || exit 1"
                           cond strpath))       ; This is the fix: "strpath"!!
           0)
        nil)
    #-UNIX
    (and cond strpath t))

(defun directory-list (pathname &rest options)
  "Returns a lisp Machine style directory listing."
  (let ((pathnames #+(or UNIX ACLPC ACL5)
                   (apply #'unix-directory-list* (merge-pathnames pathname "*.*") nil options)
                   #+MCL
                   (directory (merge-pathnames pathname "*.*")
                              :files t
                              :resolve-aliases t
                              :directories t)))
    (when (member :sorted options)
      (setq pathnames (sort pathnames
			    #+(or UNIX ACLPC) #'directory-name-type<
			    #-(or UNIX ACLPC) #'original-name-type<)))
    (loop with length and creation-date
          for path in pathnames
          do (multiple-value-setq (length creation-date)
               (file-properties path))
          collect `(,path 
                    ,.(when length `(:length-in-bytes ,length))
                    ,.(when creation-date `(:creation-date ,creation-date))))))

(declaim (inline alphalessp))

(defun alphalessp (a b)
  (string< a b))

(define directory-info (pathname &key (name :wild) (type :wild) (version :newest) (sort-pathnames t)
                                 directories)
  "Returns a poperty list of information for every file in the directory PATHNAME
that matches pathnames wildcards. Directories are included when directories is non-null."
  (declare (notinline))
  (flet ((get-directory-listing (p &optional (sort-p sort-pathnames))
           (let ((args nil))
             (declare (dynamic-extent args))
             (when sort-p (push :sorted args))
             (when directories (push :directories args))
             (apply #'directory-list p :no-extra-info args)))
         (pattern (path type)
           (make-pathname :host (pathname-host path)
			  :device (pathname-device path)
                          :directory (pathname-directory path)
                          :name (etypecase name
                                  (keyword
                                    (ecase name
                                      (:wild "*")))
                                  (string name))
                          :type (etypecase type
                                  (keyword
                                    (case type
                                      (:wild "*")
                                      (t (symbol-name type))))
                                  (string type))
                          :version (etypecase version
                                     (keyword
                                       (ecase version
                                         (:wild nil)
                                         (:newest :newest))))))
         (sorter (e1 e2)
           (let ((p1 (car e1))
                 (p2 (car e2)))
             (and (alphalessp  (pathname-name p1) (pathname-name p2))
                  (let ((t1 (pathname-type p1))
                        (t2 (pathname-type p2)))
                    (cond ((and t1 t2)
                           (alphalessp t1 t2))
                          (t1 nil)
                          (t t)))))))
    (let ((p (pathname pathname)))
      (typecase type
        (keyword
          (ecase type
            (:wild (get-directory-listing (pattern p "*")))))
        (string
          (get-directory-listing (pattern p type)))
        (cons
          (loop for type in type
                nconc (get-directory-listing (pattern p type)) into paths
                finally (return (if sort-pathnames
                                    (sort paths #'sorter)
                                    paths))))))))

#+(or UNIX ACL5) ;OBC ;kr
(defun unix-directory-pathname (pathname)
  (let ((lastdir (pathname-name pathname)))
    (if lastdir
        (make-pathname :directory (append (pathname-directory pathname)
                                          (list lastdir))
		       :name nil
		       :type nil
		       :version nil
                       :defaults pathname))))

#+ACLPC ;OBC
(defun unix-directory-pathname (pathname)
  (make-pathname :host (pathname-host pathname)
		 :device (pathname-device pathname)
		 :directory (pathname-directory pathname)
		 :name nil
		 :type nil
		 :version nil))

#+ACLPC ;OBC
(defun directory-directories (pathname &key (current t) (parent t))
  (let ((dirpath (make-pathname :host (pathname-host pathname)
				:device (pathname-device pathname)
				:directory (append (or (pathname-directory pathname)
						       '(:relative))
						   '(:wild))
				:name nil
				:type nil
				:version nil))
	dirs)
    (setq dirs (ignore-errors (directory dirpath)))
    (unless current
      (setq dirs (delete "." dirs :key #'(lambda (p) (first (last (pathname-directory p)))) :test #'equal :count 1)))
    (unless parent
      (setq dirs (delete ".." dirs :key #'(lambda (p) (first (last (pathname-directory p)))) :test #'equal :count 1)))
    dirs))

#+(or UNIX ACLPC ACL5) ;OBC ;kr
(defun unix-directory-list* (pathname predicate &rest options)
  (multiple-value-bind (dirs error)
      (ignore-errors (directory pathname
				;; This fixes directory problem on UNIX for Allegro
				:directories-are-files #+(version>= 4 3) t #-(version>= 4 3) nil))
    (when error
      (warn "Error reading directory ~s." pathname)
      (return-from unix-directory-list* nil))
    #+ACLPC
    (if (member :directories options)
	(setq dirs (append dirs (directory-directories pathname :current nil :parent nil))))
    #-ACLPC
    (if (not (member :directories options))
        (setq dirs (loop for file in dirs
                       unless (pathname-directory-p file)
                       collect file))
      #+(or (not Allegro) ;Fix for non Allegro case
	    (and Allegro (version>= 4 3))) ;Fix ACL 4.3 bug!
      (setq dirs (loop for file in dirs
                     when (pathname-directory-p file)
                     collect (unix-directory-pathname file)
                     else collect file)))
    (if predicate
        (setq dirs (loop for file in dirs
                       when (funcall predicate file)
                       collect file)))
    dirs))

(defun original-name-type< (x y)
  (and (string< (pathname-name x)
		(pathname-name y))
       (string< (pathname-type x)
		(pathname-type y))))

;; Use this to sort directories firsts then sorts remaining
;; files by name and type in one pass. Awful hacks - OBC
;;
(defun directory-name-type< (x y)
  (block nil
    (let (c nx ny)
      (flet ((pname (path)
	       (let ((n (and (null (pathname-name path))
			     (null (pathname-type path))
			     (first (last (pathname-directory path))))))
		 (if (stringp n)
		     n))))
	(setq nx (pname x) ny (pname y))
	(setq c (string< nx ny)))
      (if nx
	  (if ny
	      (return c)
	    (return 0))
	(if ny
	    (return nil))))
      (let ((a (string< (pathname-name x)
			(pathname-name y))))
	(if a
	    (let ((b (string< (pathname-type x)
			      (pathname-type y))))
	      (if b
		  (+ a b)
		a))))))

;;; Fall back
(defun namestring< (x y)
  (string< (namestring x) (namestring y)))

(defun directory-list* (pathname predicate &rest options)
  "Accepts the options :FILES :DIRECTORIES :SORTED :PROPERTIES."
  (let ((pathnames #+(or UNIX ACLPC ACL5) ;OBC ;kr
                   (apply #'unix-directory-list* (merge-pathnames pathname "*.*") predicate options)
                   #+MCL
                   (directory (merge-pathnames pathname "*.*")
                              :files (member :files options)
                              :test predicate
                              :resolve-aliases t
                              :directories (member :directories options))))
    (when (member :sorted options)
      (setq pathnames (sort pathnames
			    #+(or UNIX ACLPC) #'directory-name-type<
			    #-(or UNIX ACLPC) #'original-name-type<)))
    (cond ((member :properties options)
           (loop for path in pathnames
                 collect  (multiple-value-bind (length creation-date)
                              (file-properties path) 
                            `(,path 
                              ,.(when length `(:length-in-bytes ,length))
                              ,.(when creation-date `(:creation-date ,creation-date))))))
          (t pathnames))))

(define create-directories-recursively (pathname)
  "Recursively create directories according to the directories present in PATHNAME."
  #+CCL
  (ccl:create-directory pathname :if-exists :error)
  #-CCL
  (create-directory-recursively pathname))

;;; For implementations where pathname-directory does
;;; not return NIL when there is no directory in the pathname.
;;; -- OBC
(defun pathname-dirs (pathname)
  (let ((dirs (pathname-directory pathname)))
    (and (consp dirs) dirs)))

(defun %create-a-directory (path)
  (let ((str (namestring path))) ;;(directorystring path)
    (declare (ignorable str))
    #+(and UNIX (not ACL5))
    (if (eql (system (format nil "mkdir ~S" str)) 0)
	path)
    #+MCL (if (ccl:create-directory path) path)
    #+ACLPC (if (acl::create-directory str) path) ;;(win:mkdir str)
    #+ACL5 (if (excl::make-directory path) path)
    #-(or UNIX MCL ACLPC ACL5)
    (warn "Create a directory not implemented for this system.")))
  
;;; -- OBC
(defun create-a-directory (path &optional (error-p t))
  (or (%create-a-directory path)
      (if error-p
	  (if (probe-directory path)
	      (error "create-a-directory: file or directory already exists: ~a" path)
	    (error "create-a-directory: failed on: ~a" path))
	path)))

;;; Return path if you can write in it or over it.
;;; -- OBC
(defun file-permit-p (path &optional (permission "w"))
  #-UNIX
  (declare (ignore permission))
  #+UNIX
  (and (unix-sh-test (concatenate 'string "-" permission) path) path)
  #-UNIX
  path)

;;; -- OBC
(defun create-directory-recursively (path &optional (error-p t))
  (setq path (translate-logical-pathname (pathname path)))
  ;; most system cannot create a whole directory from scratch so
  ;; recursively create directories for path to be valid
  (let ((host (pathname-host path))
	(device (pathname-device path))
	(dirs (pathname-dirs path)))
    #+UNIX
    (let ((dir (make-pathname :host host :device device :directory dirs)))
      (if (eql (system (format nil "mkdir -p ~S" (namestring dir))) 0)
	  (return-from create-directory-recursively dir)))
    (let ((order-dirs (nreverse (maplist #'reverse (reverse dirs))))
	  lastpath result)
      (dolist (dirs #-(and ACLPC (not ACL3.0)) order-dirs
		    #+(and ACLPC (not ACL3.0)) (cddr order-dirs))
	(setq lastpath (make-pathname :host host
				      :device device
				      :directory dirs))
	(cond ((probe-directory lastpath)
	       (setq result lastpath))
	      (t
	       (if error-p
		   (setq result (create-a-directory lastpath error-p))
		 (if (and result (file-permit-p result))
		     (setq result (create-a-directory lastpath error-p))
		   ;; quiet and early termination, don't want to bother
		   ;; user with any low system messages since ERROR-P is NIL
		   (return))))))
      result)))

(defgeneric probe-directory (pathname)
  (:documentation "Returns non-null if the directory pathname exists."))

(defmethod probe-directory ((pathname pathname))
  (setq pathname (translate-logical-pathname pathname))
  (pathname-directory-p (make-pathname :host (pathname-host pathname)
				       :device (pathname-device pathname)
				       :directory (pathname-directory pathname))))

(defmethod probe-directory ((pathname string))
  (probe-directory (pathname pathname)))
   
(define-macro with-automatic-login ((host user-id user-pw) &body body)
  "Supplies userid and PW to ensure successul FTP login to host with BODY."
  `(progn (notify-log-window "~&(WITH-AUTOMATIC-LOGIN (~S ~S ~S) - Not available on MAC"
                             ,host ,user-id ,user-pw)
          ,@body))

(define ftp-directory-info (directory &optional (user-id "anonymous") (user-pw (user-mail-address)))
  "Returns a list of pathname spec for directory just like DIRECTORY-INFO.
If a network error is encountered, this returns NIL."
  (let* ((path (pathname directory))
         (host (pathname-host path)))
    (handler-case 
      (with-automatic-login (host user-id user-pw)
        ;; ansi CL directory fails due to :fast option  3/13/94 -- JCMa.
        (directory-info directory))
      ;; handle remote connection problems, including dead host, refused connection.
      (remote-network-error () nil))))

(define ftp-copy-file (from-pathname to-stream &key (element-type 'character)
                                     (user-id "anonymous") (user-pw (user-mail-address)))
  "Copies the content of FROM-PATHNAME to TO-STREAM. 
If a network error is encountered, this returns NIL, otherwise T.
ELEMENT-TYPE is the ANSI file openning argument."
  (let ((host (pathname-host from-pathname)))
    (handler-case 
      (with-automatic-login (host user-id user-pw)
        (with-open-file (ftp-stream from-pathname :direction :input :element-type element-type)
          (http::stream-copy-until-eof ftp-stream to-stream)
          (values t)))
      ;; handle remote connection problems, including dead host, refused connection.
      (remote-network-error () nil))))

#-ACLPC
(declaim (inline char-bits))

;; returns font or shift bits
;; not an issue if they are not stored in characters.
#-ACLPC
(defun char-bits (char)
  (declare (ignore char))
  0)

(declaim (inline string-thin))

;; removes fonts from Lispm Fat strings.-- JCMa 12/30/1994.
(defun string-thin (string)
  "Strips font description"
  string)

;;;------------------------------------------------------------------- 
;;;
;;; SECURE SUBNETS
;;;

;; move into portable code.-- JCMa 12/30/1994.
(define parse-internet-addresses (ip-addresses)
  "Parses IP-ADDRESSES into a list of ip-address specifications."
  (loop for ip-address in ip-addresses
        for parsed-address = (%parse-host-address ip-address)
        when parsed-address
          collect parsed-address)) 

;; Presently only does exact ip number matches.
; Needs to match partial addresses (e.g., 128.52.0.0) -- JCMa 12/30/1994.
(define ip-host-trusted-p (address secure-subnets &optional network)
  "Returns non-null if IP-address address is trusted given secure-subnets."
  (declare (ignore network))
  (flet ((address-match-p (addr1 addr2)
           (= addr1 addr2)))
    (declare (inline address-match-p))
    (cond (secure-subnets
           (member (etypecase address
                     (integer address)
                     (string (%parse-host-address address)))
                   secure-subnets
                   :test #'address-match-p))
          (t t))))

;;;------------------------------------------------------------------- 
;;;
;;; STREAM HACKING
;;;

#+CCL
(define-macro with-binary-stream ((stream direction) &body body)
  "Turns STREAM into a binary stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  `(unwind-protect
       (progn ,(ecase direction
                 (:output `(ccl:binary-output-mode ,stream))
                 (:input `(ccl:binary-input-mode ,stream))
                 (:both `(progn (ccl:binary-output-mode ,stream)
                                (ccl:binary-input-mode ,stream))))
              ,@body)
     ,(ecase direction
        (:output `(ccl:ascii-output-mode ,stream))
        (:input `(ccl:ascii-input-mode ,stream))
        (:both `(progn (ccl:ascii-output-mode ,stream)
                       (ccl:ascii-input-mode ,stream))))))
#+ACLPC
(defmethod finish-outputs ((stream acl:compound-stream))
  (finish-output stream)
  (if (input-stream-p stream)
      (finish-outputs (two-way-stream-output-stream stream)) ;Really two-way
    ;; Really a broadcast stream
    (loop for stream in (broadcast-stream-streams stream)
      do (finish-outputs stream))))

#+ACLPC
(defmethod finish-outputs ((stream stream))
  (finish-output stream))

#+Allegro
(defmethod finish-outputs ((stream two-way-stream))
  (finish-output stream)
  (finish-outputs (two-way-stream-output-stream stream)))

#+Allegro
(defmethod finish-outputs ((stream broadcast-stream))
  (finish-output stream)
  (loop for stream in (broadcast-stream-streams stream)
      do (finish-outputs stream)))

#+Allegro
(defmethod finish-outputs ((stream stream:fundamental-output-stream))
  (finish-output stream))

#+FRANZ-INC
(defvar *force-binary-stream-mode* nil)

#+FRANZ-INC ;Actually not required for Allegro any longer but for ACLPC
(define-macro with-binary-stream ((stream direction) &body body)
  `(let ((*force-binary-stream-mode* (and ,direction ,stream)))
     ,@body))

#-(or CCL FRANZ-INC)
(define-macro with-binary-stream ((stream direction) &body body)
  `(progn
     (warn "WITH-BINARY-STREAM may not be implemeted yet.")
     ,@body))

#+ACLPC
(defmethod http:stream-copy-until-eof ((from-stream stream) (to-stream stream) &optional copy-mode)
  (declare (ignorable copy-mode))
  (if *force-binary-stream-mode*
      (transfer-buffer-streams from-stream to-stream)
    (call-next-method)))

#+ACL5
(defmethod http:stream-copy-until-eof ((from-stream stream) (to-stream stream) &optional copy-mode)
   (declare (ignore copy-mode))
   (if *force-binary-stream-mode*
      (transfer-buffer-streams from-stream to-stream)
      (ecase copy-mode
        (:text
          (loop for line = (read-line from-stream nil)
            while line
            do (write-line line to-stream)))
        ((:binary :crlf)
         (with-binary-stream (from-stream :input)
           (with-binary-stream (to-stream :output)
             (loop for byte = (read-byte from-stream nil)
               while byte
               do #+ACL5 (write-char (code-char byte) to-stream)
                  #-ACL5 (write-byte byte to-stream))))))))

;;; Allegro provides a real stream implementation from X3J13
;;; STREAM-DEFINITION-BY-USER, Version 1, 22-Mar-89 by David N. Gray
;;;
#+(and Allegro (not ACL5))
(defmethod http:stream-copy-until-eof ((from-stream stream:fundamental-input-stream) (to-stream stream:fundamental-output-stream) &optional copy-mode)
  (declare (ignore copy-mode))
  (stream-buffer-copy-until-eof from-stream to-stream))

#+(and Allegro (not ACL5))
(defmethod http:stream-copy-until-eof ((from-stream stream:fundamental-binary-input-stream) (to-stream stream:fundamental-output-stream) &optional copy-mode)
  (declare (ignore copy-mode))
  (with-binary-stream (from-stream :output)
    (stream-buffer-copy-until-eof from-stream to-stream)))

(define-macro with-optimal-stream-buffer (() . body)
  #+(and Allegro (not ACL5)) `(excl:with-stream-buffer-size (1024) ,@body)
  #+(or (not Allegro) ACL5) `(progn ,@body))

#+(and Allegro (not ACL5))
(defmethod stream-buffer-copy-until-eof ((from-stream stream:fundamental-input-stream) (to-stream stream:fundamental-output-stream))
  (finish-outputs to-stream) ; Fresh start
  (loop with buffer-size = (excl:stream-buffer-length from-stream)
      with buffer = (excl:stream-input-buffer from-stream)
      as end = (read-sequence buffer from-stream :end buffer-size)
      do #+(and allegro-version>= (version>= 4 3) sgi) ;Jeff Long mailto:long@eecs.ukans.edu
	 (stream:stream-write-sequence to-stream buffer 0 end)
	 #-(and allegro-version>= (version>= 4 3) sgi)
	 (write-sequence buffer to-stream :end end)
      when (< end buffer-size)
      do (finish-outputs to-stream) ; Synchronize streams
      until (zerop end)))

;;; Proxy type cases and known 4.3 exceptions
#+(and Allegro (not ACL5))
(defmethod stream-buffer-copy-until-eof ((from-stream excl::bidirectional-multivalent-8-bit-stream) (to-stream stream:fundamental-output-stream))
  (finish-outputs to-stream) ; Fresh start
  (loop with buffer-size = (excl:stream-buffer-length from-stream)
      with buffer = #-(version>= 4 3) (excl:stream-input-buffer from-stream)
		    #+(version>= 4 3) (make-string buffer-size)
		    ;; Suddenly in 4.3 using the stream buffer does not work
		    ;; as the first character gets overwritten.
      as end = (read-sequence buffer from-stream :end buffer-size)
      do (write-sequence buffer to-stream :end end)
      when (< end buffer-size)
      do (finish-outputs to-stream) ; Synchronize streams
      until (zerop end)))

(defvar *max-copy-buffer-size* 1024)

;;; Would be able to use one resource if only it discriminated
;;; based on arguments passed at creation...
;;;
(defun make-copy-bytes-buffer (resource)
  (declare (ignore resource))
  (make-array *max-copy-buffer-size* :element-type '(unsigned-byte 8)))

(clim-sys:defresource copy-bytes-buffer ()
  :constructor make-copy-bytes-buffer)

(defun make-copy-chars-buffer (resource)
  (declare (ignore resource))
  (make-string *max-copy-buffer-size*))

(clim-sys:defresource copy-chars-buffer ()
  :constructor make-copy-chars-buffer)

;;; Specialized from http:server;utils
#+Allegro
(defmethod http::stream-copy-bytes ((from-stream stream:fundamental-character-input-stream) (to-stream stream:fundamental-output-stream) n-bytes &optional copy-modes)
  (declare (ignore copy-modes))
  #+ignore
  (loop for bytes upfrom 0
        while (< bytes n-bytes)
      do (write-byte (read-byte from-stream) to-stream))
  (clim-sys:using-resource (buffer copy-chars-buffer)
    (loop with end = n-bytes
	do (setq end (read-sequence buffer from-stream :end (min *max-copy-buffer-size* n-bytes)))
	   (write-sequence buffer to-stream :end end)
	while (plusp (decf n-bytes end)))))

#+Allegro
(defmethod http::stream-copy-bytes ((from-stream stream:fundamental-binary-input-stream) (to-stream stream:fundamental-output-stream) n-bytes &optional copy-modes)
  (declare (ignore copy-modes))
  #+ignore
  (loop for bytes upfrom 0
        while (< bytes n-bytes)
      do (write-byte (read-byte from-stream) to-stream))
  (clim-sys:using-resource (buffer copy-bytes-buffer)
    (loop with end = n-bytes
	do (setq end (read-sequence buffer from-stream :end (min *max-copy-buffer-size* n-bytes)))
	   (write-sequence buffer to-stream :end end)
	while (plusp (decf n-bytes end)))))

;;; Specialized from http:server;utils
#+Allegro
(defmethod http::stream-copy-byte-range ((from-stream stream:fundamental-input-stream) (to-stream stream:fundamental-output-stream) start last)
  (cond ((file-position from-stream start)
	 ;; Copy from start to last non included - spec. finally fixed 1/98!
	 (http::stream-copy-bytes from-stream to-stream (- last start)))
	(t (error "Unable to set file position for byte range copy."))))

;;;------------------------------------------------------------------- 
;;;
;;; ABORTING CONNECTIONS
;;; 

#+CCL
(define live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  (if (slot-value http-stream 'ccl::conn) t nil))

#+ignore ;;(from Genera)
(define live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  (scl:send http-stream :connected-p))

#-CCL
(define live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
  (and #+Allegro
       (ipc:tcp-client-alive http-stream)
       (open-stream-p http-stream)
       #+ACLPC
       (not (ipc::stream-closed-p http-stream))))

(declaim (inline abort-http-stream))

(define abort-http-stream (http-stream)
  "Closes http-stream in abort mode.  
This will push any output in the transmit buffer and catch any network errors.
Takes care to clean up any dangling pointers."
  (handler-case 
    (close http-stream :abort t)
    (file-error ())
    (network-error ())))

#+ignore
(export 'abort-http-stream :www-utils)

;; these definitions should be moved into the shared code -- JCMa 12/30/1994.
(define abort-current-connection ()
  "Aborts the computation associated with the current HTTP connection."
  (signal 'http-abort))

(declaim (inline abort-if-connection-dead))

(define abort-if-connection-dead (http-stream)
  "Aborts the HTTP connection if the TCP/IP connection over HTTP-STREAM
has died, i.e. the remote host is no longer connected."
  (unless (live-connection-p http-stream)
    (abort-current-connection)))

;;;------------------------------------------------------------------- 
;;;
;;; LOGGING EVENTS
;;; 

;;Bound to the HTTP server log window when one exists.
(defvar *log-window* nil)

;;Returns the active log window
#+CCL
(defun log-window ()
  (flet ((make-log-window (host-name)
           (make-instance 'ccl::fred-window
                          :scratch-p t
                          :color-p t
                          :window-show t
                          :window-title (format nil "MAC Common Lisp HTTP Log (~:(~A~))" host-name)
                          :window-layer 0)))
    (ccl:without-interrupts
      (let ((window *log-window*))
        (cond 
          ((and window (ccl::wptr window))
           window)
          (t (setq *log-window*  (make-log-window (local-host-domain-name)))))))))

#+(and CCL (not ccl-3))
(defmacro with-log-window-stream ((stream) &body body)
  `(let* ((window (log-window))
              ;;;Karsten 5/17/95 A Fred-Window is already the right stream in 2.01, 
          ;; window-key-handler is undefined
          (,stream window))
     (ccl::set-mark (ccl::fred-buffer window) t)
     (fresh-line ,stream)
     (ccl::window-show-cursor window)
     (prog1 (progn . ,body)
            (ccl::fred-update window))))

;; Bound to the process queue for the HTTP server log window when one exists.
#+:ccl-3
(defvar *log-window-process-queue* nil)

#+:ccl-3
(define log-window-process-queue ()
  "Returns the process queue for the CL-HTTP log window."
  (ccl:without-interrupts
    (cond (*log-window-process-queue*)
          (t (setq *log-window-process-queue* (ccl::make-process-queue 
                                                "HTTP Log Window Process Queue"))))))

#+:ccl-3
(define-macro with-log-window-stream ((stream) &body body)
  "Use this macro to write to the log window, which is bound to STREAM.
A process queue prevents forms executed within BODY from colliding with
other processes trying to write the log simultaneously."
  `(ccl::with-process-enqueued 
     ((log-window-process-queue) ccl::*current-process* "HTTP Log Window Wait")
     (let* ((window (log-window))
            (,stream (ccl::window-key-handler window)))
                                                ;how to make sure we're inserting at the end of the buffer????
       (ccl::set-mark (ccl::fred-buffer window) t)
       (fresh-line ,stream)
       (ccl::window-show-cursor window)
       (prog1 (progn . ,body)
              (ccl::fred-update window)))))

#-Allegro ;; No idea what this is  - OBC
(declaim (ftype http::write-standard-time))

#+CCL
(define notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window."
  (with-log-window-stream (stream)
                          (fresh-line stream)
                          (write-char #\[ stream)
                          (http::write-standard-time (get-universal-time) stream)
                          (write-string "]  " stream)
                          (apply #'format stream format-string format-args)))

#-CCL
(define notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window."
  (let ((stream *trace-output*))
    (fresh-line stream)
    (write-char #\[ stream)
    (http::write-standard-time (get-universal-time) stream)
    (write-string "]  " stream)
    (apply #'format stream format-string format-args)))

#+CCL
(define expose-log-window ()
  "Exposes the window."
  (let ((selected-window ccl::*selected-window*))
    ;; bring the log window to the front
    ;; (ccl::window-bring-to-front (log-window)) ;; causes confusion with selection
    (ccl::window-select (log-window))
    ;; but don't lose selection of the current window
    (ccl::window-select selected-window)))

#-CCL
(define expose-log-window ()
  "Exposes the Log window. Does nothing."
  nil)

#-CCL
(defmacro with-log-window-stream ((stream) &rest body)
  `(let ((,stream *trace-output*))
     (fresh-line ,stream)
     ,@body))

(define common-logfile-notify (server)
  "Issues a notification of server activity on a window."
  (with-log-window-stream (stream)
    (http::write-common-logfile-entry server stream)))

(define log-http-server-error (format-string &rest format-args)
  (apply #'notify-log-window  format-string format-args))

(define http::log-http-request (client-host method url-string case)
  #|(log-http-access client-host
                   (if accepted-p "Serving ~A ~S" "Rejected ~A ~S") method url-string)|#
  (let ((host-name (http::host-domain-name client-host)))
    (ecase case
      (:accepted
        (notify-log-window "HTTP Serving ~A: Method ~S: ~S" host-name method url-string))
      (:rejected
        (notify-log-window "HTTP Rejected ~A: Method ~S: ~S" host-name method url-string))
      (:timeout
        (notify-log-window "HTTP Timed Out Serving ~A: Method ~S: ~S" host-name method url-string)))))

;; export the logging symbols
#+ignore
(export '(log-window notify-log-window expose-log-window common-logfile-notify) :www-utils)

;;;------------------------------------------------------------------- 
;;;
;;; MAIL SENDING  
;;;

(defmethod stream-tyi (stream)
  (read-char stream))

(defmethod stream-tyo (stream char)
  (write-char char stream))

;;(tcp-service-port-number "smtp")

(defvar *unix-services-cache* nil)

(defun tcp-service-port-number (srvstr)
  (if (symbolp srvstr)
      (setq srvstr (string-downcase (string srvstr))))
  (or (and (integerp srvstr) srvstr)
      (rest (assoc srvstr *unix-services-cache* :test #'equal))
      (let ((port
	     #+UNIX
	     (www-utils::system (format nil "exit `grep ~s /etc/services | awk '{ print $2 }' | awk -F/ '{ print $1 }'`" srvstr))))
	(if (or (null port) (zerop port))
	    (if (equal srvstr "smtp")
		(setf port 25)
	      (error "Unknown service ~s in /etc/services" srvstr)))
	(setq *unix-services-cache*
	  (nconc *unix-services-cache* (list (cons srvstr port))))
	port)))

(deftype acl-mailer-stream () 'ipc:tcp-client-stream)

(setf (find-class 'acl-mailer-stream) (find-class 'ipc:tcp-client-stream))

#+Franz-Inc
(defun find-direct-connection (host &key port)
   ;;; Add other ports favorite non portable way to open a connection here:
   #+(and Allegro (not (version>= 5 0)))
   (make-instance 'ipc::tcp-client-stream :host host :port port)
   #+ACLPC
   (socket:make-socket :remote-host host :remote-port port)
   #+ACL5
   (change-class (socket:make-socket :remote-host host :remote-port port) 'ipc:tcp-client-stream))

#+Franz-Inc
(defmethod %open-mailer-stream (host port &rest args)
  #+(and Allegro (not (version>= 5 0)))
  (make-instance 'acl-mailer-stream :host host :port port)
  #+ACLPC
  (socket:make-socket :remote-host host :remote-port port)
  #+ACL5
  (change-class (socket:make-socket :remote-host host :remote-port port) 'ipc:tcp-client-stream))

;;; In MCL 3.0, http:mac;server;mail.lisp implements these for real
;;; based on the simple mailer in http:mac;server;smtp.lisp

#-(or :ccl-3 Franz-Inc)
(defun send-mail-from (from to subject mail-writer &key reply-to keywords comments
                                             file-references)
   (declare (ignore reply-to keywords comments file-references))
   (notify-log-window
     (with-output-to-string (stream)
         (format stream "~&~A~&From: ~A~&To: ~A~&Subject: ~A~2%"
		 #+ignore case "Mail Send:" from to subject)
         (princ mail-writer stream)
	 (terpri stream))))

;; need to interface to a mail sending program, e.g.eudora. -- JCMa 1/1/1995.
#-(or :ccl-3 Franz-Inc)
(define report-bug  (to subject format-string &rest format-args)
  (notify-log-window (with-output-to-string
                       (stream)
                       (format  stream "~&Report Bug:  To:~A~&Subject: ~A"
                                to subject)
                       (fresh-line stream)
                       (apply #'format stream format-string format-args))))

#+(and Allegro (not ACL5))
(progn
  (setf (fdefinition 'bytes-received) (fdefinition 'excl::bytes-received))
  (setf (fdefinition '(setf bytes-received)) (fdefinition '(setf excl::bytes-received)))
  (setf (fdefinition 'bytes-transmitted) (fdefinition 'excl::bytes-transmitted))
  (setf (fdefinition '(setf bytes-transmitted)) (fdefinition '(setf excl::bytes-transmitted))))

#+(or ACLPC ACL5)
(progn
  (defmethod bytes-received ((stream t)) 0)
  (defmethod (setf bytes-received) (val (stream t)) val)
  (defmethod bytes-transmitted ((stream t)) 0)
  (defmethod (setf bytes-transmitted) (val (stream t)) val)
)


;;;------------------------------------------------------------------- 
;;;
;;; MCL specific initializations 
;;;

#+CCL
(add-initialization "Maybe Enable EGC" '(ccl:egc  (ccl::egc-mmu-support-available-p)))


;;;------------------------------------------------------------------- 
;;;
;;; LOG RELATED PORTABILITY
;;;

(defun make-lock (name &key type &allow-other-keys)
  "Returns a lock named name that is suitable for use with with-lock-held."
  (declare (ignore type ignore))
  (clim-sys:make-lock name))

(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore mode))
  `(clim-sys:with-lock-held (,lock ,whostate)
     ,@body))

(defun %make-log-pathname (device directory name host)
  "Returns the pathname to which current log entries are written."
  (make-pathname
    :host host
    :device device
    :directory directory
    :name name
    :type "text"))

(defvar *time-zone* 0)

(defun time-zone (&optional update-p)
  (declare (ignorable update-p))
  #+Allegro
  excl::*time-zone*
  #-Allegro
  (flet ((get-time-zone ()
	   (multiple-value-bind (secs min hour day month year week-day
				 day-light-savings-p time-zone)
	       (decode-universal-time
		(get-universal-time))
	     (declare (ignore secs min hour day month year week-day
			      day-light-savings-p))
	     time-zone)))
    (cond ((and (not update-p) *time-zone*))
	  (t (setq *time-zone* (get-time-zone))))))

;;; Add fix to character types. ANSI CL? - OBC
;;;
#+(and Allegro (not (version>= 4 3)))
(excl:without-package-locks
 (deftype common-lisp:base-char () 'character))

#+(and Allegro (not (version>= 4 3)))
(excl:without-package-locks
 (deftype common-lisp:standard-char () 'character))

(defun periodic-tasks (&key exit (name "HTTP Daily Tasks"))
  (mapc #'clim-sys:process-kill (minp:find-process-named name :collect t))
  (if (not exit)
      (clim-sys:make-process-loop
       :name name
       :with ((next (next-3am-universal-time)) (lapse 0))
       :do (cond ((> (setq lapse (- next (get-universal-time))) 0)
		  (clim-sys:process-sleep lapse))
		 (t
		  (run-daily-server-tasks)
		  (clim-sys:process-yield)
		  (notify-log-window "Daily Tasks Completed.")
		  (setq next (next-3am-universal-time)))))))

;;; Avoid using these they are useless - OBC
;;;
#+ACLPC
(deftype common-lisp:base-char () 'character)

#+ACLPC
(deftype common-lisp:standard-char () 'character)

;; needs to set up a timer or alarm on the MAC that will cause lisp to execute
;; run-daily-server-tasks  at the universal time returned by next-3am-universal-time
;; -- JCMa 12/31/1994.
(define synchronize-daily-server-tasks ()
   ;; set up a timer for mighnight to start the 24 interval timer
   (next-3am-universal-time))

;;; Ken Anderson's optimal char-equal for 2 args
;;;
(locally (declare (optimize (speed 3) (safety 0)))
(let* ((N 256)
       (up (make-array N))
      (down (make-array N)))
  (dotimes (i N)
    (let ((c (#+Allegro cltl1:int-char #-Allegro int-char i)))
      (setf (svref up i) (char-upcase c)
	    (svref down i) (char-downcase c))))
  (defun %upcase(c)
    (declare (character c))
    (svref up (char-int c)))
  (defun %downcase (c)
    (declare (character c))
    (svref down (char-int c)))
  (defun %char-equal (a b)
    (declare (character a b))
    #+allegro				; For Lisp's with char's that are EQ
    (eq (the character (svref up (char-int a)))  ; 0.29372194 musec
	(the character (svref up (char-int b))))
    #-allegro
    (eql (the character (svref up (char-int a))) ; 0.5495606 musec
	 (the character (svref up (char-int b))))))
)

;;; HTTP Add-ons
;;;
(defpackage "HTTP"
  (:use)
  (:import-from "WWW-UTILS" "ADD-EXPORT-PATHNAME" "COMPILE-EXPORTS" "LOAD-EXPORTS" "LOGICAL-HOST-URL-LOCATION" "INSTALL-SYMBOLIC-HOST-SHADOW")
  (:export "ADD-EXPORT-PATHNAME" "COMPILE-EXPORTS" "LOAD-EXPORTS" "LOGICAL-HOST-URL-LOCATION" "INSTALL-SYMBOLIC-HOST-SHADOW"))



;;;------------------------------------------------------------------- 
;;;
;;; 
;;;

#|
genera process functions
  CLIM-SYS:PROCESS-WAIT-WITH-TIMEOUT - Function (WAIT-REASON TIMEOUT PREDICATE)
  CLIM-SYS:PROCESS-NAME - Function (PROCESS)
  CLIM-SYS:PROCESS-YIELD - Function ()
  CLIM-SYS:PROCESS-STATE - Function (PROCESS)
  CLIM-SYS:PROCESS-WAIT - Function (WAIT-REASON PREDICATE)
  CLIM-SYS:PROCESS-WHOSTATE - Function (PROCESS)
  CLIM-SYS:PROCESS-INTERRUPT - Function (PROCESS FUNCTION)
  CLIM-SYS:ENABLE-PROCESS - Function (PROCESS)
  CLIM-SYS:CURRENT-PROCESS - Function ()
  CLIM-SYS:DISABLE-PROCESS - Function (PROCESS)
  CLIM-SYS:ALL-PROCESSES - Function ()
  CLIM-SYS:MAKE-PROCESS - Function (FUNCTION &key :NAME)
  CLIM-SYS:RESTART-PROCESS - Function (PROCESS)
  CLIM-SYS:DESTROY-PROCESS - Function (PROCESS)

|#

;;; Not implemented CLIM-SYS:PROCESS-RUN-FUNCTION, using MP:PROCESS-RUN-FUNCTION
;;; for Allegro, why not use CLIM-SYS:MAKE-PROCESS??
;;;

#-Allegro
(defun process-run-function (name function &rest args)
  (clim-sys:make-process #'(lambda ()
			     (apply function args))
			 :name name))

(setf (fdefinition 'process-disable) #'clim-sys:enable-process
      (fdefinition 'process-enable) #'clim-sys:disable-process
      (fdefinition 'process-kill) #'clim-sys:destroy-process
      (fdefinition 'process-reset) #'clim-sys:restart-process)

;;; Why don't we use CLIM-SYS:MAKE-PROCESS in the first place
;;; and then ask the vendors to fix it? - OBC
;;;
(define make-process (process-name &key (priority 0) quantum
				   run-reasons #-acl5 background-p 
				   restart-after-reset warm-boot-action &allow-other-keys)
  "Creates a process using a portable set of options."
  (declare (ignore warm-boot-action restart-after-reset))
  ;;; Our version of CLIM-SYS is fixed
  (CLIM-SYS:MAKE-PROCESS nil
			 :name process-name
			 #-os-threads
			 :priority #-os-threads
				   priority 
			 #-os-threads
			 :quantum #-os-threads
				  quantum 
			 #-os-threads
			 :run-reasons #-os-threads
				      run-reasons
				      #-acl5
				      :background-p #-acl5
				       background-p))

#-Allegro
(defun process-wait (wait-reason predicate &rest args)
  (declare (dynamic-extent args))
  (clim-sys:process-wait wait-reason (if args
					 #'(lambda ()
					     (apply predicate args))
				       predicate)))

#-Allegro
(defun process-wait-with-timeout (wait-reason timeout predicate &rest args)
  #+dontdothis (declare (dynamic-extent args))
  (clim-sys:process-wait-with-timeout wait-reason timeout
				      (if args
					  #'(lambda ()
					      (apply predicate args))
					predicate)))

#+Allegro
(defmacro handle-scheduler-errors! (&rest forms)
  `(let (#1=#:result #2=#:error)
     (macrolet ((true-if-error (form)
		  `(progn (multiple-value-setq (#1# #2#) ,form)
			  (if #2# t #1#))))
       ,@forms
       (if #2#
	   (signal #2#)
	 #1#))))
  
;;; Now we need to pass args explicitely - Initially not a very useful thing.
;;; This is now required to tell the scheduler what UNIX FD it has to watch
;;; for. Now that's very useful - OBC.
;;;
#+Allegro
(defun process-wait (wait-reason predicate &rest args)
  #+dontdothis (declare (dynamic-extent args))
  (let (streams)
    (declare (dynamic-extent streams))
    (loop for arg in args
	when (and (streamp arg) (input-stream-p arg))
	do (pushnew arg streams))
    (handle-scheduler-errors!
     (if streams
	 (mp::wait-for-input-available
	  streams
	  :whostate "Wait for input from Unix FD(s)"
	  :wait-function (if args
			     #'(lambda (stream)
				 (declare (ignore stream))
				 (true-if-error (apply predicate args)))
			   #'(lambda (stream)
			       (declare (ignore stream))
			       (true-if-error (funcall predicate)))))
       (clim-sys:process-wait wait-reason (if args
					      #'(lambda ()
						  (apply predicate args))
					    predicate))))))

#+Allegro
(defun process-wait-with-timeout (wait-reason timeout predicate &rest args)
  #+dontdothis (declare (dynamic-extent args))
  (let (streams)
    (declare (dynamic-extent streams))
    (loop for arg in args
	when (and (streamp arg) (input-stream-p arg))
	do (pushnew arg streams))
    (handle-scheduler-errors!
     (if streams
	 (mp:wait-for-input-available
	  streams
	  :whostate "Wait for input from Unix FD(s)"
	  :timeout timeout
	  :wait-function (if args
			     #'(lambda (stream)
				 (declare (ignore stream))
				 (true-if-error (apply predicate args)))
			   #'(lambda (stream)
			       (declare (ignore stream))
			       (true-if-error (funcall predicate)))))
       (clim-sys:process-wait-with-timeout wait-reason
					   timeout
					   (if args
					       #'(lambda ()
						   (apply predicate args))
					     predicate))))))

#+Franz-Inc
(defun process-run-time (process)
  "Returns the amount of run time the process has accumulated, in microseconds."
  #+Allegro
  (* 1000
     (mp::process-cpu-msec-used process)) ;milsec
  #-Allegro
  1000) ; whynot?

#+Franz-Inc
(defun process-idle-time (process)
  "Returns the amount of time the process has been up, in seconds."
  #+Allegro
  (mp::process-start-secs process)
  #-Allegro
  1000000) ; whynot?


;;;------------------------------------------------------------------- 
;;;
;;; ATOMIC CREATION OF CRLF FILES
;;;

(defmethod http:valid-crlf-cache-file-p ((pathname pathname))
  ;; Since creation uses an atomic rename-file operation, the crlf is valid if
  ;; it exists and is newer than the source.
  (let* ((source (probe-file pathname))
         (canonical (http:crlf-pathname source))
         (c-probe-date (file-write-date canonical)))
    (cond ((and source c-probe-date (< (file-write-date source) c-probe-date))
           (values t source (truename canonical)))
          (t (values nil source (probe-file canonical) canonical)))))

(defmethod http:ensure-crlf-canonical-file ((pathname pathname))
  (multiple-value-bind (valid-crlf-cache-p source-pathname crlf-pathname canonical-pathname)
      (http:valid-crlf-cache-file-p pathname)
    (cond (valid-crlf-cache-p crlf-pathname)
          (t (and (atomic-crlf-canonicalize-file source-pathname canonical-pathname)
		  (http:crlf-pathname pathname))))))

(defun atomic-crlf-canonicalize-file (source-pathname canonical-pathname)
  (let ((lock-file (make-pathname :type (concatenate 'simple-string
						     (pathname-type canonical-pathname)
						     "-lock")
				  :defaults canonical-pathname)))
    ;; Our caller saw an out-of-date crlf file, so repeatedly try to create it.
    (loop (let ((lock-stream nil))
	    (unwind-protect
		(progn
		  ;; Atomically probe and create the lock file if it doesn't
		  ;; exist.
		  (clim-sys:without-scheduling
		   (setq lock-stream
			 (open lock-file :direction :output
			       :if-exists nil
			       :if-does-not-exist :create)))
		  (when lock-stream
		    ;; We have the lock, so noone else can create the file now.
		    ;; If the file is valid then it has just been create by another process.
		    (when (http:valid-crlf-cache-file-p source-pathname)
		      (return))
		    (let* ((temp-file (make-pathname
				       :type (concatenate 'simple-string
							  (pathname-type canonical-pathname)
							  "-temp")
				       :defaults canonical-pathname)))
		      ;; Make the file in a temporary place.
		      (http:crlf-canonicalize-file source-pathname temp-file)
		      ;; Unix rename is atomic, so other processes will see either
		      ;; the old out-of-date file or the new one.
		      (rename-file temp-file canonical-pathname)
		      (return t))))
	      (when lock-stream
		(close lock-stream)
		(delete-file lock-file))))
	  (clim-sys:process-wait-with-timeout
	   "CRLF Wait"
	   1
	   #'(lambda () (not (probe-file lock-file)))))))
