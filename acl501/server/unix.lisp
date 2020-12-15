;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: www-utils -*-

;;; (C) Copyright 1994-1995, John C. Mallery.
;;;     All Rights Reserved.
;;;
;;; (C) Copyright 1995, Olivier (OBC).
;;;	All Rights Reserved -- Allegro UNIX & PC extensions.

(eval-when (compile load eval)
  ;; this was exported in server/package.lisp, we have to get rid of
  ;; it so that it can be imported from clim-sys
  (unintern 'www-utils:current-process :www-utils))
	  
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
		"PROCESS-WHOSTATE" "CURRENT-PROCESS" 
		)
  (:import-from #:excl #:if*)
  (:export "MAKE-PROCESS" "PROCESS-RUN-TIME" "PROCESS-IDLE-TIME" "CURRENT-PROCESS")
  (:shadowing-import-from "MP" "PROCESS-RUN-FUNCTION")
  (:import-from #:mp #:process-wait-with-timeout)
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
  
  
  ; (:shadowing-import-from "EXCL" "CHUNK-TRANSFER-ENCODING-MODE" "NOTE-FIRST-CHUNK" "NOTE-LAST-CHUNK")
  (:export "CHUNK-TRANSFER-ENCODING-MODE" "NOTE-FIRST-CHUNK" "NOTE-LAST-CHUNK")
  (:export "NETWORK-ERROR-MIXIN")
  (:export "TCP-SERVICE-PORT-NUMBER" "STREAM-TYI" "STREAM-TYO" "OPEN-MAILER-STREAM" "FIND-DIRECT-CONNECTION")
  (:export "PROCESS-PRIORITY")
)




(in-package :www-utils)


;; macros

(define-macro atomic-incf (reference &optional (delta 1))
  "Atomically increments REFERENCE by DELTA."
  `(minp:atomic-incf ,reference ,delta))

(define-macro atomic-decf (reference &optional (delta 1))
  "Atomically decrements REFERENCE by DELTA."
  `(minp:atomic-decf ,reference ,delta))

(define-macro atomic-push (item reference)
  "Atomically pushes ITEM onto REFERENCE."
  `(mp:without-scheduling
          (push ,item ,reference)))

(define-macro atomic-pop (reference)
  "Atomically pops an item off REFERENCE."
  `(mp:without-scheduling
	  (pop ,reference)))

(declaim (inline arglist))

(defun arglist (function)
  "Returns the arglist for FUNCTION."
  (declare (values (arglist values type arglist-types value-types)))
  (excl:arglist function))



(define-macro with-binary-stream ((stream direction) &body body)
  `(progn
     ,@body))

(define-macro with-text-stream ((stream direction) &body body)
  "Turns STREAM into a text stream within the scope of BODY.
direction can be :OUTPUT, :INPUT, or :BOTH."
  (declare (ignore stream direction))
  `(progn ,@body))

(define-macro with-optimal-stream-buffer (() . body)
   `(progn ,@body))

;;;------------------------------------------------------------------- 
;;;
;;; MAC AND LISPM FILES COMPATABILITY CODE ADAPTED FOR ALLEGRO CL
;;;



(locally (declare (optimize (speed 3) (safety 0)))
(let* ((N 256)
       (up (make-array N))
      (down (make-array N)))
  (dotimes (i N)
    (let ((c (code-char i)))
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



(declaim (inline string-thin))

;; removes fonts from Lispm Fat strings.-- JCMa 12/30/1994.
(defun string-thin (string)
  "Strips font description"
  string)




;;;------------------------------------------------------------------- 
;;;
;;; LOG RELATED PORTABILITY
;;;

;***** change over to acl locks

(defun make-lock (name &key type &allow-other-keys)
  "Returns a lock named name that is suitable for use with with-lock-held."
  (declare (ignore type ignore))
  (clim-sys:make-lock name))

(defmacro with-lock-held ((lock &optional (mode :write) (whostate "Wait for Lock")) &body body)
  "Executes BODY with LOCK held in MODE, which is one of :READ or :WRITE."
  (declare (ignore mode))
  `(clim-sys:with-lock-held (,lock ,whostate)
     ,@body))

;


;--------------

(defgeneric probe-directory (pathname)
  (:documentation "Returns non-null if the directory pathname exists."))

(defmethod probe-directory ((pathname pathname))
  (setq pathname (translate-logical-pathname pathname))
  (pathname-directory-p (make-pathname :host (pathname-host pathname)
				       :device (pathname-device pathname)
				       :directory (pathname-directory pathname))))

(defmethod probe-directory ((pathname string))
  (probe-directory (pathname pathname)))



(declaim (inline pathname-directory-p))

(defun pathname-directory-p (pathname)
  "Returns non-null if PATHNAME denotes a directory."
  (excl::file-directory-p pathname))
  



;--------


(define create-directories-recursively (pathname)
  "Recursively create directories according to the directories present in PATHNAME."
  (create-directory-recursively pathname))

;;; For implementations where pathname-directory does
;;; not return NIL when there is no directory in the pathname.
;;; -- OBC
(defun pathname-dirs (pathname)
  (let ((dirs (pathname-directory pathname)))
    (and (consp dirs) dirs)))


(defun %create-a-directory (path)
  (let ((str (namestring path))) ;;(directorystring path)
    (if (excl::make-directory path) path)))
  
  
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



(defun system (arg)
  (excl:shell arg))


;; needs to set up a timer or alarm on the MAC that will cause lisp to execute
;; run-daily-server-tasks  at the universal time returned by next-3am-universal-time
;; -- JCMa 12/31/1994.
(define synchronize-daily-server-tasks ()
   ;; set up a timer for mighnight to start the 24 interval timer
   (next-3am-universal-time))


(defvar *time-zone* 0)

(defun time-zone (&optional update-p)
  excl::*time-zone*
  )




;-----------

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


(defvar http::*shadow-host-domain-name* nil
  "Set this variable to a replacement for local-host-domain-name.
This name must be a valid name in the NIS directory and will be used
to shadow the name normally returned by local-context.")

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
        (t (setq http::*local-host-domain-name*
             (or http:*http-host-name* (%local-host-domain-name))))))



(defvar *domain-name-lookup* (make-hash-table :test #'eql))


(define domain-name-for-parsed-ip-address (ip-number &optional (no-error-p t))
  ;;
  ;; my guess: this is supposed to return the fully qualified domain
  ;;  name for a given ip address, if on exists, else it returns the
  ;;  dotted ipaddr in a string
  ;;
  (if* (and http::*shadow-host-domain-name* (eql ip-number (local-host)))
     then http::*shadow-host-domain-name*
   elseif (gethash ip-number *domain-name-lookup*)
     thenret
     else (let ((domain (default-domain-name))
		(name (if* http:*resolve-ip-addresses*
			 then (socket:ipaddr-to-hostname ip-number))))
	    (if* (null name)
	       then (socket:ipaddr-to-dotted ip-number)
	     elseif (not (find #\. name))
	       then ; some versions don't add the domain
		    (concatenate 'string name domain)
	       else name))))
	    
		    
(defun default-domain-name (&key (where 
				  #.(pathname "HTTP:acl;defaultdomain")))
  ipc::*domainname*)
	    
	    
	    
(define domain-name-for-ip-address (address &optional (no-error-p t))
  "Given the IP address, ADDRESS, this returns the domain name or NIL."
  (domain-name-for-parsed-ip-address (%parse-host-address address) no-error-p))		
		
	    
(define ip-address-for-host-domain-name (domain-name)
  "Returns the IP address string for domain-name."
  (ip-address-for-parsed-ip-address (%parse-host-address domain-name)))


(defun %parse-host-address (address)
  "Returns an IP-NUMBER which is integer denoting the address of host."
  ;; address is an ipaddr, or string holding a dns name for a
  ;; machine, or a string containing a dotted ip address.
  (declare (values ip-number))
  (etypecase address
    (integer address)
    (string (if* (or (and http::*shadow-host-domain-name*
			  (equal http::*shadow-host-domain-name*
				 address))
		     (equalp address "localhost"))
	       then (local-host)
	       else (if* http::*proxy-service*
		       then (http::ip-host-proxy-address address)
		       else (socket:lookup-hostname address))))))


(define ip-address-for-parsed-ip-address (ip-number)
  "Returns an IP address as a string from, IP-NUMBER, the parsed address."
  (socket:ipaddr-to-dotted ip-number))


;------


(defun %make-log-pathname (device directory name host)
  "Returns the pathname to which current log entries are written."
  (make-pathname
    :host host
    :device device
    :directory directory
    :name name
    :type "text"))




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



;=========

(defun directory-list (pathname &rest options)
  "Returns a lisp Machine style directory listing."
  (let ((pathnames (apply #'unix-directory-list* (merge-pathnames pathname "*.*") nil options)
                   ))
    (when (member :sorted options)
      (setq pathnames (sort pathnames
			    #'directory-name-type<
			    )))
			    
    (loop with length and creation-date
          for path in pathnames
          do (multiple-value-setq (length creation-date)
               (file-properties path))
          collect `(,path 
                    ,.(when length `(:length-in-bytes ,length))
                    ,.(when creation-date `(:creation-date ,creation-date))))))



(defun unix-directory-list* (pathname predicate &rest options)
  (multiple-value-bind (dirs error)
      (ignore-errors (directory pathname
				;; This fixes directory problem on UNIX for Allegro
				:directories-are-files  t))
    (when error
      (warn "Error reading directory ~s." pathname)
      (return-from unix-directory-list* nil))
    
    (if (not (member :directories options))
        (setq dirs (loop for file in dirs
                       unless (pathname-directory-p file)
                       collect file))
      (setq dirs (loop for file in dirs
                     when (pathname-directory-p file)
                     collect (unix-directory-pathname file)
                     else collect file)))
    (if predicate
        (setq dirs (loop for file in dirs
                       when (funcall predicate file)
                       collect file)))
    dirs))


(defun unix-directory-pathname (pathname)
  (let ((lastdir (pathname-name pathname)))
    (if lastdir
        (make-pathname :directory (append (pathname-directory pathname)
                                          (list lastdir))
		       :name nil
		       :type nil
		       :version nil
                       :defaults pathname))))

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
			     #'directory-name-type<)))
    (cond ((member :properties options)
           (loop for path in pathnames
                 collect  (multiple-value-bind (length creation-date)
                              (file-properties path) 
                            `(,path 
                              ,.(when length `(:length-in-bytes ,length))
                              ,.(when creation-date `(:creation-date ,creation-date))))))
          (t pathnames))))



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
  (if (host-eq host (local-host))
      (or http:*http-host-name*
          (host-mail-name host))
    (host-mail-name host)))

(declaim (inline %host-log-name))

(define %host-log-name (address host &optional resolve-ip-address-p)
  "Returns a string for use in logging server access."
  (declare (ignore host))
  (if resolve-ip-address-p
      (domain-name-for-parsed-ip-address address t)
      (ip-address-for-parsed-ip-address address))) 

;;; These need definitions in non CCL case!








(defmethod local-port ((http-stream ipc:tcp-client-stream))
   (socket:local-port http-stream))

(defmethod local-port ((http-stream ipc:tcp-server-stream))
   (socket:local-port http-stream))

(defmethod foreign-host ((http-stream ipc:tcp-client-stream))
   (socket:remote-host http-stream))

(defmethod foreign-port ((http-stream ipc:tcp-client-stream))
   (socket:remote-port http-stream))




;------




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

(defvar *hide-export-errors* nil)

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



(defun reset-http-server-location (&key (reload-exports (export-pathnames)) standard-port-change (force-reload #+ignore (> (http-image-date) 0)) (reset-location t))
  (declare (special http::*fast-remap-p*))
  ;; who needs this?? -jkf
  #+ignore (if reset-location
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






;;; HTTP Add-ons
;;;
(defpackage "HTTP"
  (:use)
  (:import-from "WWW-UTILS" "ADD-EXPORT-PATHNAME" "COMPILE-EXPORTS" "LOAD-EXPORTS" "LOGICAL-HOST-URL-LOCATION" "INSTALL-SYMBOLIC-HOST-SHADOW")
  (:export "ADD-EXPORT-PATHNAME" "COMPILE-EXPORTS" "LOAD-EXPORTS" "LOGICAL-HOST-URL-LOCATION" "INSTALL-SYMBOLIC-HOST-SHADOW"))




;------

(define expose-log-window ()
  "Exposes the Log window. Does nothing."
  nil)

(define notify-log-window (format-string &rest format-args)
  "Top-level method for writing to the HTTP log window."
  (let ((stream *trace-output*))
    (fresh-line stream)
    (write-char #\[ stream)
    (http::write-standard-time (get-universal-time) stream)
    (write-string "]  " stream)
    (apply #'format stream format-string format-args)))



;-------

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


;--------

(define live-connection-p (http-stream)
  "Returns non-null if the TCP/IP connection over HTTP-STREAM remains alive
in that the remote host continue to respond at the TCP/IP level."
   (open-stream-p http-stream))

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



;;;--------

(defun process-run-time (process)
  "Returns the amount of run time the process has accumulated, in microseconds."
  (* 1000
     (mp::process-cpu-msec-used process)) ;milsec

  )


(defun process-idle-time (process)
  "Returns the amount of time the process has been up, in seconds."
  (mp::process-start-secs process))


;;-------

(define report-string (condition)
  "Returns the report string for CONDITION."
  (with-output-to-string (stream)
    (report-condition condition stream)))

(define report-condition (condition stream)
  "Prints the report string for CONDITION onto STREAM."
  (handler-case (format stream "~A" condition)
    ;; Cannot guaranty all errors are printable.
    (error ()
      (describe condition stream))))

;--------

(defun char-bits (char)
  (declare (ignore char))
  0)


;-------


(deftype file-not-found () 
  "Specialization of Common Lisp File-error in which the file was not found on open."
  '(and condition file-error))


;----------

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
(defmethod file-length-in-bytes ((pathname pathname) &optional new-length)
   (declare (ignore new-length))
   (with-open-file (file-stream pathname)
       (file-length file-stream)))



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





(define file-properties (pathname)
  "Returns the length in bytes  and the creation in in universal time 
for FILE-STREAM's source file."
  (declare (values length-in-bytes creation-date version))
  (let* ((pathname (namestring pathname))
	(date (excl::filesys-write-date pathname)))
    (values (excl::filesys-size pathname)
	    date
	    date)))




;--------
(defmethod bytes-received ((stream t)) 0)
(defmethod (setf bytes-received) (val (stream t)) val)
(defmethod bytes-transmitted ((stream t)) 0)
(defmethod (setf bytes-transmitted) (val (stream t)) val)



;------------
; chunked support
;
(defmethod chunk-transfer-encoding-mode 
    ((stream socket::socket-stream-internet-active-bivalent)
     chunk-function)
  (declare (ignore chunk-function))
  nil)

(defmethod note-first-chunk 
    ((stream socket::socket-stream-internet-active-bivalent))
  (force-output stream)
  (socket:socket-control stream :output-chunking t))

(defmethod note-last-chunk 
    ((stream socket::socket-stream-internet-active-bivalent)
     &optional footers-plist)
  
  (socket:socket-control stream :output-chunking-eof t)
  (http::write-headers stream footers-plist t)
  (force-output stream))

  
  
;------------------------

(defun unix-directory-pathname (pathname)
  (let ((lastdir (pathname-name pathname)))
    (if* lastdir
       then (make-pathname :directory (append (pathname-directory pathname)
					      (list lastdir))
			   :name nil
			   :type nil
			   :version nil
			   :defaults pathname))))



(defparameter *services-table* (make-hash-table))

(defstruct protocol&port
  protocol
  port)

(defstruct service
  name
  protocol&ports)

(defun service-protocol-port (service &optional (protocol :tcp))
  (let ((service (gethash service *services-table*)))
    (if* service 
       then (dolist (protocol-port (service-protocol&ports service))
	      (if* (eq (protocol&port-protocol protocol-port) protocol)
		 then (return protocol-port))))))

;(defun build-services-table ()
;  ;; the format of the table is 
;  ;; servicename port/protocol alt-service-name1 alt-service-name2 ...
;  ;; where comments start with #
;  ;;
;  
;  (let ((*readtable* (copy-readtable)))
;    (set-syntax-from-char #\/ #\space)
;    (set-syntax-from-char #\# #\;)
;    (let ((fname #-mswindows (probe-file "/etc/services")
;		 #+mswindows
;		 (or 
;		  (probe-file  ; windows nt
;		   (concatenate 'string
;		     (sys:getenv "WINDIR")
;		     "\\system32\\drivers\\etc\\services"))
;		  (probe-file  ; windows 9x
;		   (concatenate 'string
;		     (sys:getenv "WINDIR")
;		     "\\services")))))
;      (flet ((insert-data (data)
;	       ;; data is (alt-name1 ... protocol port name)
;	       ;; put in the database
;	       (destructuring-bind (name port protocol &rest alt-names)
;		   (reverse data)
;		 (dolist (ins-name (cons name alt-names))
;		   (setq ins-name (intern (symbol-name ins-name) :keyword))
;		   (let ((service (gethash ins-name *services-table*)))
;		     (if* (null service)
;			then (setq service
;			       (make-service :name ins-name))
;			     (setf (gethash ins-name *services-table*) 
;			       service))
;			     
;		     (push 
;		      (make-protocol&port 
;		       :port port 
;		       :protocol (intern (symbol-name protocol) :keyword))
;		      (service-protocol&ports service)))))))
;		       
;	(if* fname
;	   then (with-open-file (in fname :direction :input)
;		  ;; since we don't see line boundaries the only way
;		  ;; we know that we've finished a line is when we see
;		  ;; a port number, in which case we know we're at 
;		  ;; the second field in the following line
;		  (let ((prev nil) (eof (cons nil nil)))
;		    (loop
;		      (let ((val (read in nil eof)))
;			(if* (eq eof val)
;			   then (if* prev then (insert-data prev))
;				(return)
;			 elseif (numberp val)
;			   then ; port number
;				(if* (cdr prev)
;				   then (insert-data (cdr prev)))
;				(setq prev (list val (car prev)))
;			   else (push val prev)))))))))))

;; new version from JKF on 11/02/99
(defun build-services-table ()
  ;; the format of the table is 
  ;; servicename port/protocol alt-service-name1 alt-service-name2 ...
  ;; where comments start with #
  ;;
  
  (let ((fname #-mswindows (probe-file "/etc/services")
	       #+mswindows
	       (or 
		 (probe-file			; windows nt
		   (concatenate 'string
				(sys:getenv "WINDIR")
				"\\system32\\drivers\\etc\\services"))
		 (probe-file			; windows 9x
		   (concatenate 'string
				(sys:getenv "WINDIR")
				"\\services")))))
    (flet ((insert-data (name port protocol alt-names)
	     ;; data is (alt-name1 ... protocol port name)
	     ;; put in the database
	     (if* (eq excl:*current-case-mode* :case-insensitive-upper)
		  then				; user may be working with punched cards.
						; must upcase to match
		  (setq name (string-upcase name)
			protocol (string-upcase protocol)))
	     (setq port (read-from-string port))
		     
	     (dolist (ins-name (cons name alt-names))
	       (setq ins-name (intern (string ins-name) :keyword))
	       (let ((service (gethash ins-name *services-table*)))
		 (if* (null service)
		      then (setq service
				 (make-service :name ins-name))
		      (setf (gethash ins-name *services-table*) 
			    service))
			     
		 (push 
		   (make-protocol&port 
		     :port port 
		     :protocol (intern  protocol :keyword))
		   (service-protocol&ports service))))))
		       
      (if* fname
	   then (with-open-file (in fname :direction :input)
		  (loop
		    (let ((line (read-line in nil nil)))
		      (if* (null line) then (return))	; eof
		      (let ((comment-start 
			      (or (position #\# line)
				  (length line))))
			(multiple-value-bind (ok whole name port protocol alt)
			    (excl:match-regexp "\\([^	 ]+\\)[ 	]+\\([0-9]+\\)/\\([a-z]+\\)\\(.*\\)" line 
					       :end comment-start
					       :case-fold t
					       )
			  (declare (ignore whole))
			  (if* ok
			       then		; parse alt names
			       (let (alts (pos 0) aname)
				 (loop (multiple-value-setq (aname pos)
					 (read-from-string alt
							   nil nil
							   :start pos))
				       (if* aname 
					    then (push aname alts)
					    else (return))
				  
				       (insert-data name port
						    protocol
						    alts)))))))))))))	
		  
		  

(defun tcp-service-port-number (srvstr)
  (let ((ans (service-protocol-port 
	      (intern (symbol-name srvstr) :keyword) :tcp)))
    (if* ans
       then (protocol&port-port  ans))))

(build-services-table)

