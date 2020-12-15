;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: IPC; Base: 10 -*-

(in-package "IPC")

(defpackage "IPC"
  (:use) (:export "FRANZ-HOSTNAME" "GETDOMAINNAME" "INTERNET-ADDRESS" "INTERNET-ADDRESS-DOMAIN" "PROTOCOL-ERROR" "TCP-CLIENT-ALIVE" "TCP-CLIENT-STREAM" "TCP-SERVER-STREAM" "STREAM-READ"))

;;; In part copied and adapted from IPC.CL - Olivier (OBC) 6/95
;;
;; Allegro CL IPC interface
;;
;; copyright (c) 1986-1994 Franz Inc, Berkeley, CA
;; All rights reserved.
;;
;; Permission is granted only to any individual or institution which has
;; current Allegro CL license(s) to use, copy, or modify this software,
;; provided any reproduction or distribution of binary versions of this
;; software are compiled with a licensed Allegro CL, and provided
;; that this complete copyright and permission notice is maintained, intact,
;; in all copies and supporting documentation. 
;;
;; Franz Incorporated provides this software "as is" without
;; express or implied warranty.
;;
;; Restricted Rights Legend
;; ------------------------
;; Use, duplication, and disclosure of the software, data and information
;; contained herein by any agency, department or entity of the U.S.
;; Government are subject to restrictions of Restricted Rights for
;; Commercial Software developed at private expense as specified in FAR
;; 52.227-19 or DOD FAR Supplement 252 52.227-7013 (c) (1) (ii), as
;; applicable.
;; This code is provided to licensed users of Allegro CL as an example 
;; and for restricted reuse.
;;;

(eval-when (load eval)
  (load "" :unreferenced-lib-names
	(mapcar #'ff:convert-to-lang
		'("inet_ntoa"
		  "inet_addr"
		  "gethostbyaddr"))))

(ff:defforeign 'inet-ntoa
    :arguments '(integer)
    ;;:arguments '(fixnum)
    ;; From: "Scott L. Burson" <gyro@zeta-soft.com>
    ;; When the CL-HTTP (unix) process is already that large before CL-HTTP
    ;; is started (512MB, I believe), FF:MALLOC-CSTRUCT begins to return
    ;; bignums, which caused INET-NTOA to barf with a message like
    ;; Error: expected arg of type FIXNUM, got type BIGNUM for 1036871224
    :entry-point (ff:convert-to-lang "inet_ntoa")
    :call-direct t
    :callback nil
    :return-type :integer)

(ff:defforeign 'inet-addr
    :arguments '(simple-string)
    :entry-point (ff:convert-to-lang "inet_addr")
    :call-direct t
    :callback nil
    :return-type :integer)

;;; Turn internet address into string format
#+(and (or :SUN4 :SPARC :SUN) (not Linux86))
(defmethod ip-address-string ((address integer))
  (let ((in-addr (ff:malloc-cstruct #+(version>= 4 3) 'unsigned-int #-(version>= 4 3) 'unsigned-long))
	addr-str)
    (setf (#+(version>= 4 3) unsigned-int-unsigned-int
	     #-(version>= 4 3) unsigned-long-unsigned-long in-addr) address)
    (setf addr-str (ff:char*-to-string (inet-ntoa in-addr)))
    (ff:free-cstruct in-addr)
    addr-str))

#-(or :Linux86 :SUN4 :SPARC :SUN) ;;IRIS4D :SGI4D :SGI
(defmethod ip-address-string ((address integer))
  (format nil "~D.~D.~D.~D" ;or? "~11,1,1,'.<~2,'0D~;~2,'0D~;~2,'0D~;~2,'0D~>"
          (ldb (byte 8 24) address)
          (ldb (byte 8 16) address)
          (ldb (byte 8  8) address)
          (ldb (byte 8  0) address)))

;;; A method here would work if you had the right patches loaded - obc
#+Linux86
(defun ip-address-string (address)
  (format nil "~D.~D.~D.~D"
          (ldb (byte 8  0) address)
          (ldb (byte 8  8) address)
          (ldb (byte 8 16) address)
          (ldb (byte 8 24) address)))

(defvar *static-sstring* (make-string 256 :initial-element #\ ))

(defmethod internet-address ((name string))
  (or (internet-address-domain name)
      ;; Fall-back
      (let ((end (position #\. name)))
	(if (if end (> end 0))
	    ;; Use short name, host may be local and its name server down
	    (internet-address-domain (subseq name 0 end))
	  0))))

#+(version>= 4 3)
(progn
  (setf (fdefinition 'gethostbyname) (fdefinition 'excl::gethostbyname))
  (setf (fdefinition 'getuid) (fdefinition 'excl::getuid))
  (setf (fdefinition 'getservbyname) (fdefinition 'excl::getservbyname)))

;;; Like gethostbyname but returns an internet address (long network format)
(defmethod internet-address-domain ((name string))
  (without-scheduling
    (typecase name
      (simple-string 
       name)
      (t
       (setf (subseq *static-sstring* 0) name
	     (elt *static-sstring* (length name)) #.(character 0)
	     name *static-sstring*)))
    (let ((hpoint (gethostbyname name)))
      (if (plusp hpoint)
	  #+(version>= 4 3)
	  (unsigned-int-unsigned-int (unsigned-int-unsigned-int (ipc::hostent-addr hpoint)))
	  #-(version>= 4 3)
	  (unsigned-long-unsigned-long (unsigned-long-unsigned-long (ipc::hostent-addr hpoint)))))))

(ff:defforeign 'gethostname
    :entry-point (ff:convert-to-lang "gethostname")
    :arguments '(simple-string fixnum)
    :call-direct t
    :return-type :integer)

(ff:defforeign 'gethostbyaddr
    :entry-point (ff:convert-to-lang "gethostbyaddr")
    :arguments '(simple-string fixnum fixnum)
    :call-direct t
    :return-type :integer)

(ff:defcstruct (cstring :malloc :no-constructor)
  1 :unsigned-char)

(defvar *address-string* (make-string 4 :initial-element #\ ))

;;; For side effect only
#-Linux86
(defun long-address-string (address)
  (declare (type (simple-string 4) *address-string*))
  (setf (elt *address-string* 0) (code-char (ldb '#.(byte 8 24) address))
	(elt *address-string* 1) (code-char (ldb '#.(byte 8 16) address))
	(elt *address-string* 2) (code-char (ldb '#.(byte 8 8) address))
	(elt *address-string* 3) (code-char (ldb '#.(byte 8 0) address)))
  (values))

#+Linux86
(defun long-address-string (address)
  (declare (type (simple-string 4) *address-string*))
  (setf (elt *address-string* 3) (code-char (ldb '#.(byte 8 24) address))
	(elt *address-string* 2) (code-char (ldb '#.(byte 8 16) address))
	(elt *address-string* 1) (code-char (ldb '#.(byte 8 8) address))
	(elt *address-string* 0) (code-char (ldb '#.(byte 8 0) address)))
  (values))

(define-condition unknown-host-name (simple-error)
  ((address :initform 0 :initarg :address))
  (:default-initargs :format-control "host name not found for address ~D."))
    
(defmethod initialize-instance :after ((error unknown-host-name) &key format-arguments address &allow-other-keys)
  (if (and address (null format-arguments))
      (reinitialize-instance error :format-arguments (list address))))
 
(defun get-host-by-address (address)
  (without-scheduling
    (long-address-string address)
    (let ((host (gethostbyaddr *address-string* 4 *af-inet*)))
      (if (zerop host)
	  (error 'unknown-host-name :address address)
	host))))

(defun ctolisp-name (pointer)
  (declare (type (simple-string *) *static-sstring*))
  (without-scheduling
    (loop for i upfrom 0
	as code = (cstring pointer i)
	while (plusp code)
	do (setf (elt *static-sstring* i) (code-char code))
	finally (return (subseq *static-sstring* 0 i)))))

(defun get-host-name-by-address (address)
  (let ((cname (hostent-name (get-host-by-address address))))
    (ctolisp-name cname)))

;;; This allocates a string for hostname
(defun franz-hostname ()
  (without-scheduling
    (let ((string *static-sstring*))
      (when (zerop (gethostname string 256))
	(let ((length (position '#.(character 0) string)))
	  (if length
	      (subseq string 0 length)))))))

(defvar *domainname* nil)

;;; There are more than one strategies you can use
;;; based on what OS you are using. Instead of providing
;;; each one we try /etc/defaultdomain and let you override it using
;;; the DOMAINNAME shell variable.
;;;
(defun getdomainname (&optional (where #p"/etc/defaultdomain"))
  (or (etypecase where
	(string
	 (setq *domainname* where))
	(null
	 (setq *domainname* nil)
	 (return-from getdomainname nil))
	(pathname
	 *domainname*))
      ;; Shell initialization variable
      (setq *domainname* (sys:getenv "DOMAINNAME"))
      (if (probe-file where)
	  (with-open-file (stream where :direction :input)
	    (setq *domainname* (read-line stream))))))

;;; Client
;;;
(defclass meta-tcp-client (standard-class) ())

(defmethod clos:validate-superclass ((class meta-tcp-client) (superclass standard-class))
  t)

;;; Now this class can be confusing,
;;; it seems to be a character stream, however it supports
;;; multivalent read-byte and write-byte as well for HTTP convenience
;;;
(defclass tcp-client-stream (excl::bidirectional-multivalent-8-bit-stream)
  #+ignore (stream:bidirectional-terminal-stream) ;Previously only character io
  ((host :initarg :host)
   (server-port :initarg :server-port)
   (port :initarg :port)
   (host-address :initarg :host-address)
   (listen-sockaddr :initform nil :initarg :listen-sockaddr)
   (process :initform nil :accessor tcp-stream-process)
   (alive :initform t :accessor tcp-client-alive))
  (:metaclass meta-tcp-client))

(defmethod tcp-client-alive ((stream t))
  nil)

(defmethod (setf tcp-client-alive) (value (stream t))
  value)

;;; Fix 3 bugs on stream closed locally or remotely - OBC
;;;
(excl:without-package-locks
(defmethod open-stream-p :around ((stream excl::bidirectional-multivalent-8-bit-stream))
   (without-scheduling
     (and (call-next-method)
	  (if (stream:stream-listen stream)
	      (if (handler-case (peek-char nil stream nil nil)
		    (error ()
		      (close stream :abort t)
		      nil))
		  t)
	    t))))
)

(defmethod stream:stream-listen :around ((stream tcp-client-stream))
  (with-slots (excl::fn-in excl::fn-out) stream
    (and excl::fn-in excl::fn-out (call-next-method))))

(defmethod close :around ((stream tcp-client-stream) &key (abort t))
  ;; When remote end aborts stream while we are transfering
  ;; closing the stream can cause a Signal 13 on our end.
  (setf (tcp-client-alive stream) nil)
  (if abort
      (close-unix-streams stream :abort abort) ; Really for proxy streams
    (handler-case (progn #-(version>= 4 3)
			 (close-unix-streams stream :abort abort)
			 (call-next-method))
      ((or synchronous-operating-system-signal file-error type-error) ()
	;; Try again these really won't close!
	(close-unix-streams stream :abort abort))))
  (with-slots ((lsa listen-sockaddr)) stream
    (when lsa
      (free-listen-sockaddr lsa)
      (setq lsa nil))))

(defun tcp-destroy-process (stream)
  (let ((process (tcp-stream-process stream)))
    ;; If closing multiple times
    (if process
	(process-kill process)))
  (setf (tcp-stream-process stream) nil))

;;; Desperate closing times. Be nice and finish output to client.
;;;
(defun close-unix-streams (stream &key abort)
  (close-unix-streams-before stream :abort abort)
  (close-unix-streams-after stream :abort abort))

(declaim (inline stream-unwatchfor-input))

(defun stream-unwatchfor-input (stream)
  (with-slots ((fn-in excl::fn-in)) stream
    (and fn-in
	 #-(version>= 4 3) (mp::mpunwatchfor fn-in))))

(declaim (inline stream-watchfor-input))

(defun stream-watchfor-input (stream)
  (with-slots ((fn-in excl::fn-in)) stream
    (and fn-in
	 #-(version>= 4 3)
	 (mp::mpwatchfor fn-in))))

(defun close-unix-streams-before (stream &key abort)
  (with-slots ((fn-in excl::fn-in) (fn-out excl::fn-out)) stream
    (cond ((and fn-in (eql fn-in fn-out))
	   (or abort
	       (ignore-errors (finish-output stream)))
	   #-(version>= 4 3) (mp::mpunwatchfor fn-in))
	  (fn-in
	   #-(version>= 4 3) (mp::mpunwatchfor fn-in))
	  (fn-out
	   (or abort
	       (ignore-errors (finish-output stream)))
	   #-(version>= 4 3) (mp::mpunwatchfor fn-out)))))

(defun close-unix-streams-after (stream &key abort)
  (with-slots ((fn-in excl::fn-in) (fn-out excl::fn-out)) stream
    (cond ((and fn-in (eql fn-in fn-out))
	   (unix-close fn-in))
	  (fn-in
	   (unix-close fn-in))
	  (fn-out
	   (unix-close fn-out)))
    (process-allow-schedule)
    (when abort
      (setf fn-in nil fn-out nil))))

(defvar *require-addr-indirection* nil)

(declaim (inline target-addr-indirection))

(defun target-addr-indirection ()
  (cond (*require-addr-indirection*
	 (eql *require-addr-indirection* t))
	(t (cond ((or (member #+(version>= 4 3) sys::.target.
			      #-(version>= 4 3) comp::.target.
			      ;; bug3164
			      '(:hp :hpprism :sgi4d :sony :dec3100 :rs6000 :next)
			      :test #'eq)
		      (probe-file "/lib/ld.so"))
		  (setq *require-addr-indirection* t))
		 (t
		  (setq *require-addr-indirection* :none)
		  nil)))))

(declaim (inline close-before-error))

(defun close-before-error (sock)
  (when sock
    #-(version>= 4 3) (mp::mpunwatchfor sock)
    (unix-close sock)
    (process-allow-schedule)))

(declaim (inline make-unix-ipc-terminal-stream))

(defun make-unix-ipc-terminal-stream (socket-file)
  (let ((server (make-cstruct 'sockaddr-un))
	socket-fd)
    (setf (sockaddr-un-family server) *af-unix*)
    (dotimes (i (length socket-file)
	       (setf (sockaddr-un-path server i) 0))
      (setf (sockaddr-un-path server i)
	(char-int (elt socket-file i))))
    (setq socket-fd (socket *af-unix* *sock-stream* 0))
    (if #+(version>= 4 3) (not (eq t (excl::filesys-connect socket-fd server (+ 2 (length socket-file)))))
	#-(version>= 4 3) (< (connect socket-fd server (+ 2 (length socket-file))) 0)
	(error "Connect failed to ~s" socket-file))
    (make-ipc-terminal-stream socket-fd)))

(declaim (inline make-internet-tcp-client-stream))

(defun make-internet-tcp-client-stream (host port)
  (let (sock server hostaddress)
    ;; Open a socket
    (when (< (without-scheduling
	       (setf sock (socket *af-inet* *sock-stream* 0)))
	     0)
      (error "Couldn't open socket"))
    ;; construct a socket address
    (setf server (make-cstruct 'sockaddr-in))
    #+(version>= 4 3)
    (excl::memset server 0 (ff::cstruct-length 'sockaddr-in))
    #-(version>= 4 3)
    (bzero server (ff::cstruct-length 'sockaddr-in))
    (if* (integerp host)
       then (setq hostaddress (lisp_htonl host))
     elseif (stringp host)
       then (when (= (setq hostaddress (gethostbyname host)) 0)
	      (close-before-error sock)
	      (error "Unknown host: ~a" host))
	    (when (not (= 4 (hostent-length hostaddress)))
	      (close-before-error sock)
	      (error "Address length for ~a not 4" (ip-address-string hostaddress)))
	    (setq hostaddress
	      (let ((addr (hostent-addr hostaddress)))
		(si:memref-int
		 ;; SunOS 4.0 requires an extra indirection
		 (if (target-addr-indirection)
		     (si:memref-int addr 0 0 :unsigned-long)
		   addr)
		 0 0 :unsigned-long)))
       else (close-before-error sock)
	    (error "HOST ~s is not a string or integer internet address." host))
    (setf (sockaddr-in-addr server) hostaddress)

    (setf (sockaddr-in-port server)
      (if* (integerp port)
	 then (lisp_htons port)
       elseif (stringp port)
	 then (let ((serv (getservbyname port "tcp")))
		(if* (= 0 serv)
		   then (close-before-error sock)
			(error "Unknown service name: ~s" port))
		(servent-port serv))
	 else (close-before-error sock)
	      (error "PORT ~s not a string or integer internet service." port)))

    (setf (sockaddr-in-family server) *af-inet*)
    ;; open the connection
    (when #+(version>= 4 3)
	  (let ((len (ff::cstruct-length 'sockaddr-in))
			(ofd (- -1 sock)))
		    ;; Fix me -- mp hazard for errno.
		    (mp:waiting-for-input-available ;actually wait for output
		     (ofd)
		     (loop as ret = (excl::filesys-connect sock server len)
			 when (or (eq ret t)
				  (eq ret excl::*error-code-eisconn*))
			 return nil
			 while (or (eq ret excl::*error-code-einprogress*)
				   (eq ret excl::*error-code-ealready*))
			 do (process-wait "waiting for connect to complete"
					  #'excl::filesys-fn-will-not-block-p
					  ofd)
			 finally (return t))))
	  #-(version>= 4 3)
	  (< (without-scheduling ;Avoid interrupts while connecting
	       (connect sock server (ff::cstruct-length 'sockaddr-in))) 0)
      (perror "Connect failure")
      (princ "When connecting to: ") (princ (ip-address-string hostaddress)) (princ " on port: ") (princ port)
      (close-before-error sock)
      (error "Cannot connect to ~a on socket ~a." (ip-address-string hostaddress) sock))
    #+debug (add-client-binding sock)
    sock))

(defmethod make-instance :around ((class meta-tcp-client) &rest args &key host port socket-file fn-in fn-out (watchfor t) &allow-other-keys)
  "Instantiate a open stream to a port, which is a TCP/IP communication channel.  There
are two types of ports supported, UNIX and INTERNET domain.  The domain is
chosen based on the keyword arguments supplied:
For internet domain:
 HOST is the string host name or an integer internet address.
 PORT is the string service name or a port number.
For Unix domain:
 SOCKET-FILE is the string pathname of the socket.
If FN-IN or FN-OUT are provided. Attempt to open the stream directly."
  (remf args :socket-file)
  (remf args watchfor)
  (if (and fn-in fn-out) ;AND is essential, OR silently prevent proper open!
      (call-next-method)
    (progn
      (when (or (and host port socket-file)
		(and (or (null host) (null port))
		     (null socket-file)))
	(error "Must either supply HOST and PORT *or* SOCKET-FILE keywords."))
      (if* socket-file
	 then ;; UNIX domain
	      (make-unix-ipc-terminal-stream socket-file)
	 else ;; INTERNET domain
	      (let ((sock (make-internet-tcp-client-stream host port))
		    stream)
		(unwind-protect (without-scheduling
				  (setq stream (apply #'call-next-method class :fn-in sock :fn-out sock args)))
		  (cond (stream
			 (if watchfor ;Not needed for server issued streams
			     #+(version>= 4 3)
			     nil
			     #-(version>= 4 3)
			     (mp::mpwatchfor sock))
			 #+debug
			 (format t "~&Socket open ~a.~%" sock))
			(t
			 (close-before-error sock)
			 (error "Cannot instantiate tcp client stream (fd = ~a)." sock)))))))))

(defvar *tcp-client-bindings* nil)

(defun add-client-binding (fd)
  (push fd *tcp-client-bindings*))

(defun clean-client-bindings ()
  (dolist (fd *tcp-client-bindings*)
    (cond ((zerop (unix-close (incf fd)))
	   #-(version>= 4 3) (mp::mpunwatchfor fd)
	   (format *trace-output* "~&Closed unix FD ~D." fd))
	  (t
	   (return fd)))))

;;; Server
;;;
(defclass meta-tcp-server (standard-class) ())

(defmethod clos:validate-superclass ((class meta-tcp-server) (superclass standard-class))
  t)

(defclass tcp-server-stream (stream:input-terminal-stream)
  ((host :initarg :host) ;This is the local host
   (port :initarg :port)
   (listen-socket-fd :initarg :fn-in)
   (listen-sockaddr :initarg :listen-sockaddr)
   (client-streams :initform nil)
   (process :initform :initializing :accessor tcp-stream-process))
  (:metaclass meta-tcp-server))

(defvar *tcp-server-terminate* t)

(defmethod close :around ((stream tcp-server-stream) &key (abort t))
  (with-slots (process) stream
    (setq process nil))
  (process-allow-schedule)
  ;; When remote end aborts stream while we are transfering
  ;; closing the stream can cause a Signal 13 on our end.
  (handler-case (call-next-method)
    ((or synchronous-operating-system-signal file-error type-error) ()
      (close-unix-streams stream :abort abort)))
  (with-slots (client-streams) stream
    (mapc #'(lambda (fdlsa)
	      (free-listen-sockaddr (rest fdlsa)))
	  client-streams)))

(defmethod stream:stream-listen ((stream tcp-server-stream))
  (with-slots (process client-streams) stream
    (and process client-streams t)))

(defmethod stream:stream-read-char ((stream tcp-server-stream))
  (error "Use IPC::STREAM-READ to read new TCP connection streams from ~a." stream))

(defmethod stream-read ((stream tcp-server-stream))
  (with-slots (client-streams) stream
    (destructuring-bind (fd . listen-sockaddr)
	(without-scheduling (pop client-streams))
      (when fd
	(if (symbolp fd) ;Handle future protocol errors
	    (error fd :stream stream))
	(let ((host-address (sockaddr-in-addr listen-sockaddr))
	      host
	      (port (sockaddr-in-port listen-sockaddr)))
	  (setq host
	    #-Debug host-address
	    #+Debug (handler-case (get-host-name-by-address host-address)
		      (unknown-host-name () host-address)))
	  ;; Open "" can fail here?
	  (make-instance 'tcp-client-stream :fn-in fd :fn-out fd
			 :host host :server-port (slot-value stream 'port)
			 :port port :host-address host-address
			 :listen-sockaddr listen-sockaddr
			 :watchfor nil))))))

;;; Copied and adapted from IPC.CL

(defvar *tcp-port-min* 8000)
(defvar *tcp-port-max* 9000)

(defun make-listen-sockaddr (&optional foreign)
  #+(version>= 4 3) (declare (ignore foreign))
  (if *unix-domain*
      (make-cstruct 'sockaddr-un)
    (let ((sin (make-cstruct 'sockaddr-in #-(version>= 4 3) :in-foreign-space  #-(version>= 4 3) foreign)))
      #+(version>= 4 3)
      (excl::memset sin 0 (ff::cstruct-length 'sockaddr-in))
      #-(version>= 4 3)
      (bzero sin (ff::cstruct-length 'sockaddr-in))
      sin)))

(defun free-listen-sockaddr (sockaddr)
  (unless *unix-domain*
    (if sockaddr
	#+(version>= 4 3)
	sockaddr
	#-(version>= 4 3)
	(ff:free-cstruct sockaddr))))

(defmethod make-instance :around ((class meta-tcp-server)
				  &key
				  ((:unix-domain *unix-domain*) *unix-domain*)
				  ((:port tcp-port) 0 tcp-port-p)
				  (host (franz-hostname))
				  ((:tcp-port-min *tcp-port-min*) 
				   (if tcp-port-p tcp-port *tcp-port-min*))
				  ((:tcp-port-max *tcp-port-max*) 
				   (if tcp-port-p tcp-port *tcp-port-max*))
				  ((:socket-pathname *socket-pathname*) 
				   *socket-pathname*)
				  (own-process-p t)
				  listen-fd
				  ;; We should decide this if we
				  ;; implement background streams 
				  ;; then it is no good just outputting to random locations.
				  &allow-other-keys
				  &aux 
				  (*standard-output* *initial-terminal-io*)
				  (*debug-io* *initial-terminal-io*)
				  (*terminal-io* *initial-terminal-io*)
				  (*error-output* *initial-terminal-io*))
  (block good-news
  (block bad-news
    (let (listen-socket-fd
	  (listen-sockaddr (make-listen-sockaddr))
	  stream
	  tcp-port-used)
    
      (unless *socket-pathname*
	(setq *socket-pathname* (format nil "/tmp/AclTcpSrvr~d" (getuid))))

      ;; We are loosing FDs. This should not be necessary... But it is!
      ;; An Allegro bug is suspected the FD does not get closed right
      ;; no matter how you do it. You can trace unix-close and see it.
      (clean-tcp-port-binding tcp-port)

      (unwind-protect
	  (progn
	    (if *unix-domain* (errorset (delete-file *socket-pathname*)))
	    (if listen-fd
		;; bind80 case
		(setq listen-socket-fd listen-fd)
	      (setq listen-socket-fd (socket
				      (if *unix-domain* *af-unix* *af-inet*)
				      *sock-stream*
				      0)))
	    (when (< listen-socket-fd 0)
	      (perror "Socket failure")
	      (setq listen-socket-fd nil)
	      (return-from bad-news))

	    (#+(version>= 4 3) mp:waiting-for-input-available #+(version>= 4 3) (listen-socket-fd)
	       #-(version>= 4 3)  progn
	       #-(version>= 4 3) (mp::mpwatchfor listen-socket-fd)
	    (cond (*unix-domain*
		   (setf (sockaddr-un-family listen-sockaddr) *af-unix*)
		   ;; Set pathname.
		   (dotimes (i (length *socket-pathname*)
			      (setf (sockaddr-un-path listen-sockaddr i) 0))
		     (setf (sockaddr-un-path listen-sockaddr i)
		       (char-int (elt *socket-pathname* i))))
		   (unless (zerop (bind listen-socket-fd
					listen-sockaddr
					(+ (length *socket-pathname*) 2)))
		     (perror "Bind failure")
		     (return-from bad-news)))
		  (t
		   (setf (sockaddr-in-family listen-sockaddr) *af-inet*)
		   (setq tcp-port-used *tcp-port-min*)
		   (loop
		     (setf (sockaddr-in-port listen-sockaddr)
		       (lisp_htons tcp-port-used))
		     ;; bind80 case
		     (if listen-fd
			 (return))
		     (if (zerop (bind listen-socket-fd
				      listen-sockaddr
				      (ff::cstruct-length 'sockaddr-in)))
			 (return))
		     (cond ((= tcp-port-used *tcp-port-max*)
			    (perror "Bind failure")
			    (return-from bad-news))
			   (t
			    (incf tcp-port-used))))))

	    (finish-output)
	    (unless (zerop (unix-listen listen-socket-fd 5))
	      (perror "Listen failure")
	      (return-from bad-news))
			 
	    (finish-output)
	    
	    (setq stream (call-next-method class
					   :host host
					   :port tcp-port-used
					   :fn-in listen-socket-fd
					   :listen-sockaddr listen-sockaddr)))
	    
	    (return-from good-news
	      (if own-process-p
		  (initialize-tcp-server-process stream)
		stream)))
  
	(cond (stream
	       (add-tcp-port-binding tcp-port-used listen-socket-fd))
	      (t
	       (close-before-error listen-socket-fd))))))
  (error "couldn't start TCP server daemon")))

;;; Treating this type of error is a new thing
;;; only occuring when using CL-HTTP as a relay
;;; to other proxies.
;;;
(defun handle-open-file-error (error &optional (fromfd 22) (tofd 60))
  (if (find "Too many open files" (condition-format-arguments error)
	    :test #'equal)
      (loop for fd from fromfd upto tofd
	  do #-(version>= 4 3) (mp::mpunwatchfor fd)
	     (ipc::unix-close fd)
	  finally (return t))))
  
;;; For recovery only - BSD.
;;; There still exist race conditions on closing socket based
;;; streams at the lowest level. I don't have access to the
;;; lower level so this will have to do - OBC.
;;;
(defvar *tcp-port-bindings* nil)

(defun add-tcp-port-binding (tcp-port fd)
  (setf (getf *tcp-port-bindings* tcp-port) fd))

(defun clean-tcp-port-binding (tcp-port)
  ;; Unix prevention against hard to close sockets
  (let ((fd (getf *tcp-port-bindings* tcp-port)))
    (when (and fd (> fd 0))
      #-(version>= 4 3) (mp::mpunwatchfor fd)
      (when (zerop (unix-close fd))
      	(format *trace-output* "~&Closed unix FD ~D.~%" fd)
	(force-output *trace-output*)
	(process-allow-schedule)))))

(defmethod initialize-tcp-server-process ((stream tcp-server-stream))
  (let ((server-name (format nil "TCP Server [~a]" (slot-value stream 'port)))
	proc)
    (setq proc (process-run-function server-name
				     #'tcp-server-daemon-function
				     stream))
    (setf (tcp-stream-process stream) proc)
    ;(setf (mp::process-interruptable-p proc) nil)
    ;(setf (getf (process-property-list proc) ':survive-dumplisp) 't)
    stream))

;;; Define useful errors
;;;
(define-condition protocol-error (error)
  ((stream :initform nil :initarg :stream)))

(defmethod tcp-server-daemon-function ((stream tcp-server-stream))
  (let (fd
	(timeout (make-cstruct 'timeval))
	(mask-obj (make-cstruct #+(version>= 4 3) 'unsigned-int #-(version>= 4 3) 'unsigned-long))
	mask
	(int (make-cstruct #+(version>= 4 3) 'unsigned-int #-(version>= 4 3) 'unsigned-long))
	wait-function
	listen-sockaddr)
    (with-slots (listen-socket-fd process) stream
      (setf (timeval-sec timeout) 0
	    (timeval-usec timeout) 0)
      ;; Compute a select mask for the daemon's socket.
      (setq mask (ash 1 listen-socket-fd))
      (setq wait-function
	#'(lambda ()
	    (setf (#+(version>= 4 3) unsigned-int-unsigned-int
		     #-(version>= 4 3) unsigned-long-unsigned-long
		     mask-obj)
	      mask)
	    (or (not process)
		(not (zerop (select 32 mask-obj 0 0 timeout))))))
      ;; Required
      #-(version>= 4 3)
      (mp::mpwatchfor listen-socket-fd)
      ;; Minimize what's done here
      (unwind-protect
	  (block nil
	    (loop
	      (process-wait "waiting for a connection"
			    wait-function)
	      (cond (process
		     (setf (#+(version>= 4 3) unsigned-int-unsigned-int
			      #-(version>= 4 3) unsigned-long-unsigned-long
			      int)
		       (if *unix-domain*
			   (ff::cstruct-length 'sockaddr-un)
			 (ff::cstruct-length 'sockaddr-in)))
		     (setq listen-sockaddr (make-listen-sockaddr t))
		     (without-scheduling
		       (setq fd (accept listen-socket-fd listen-sockaddr int))))
		    (t
		     ;; Terminate
		     (return)))
	      (finish-output *standard-output*)
	      (finish-output *error-output*)
	      (cond ((< fd 0)
		     (free-listen-sockaddr listen-sockaddr)
		     (setq listen-sockaddr nil)
		     (if process
			 (perror "Accept failure"))
		     (finish-output *standard-output*)
		     (finish-output *error-output*)
		     ;; Revive?
		     #-(version>= 4 3) (mp::mpwatchfor listen-socket-fd)
		     (without-scheduling
			 (push (cons 'protocol-error nil)
			       (slot-value stream 'client-streams))))
		    (t (without-scheduling
			 (push (cons fd listen-sockaddr)
			       (slot-value stream 'client-streams)))
		       (setq listen-sockaddr nil)))))
	(close-before-error listen-socket-fd)
	(free-listen-sockaddr listen-sockaddr)))))

#|| Example:
<HOST A>
IPC(6): (setq x (make-instance 'tcp-server-stream :host "tenet" :port 8000))
#<TCP-SERVER-STREAM @ #x116d30a>

<CLIENT B>
IPC(2): (setq x1 (make-instance 'tcp-client-stream :host "tenet" :port 8000))
#<TCP-CLIENT-STREAM @ #x12482fa>

<HOST A>
IPC(8): (listen x)
T
IPC(9): (setq x1 (stream-read x))
#<TCP-CLIENT-STREAM @ #x120ed3a>
IPC(11): (print 'hello1 x1)
HELLO1
IPC(12): (finish-output x1)
NIL

<CLIENT B>
IPC(3): (read x1)
HELLO1
||#
