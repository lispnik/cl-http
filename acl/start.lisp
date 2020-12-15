;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

(in-package "CL-USER")

#+(and Allegro (version>= 5 0))
(push :ACL5 *features*)

;;; lispm major.minor ACL major.minor
(setq *cl-http-server-version* '(70 23 3 11 6))

#+ACLPC
(defvar *command-line-arguments* nil)

#+ACLPC
(defparameter *http-command-line*
    (cond ((find-package "HTTP")
	   (setq *command-line-arguments* nil)
	   " port=80 host= compile=nil w3p=t w4= w4demo= proxy=")
	  (t
	   " port=80 host= fastremap=t w3p=t w4= w4demo= proxy=")))

#+ACLPC
(defun command-line-arguments (&optional reset)
  (or (and (null reset) *command-line-arguments*)
      (setq *command-line-arguments*
	(loop with comline = (concatenate 'string *command-line* *http-command-line*)
	    with spaces = '(#\Space #\Tab)
	    for start = 0 then (incf end)
	    as end = (position-if #'(lambda (c) (member c spaces)) comline :start start)
	    as arg = (string-trim spaces (subseq comline start (or end (length comline))))
	    unless (equal arg "")
	    collect arg
	    while end))))

;;; From acl;unix
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
		    (ignore-errors (read-from-string value))
		  value)
		match)))))

;;; Main HTTP source directory
;;;
#+(and Allegro (not ACL5))
(defvar *http-directory*
    #+(version>= 4 3) (let ((home (get-line-argument "home=" 'string)))
			(if home
			    (pathname home)
			  (excl:current-directory)))
    #-(version>= 4 3) *default-pathname-defaults*)

#+(or ACLPC ACL5)
(defvar *http-directory*
    (make-pathname :host (pathname-host *load-pathname*)
		   :device (pathname-device *load-pathname*)
		   :directory (butlast (pathname-directory *load-pathname*) 1)
		   :name nil :type nil :version nil))

#+ACLPC
(load "fsl\\socket.fsl")

;;; Bootstrap translations if needed: HTTP:clim;clim-sys;sysdcl.lisp
;;; (load "HTTP:clim;clim-sys;sysdcl")
(load (merge-pathnames (make-pathname :directory '(:relative "clim" "clim-sys")
				      :name "sysdcl" :type "lisp")
		       *http-directory*))

(unless (ignore-errors (logical-pathname-translations "HTTP"))
  (setf (logical-pathname-translations "HTTP")
    `(("**;*.*.*" ,*http-directory*)
      ("*.*.*" ,*http-directory*))))

;;; Really needed before ever used
#+ACLPC
(defpackage "COMMON-LISP" (:use) (:export "ARGLIST"))

;;; Turn off ~pathname expansions
;;;
#+(and Allegro (not (version>= 4 3)))
(setq SYS:*TILDE-EXPAND-NAMESTRINGS* nil)

(defmacro with-input-argument ((key type) form)
  `(multiple-value-bind (#1=#:value #2=#:defined)
       (get-line-argument ,key ',type)
     (if #2# #1# ,form)))

#+Common-Graphics
(defun cg-prompt-user-string (prompt &optional (default "") error-p)
  (multiple-value-bind (value ignore button)
      (cg:ask-user-for-string prompt (if (stringp default) default "y") "Okay" "Abort")
    (declare (ignore ignore))
    (if (equal button "Abort")
	(if error-p (error "User prompt aborted."))
      value)))

(defun non-cg-prompt-user-string (prompt &optional (default "") error-p)
  (fresh-line)
  (when default
    (princ prompt)) 
  (force-output)
  (peek-char t)
  (read-line *standard-input* error-p))

(defun in-cg-listener-p ()
  (string-equal "IDE/Common Graphics" (mp::process-name mp::*current-process*)))

(defun prompt-user-string (prompt &optional (default "") error-p)
  #-Common-Graphics
  (non-cg-prompt-user-string prompt default error-p)
  #+Common-Graphics
  (if (in-cg-listener-p)
      (cg-prompt-user-string prompt default error-p)
      (non-cg-prompt-user-string prompt default error-p)))

;;; Extend y-or-n-p
;;;
(defun y-n-or-type (type format-string &rest args)
  (loop with prompt = t
      as entry = (prompt-user-string (apply #'format nil format-string args) prompt)
      as x = (ignore-errors (let ((*package* #.(find-package "CL-USER")))
			      (read-from-string entry)))
      do (case x
	   (y (return t))
	   (n (return nil))
	   (t (if (typep x type)
		  (return x)
		(cond ((eql (ignore-errors
			     (with-input-from-string (in entry)
			       (peek-char t in)))
			    #\;)
		       (setq prompt nil))
		      (t
		       (format t "Type \"y\" for yes, \"n\" for no, or a ~a." type)
		       (setq prompt t))))))))

#+Allegro
(defun save-lisp-image (name &rest options &key (exit t))
  (declare (ignore options))
  (excl:gc t)
  (cond ((fboundp 'excl:dumplisp)
	 (excl:dumplisp :name name :checkpoint nil)
	 (if exit (excl:exit)))
	(t
	 ; hum this spawns a process to save image...
	 (excl:build-lisp-image name :exit-after-image-build nil)
	 (if exit
	     (format t "~&Type :exit when image is fully saved...~%")))))

;;; Enable W3P
#-W3P
(when (with-input-argument ("w3p=" symbol)
	(y-or-n-p "Use W3P CLIM Presentation System (y/n)? "))
  (pushnew :W3P *features*))

;;; Enable Web Walker
#-W4
(when (with-input-argument ("w4=" symbol)
	(y-or-n-p "Use W4 Web Walker System (y/n)? "))
  (pushnew :W4 *features*))

#-PROXY
(when (with-input-argument ("proxy=" symbol)
	(y-or-n-p "Use Proxy System (y/n)? "))
  (pushnew :PROXY *features*))

;;; Load system definitions
;;;
#+ACLPC
(load "HTTP:acl;aclpc;sysdcl")

(load "HTTP:acl;server;sysdcl")

(load "HTTP:acl;translations")

(load "HTTP:acl;client;sysdcl")

;;; Q1:
;;;
(when (with-input-argument ("compile=" symbol)
	(y-or-n-p "Compile CL-HTTP just in case (y/n)? "))
  (let (*load-xref-info*)
    (compile-system "defsys"))
  (let ((*source-load-error* t))
    (ignore-errors (load-system #+PROXY "http-proxy" #-PROXY "http-base-client")))
  (compile-system #+PROXY "http-proxy" #-PROXY "http-base-client")
  (load-system "http-base-client" :reload nil)
  #+W4
  (let ((*patch-w4-compilation*
	 (compile-system "w4")))
    (load-system "w4")
    #+Allegro
    (if *patch-w4-compilation*
	(compile-system "w4" :recompile t)))
  (compile-system "cl-http-examples")
  #+W4
  (compile-system "w4-examples"))

;;; Loads the basic server and client
;;;
(load-system #+PROXY "http-proxy" #-PROXY "http-base-client")
;;; Option Load Web Walker
#+W4
(load-system "w4")

;;; Q2: If all attempts to get a domain name fail
;;;     this will ask the user to enter one.
;;;
#-ACLPC
(www-utils::default-domain-name)

;;; Q3: Do you want to make (or run) this image
;;;     for a different host than the current one?
;;;

(let ((shadow (with-input-argument ("host=" string)
		(y-n-or-type 'string "Use current host name (y) or enter a shadow host name here? "))))
  (if (and (stringp shadow)
	   (> (length shadow) 0))
      (if (zerop (www-utils::%parse-host-address shadow))
	  (warn "Ignoring shadow host name ~s, address not found." shadow)
	(setq http:*shadow-host-name* shadow))))

#+ACLPC
(if (probe-file "HTTP:acl;aclpc;debug.lsp")
    (when (y-or-n-p "Add trace for debugging (y/n)? ")
      (load "HTTP:acl;aclpc;debug.lsp")))
  
;;; Q4: The simplest way to reset the port later on is with :reload t
;;;     The acl;configuration file asks the port number question here:
;;;
(cond ((with-input-argument ("configure=" symbol)
	 #-ACLPC t #+ACLPC (y-or-n-p "Load configuration examples (y/n)? "))
       (load-system "cl-http-examples" :reload nil)
       #+W4
       (if (with-input-argument ("w4demo=" symbol)
	     (y-or-n-p "Load W4 web walker examples (y/n)? "))
	   (load-system "w4-examples"))
       #+ACLPC
       (format t "~&;;; You can:~%;;; 1. Execute the following to start CL-HTTP:~%> (start)~%;;; 2. Resume CL-HTTP activity after an error:~%> (resume)~%"))
      (t
       #+ACLPC
       (format t "~&;;;~%;;; You can save an ACLPC image now...~%;;;")))

;;; Q5:
;;;
#-ACLPC
(when (with-input-argument ("start=" symbol)
	(y-or-n-p "Enable HTTP Service now? "))
  (http:start))

#+ACLPC
(defun start (&optional (update t))
  (when update
    (load-system "cl-http") ;updates
    (load-system "cl-http-examples")
    (load-softly "patched" (translate-logical-pathname "HTTP:acl;aclpc;")))
  (format t "~&;;; To resume CL-HTTP activity after :EXIT or an error:~%> (resume)~%")
  (minp:scheduling :initform (http:start)))

#+ACLPC
(defun resume () (minp:scheduling))

#+ACLPC
(defun flush ()
  (loop for proc in (butlast (clim-sys:all-processes) 2)
      do (clim-sys:destroy-process proc))
  (clim-sys:all-processes))

#+ACLPC
(unless (find 'start ACL:*SESSION-INIT-FNS*)
  (nconc ACL:*SESSION-INIT-FNS* '(start)))
