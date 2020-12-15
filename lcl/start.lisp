;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

(in-package "CL-USER")

;;; lispm major.minor LCL major.minor
(setq *cl-http-server-version* '(70 21 4 2 4))

(proclaim '(optimize (speed 3) (safety 0) (compilation-speed 0) (space 0)))
;;; Prevent in-package warnings. `ansi-package' defines `in-package' as
;;; a macro in the common-lisp package expanding in a setq statement.
;;; The reader expects still a in-package statement, with in-package
;;; interned in the lisp package
(unless (eq 'lisp::in-package 'cl::in-package)
  (setf lcl::*warn-if-no-in-package* ()))

;;; Load logical pathname package
(load "lcl/logical-pathnames")
(setf *features* (remove :cltl2 *features*))
(setf (lp::translation-rule-case (lp::find-translation-rule :logical)) 
  :unchanged)
(lp::convert-file-function lisp::pathname)

;;; Load defsystem package
(load "lcl/defsystem")

;;; Missing definition
(defmacro declaim (&rest args) 
  (let ((declares (mapcan #'(lambda (arg)
			      (if (member (car arg) '(function ftype))
				  ()
				(list `',arg)))
			  args)))
    (if declares
	`(proclaim ,@declares))))	

(defadvice (lucid::check-valid-decl-spec funcs) (form &optional flag)
  (if (member (car form) '(function values ftype dynamic-extent))
      (cond ((member (car form) '(function values ftype))
	     ;;(format t "~%;;; Warning: Ignoring ~s declaration"
	     ;;     (car form))
	     (values))
	    (t
	     (let ((args (remove-if #'consp (cdr form))))
	       (unless (equal args (cdr form))
		;;(format t "~%;;; Warning: Invalid ~s specification: ~s"
		;;	 (car form) (set-difference (cdr form) args))
		 (setf (cdr form) args))
	       (if (not (null args))
		   (advice-continue form flag)
		 (values)))))
    (advice-continue form flag)))

(declaim (notinline fdefinition))
(defadvice (fdefinition nil-allowed) (name)
  (if (null name)
      (progn 
	;; (format t "~%;;; Warning FDEFINITION called with nil argument")
	())
    (advice-continue name)))

(defvar *cl-http-options* '(:ask-compile :ask-enable))
(defvar *have-clim-sys* (not (null (find-package "CLIM-SYS"))))


;;; It's simple enought to assume the default directory
;;; has been set once to the CL-HTTP directory.
;;; Then access to working-directory is all that's
;;; needed for an implementation to get started.
;;;

(unless (ignore-errors (logical-pathname-translations "HTTP"))
  (setf (logical-pathname-translations "HTTP")
    `(("**;*.*.*" ,(namestring (merge-pathnames "**/*.*" (truename (working-directory))))))))

;;; Load system definitions
;;;
(unless *have-clim-sys*
  (load "HTTP:lcl;clim-sys;sysdcl.lisp"))

(load "HTTP:lcl;server;sysdcl.lisp")

(load "HTTP:lcl;translations.lisp")

(load "HTTP:lcl;client;sysdcl.lisp")

;;; This loads the basic server
;;;

;;; Q1:
;;;
(cond ((or (member :compile *cl-http-options*)
	   (and (member :ask-compile *cl-http-options*)
		(y-or-n-p "Compile CL-HTTP just in case (y/n)? ")))
       (compile-file "lcl/logical-pathnames")
       (compile-file "lcl/defsystem")
       (unless *have-clim-sys*
	 (compile-system 'clim-sys))
       (compile-system 'cl-http)
       (compile-system 'cl-http-examples)
       ;; This is untested
       (compile-system 'http-base-client))
      (t
       (unless *have-clim-sys*
	 (load-system 'clim-sys 
		      :compile-during-load ()
		      :load-source-if-no-binary t
		      :bother-user-if-no-binary ()))
       (load-system 'cl-http 
		    :compile-during-load ()
		    :load-source-if-no-binary t
		    :bother-user-if-no-binary ())))
  

;;; Q2: If the domain name lookup fails this will
;;;     ask the user to enter a domain name.
;;; The result is cached in file acl;defaultdomain
;;;
#+Allegro
(www-utils::default-domain-name)

;;; Q3: The simplest way to reset the port later on is with :reload t
;;;     The lcl;configuration file asks the port number question here:
;;;
(load-system 'cl-http-examples 
	     :compile-during-load ()
	     :load-source-if-no-binary t
	     :bother-user-if-no-binary ())

;(setq http::*cl-http-home-page-url-string*
;  "http://www.aie.nl/~gertjan/cl-http.html")
;(setq http::*server-version* "CL-HHTP at AI Engineering B.V")
;;; Q4:
;;;
(when (or (member :enable *cl-http-options*)
	  (and (member :ask-enable *cl-http-options*)
	       (y-or-n-p "Enable HTTP Service now? ")))
  (http:enable-http-service))
