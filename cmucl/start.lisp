;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

(in-package "CL-USER")

;;; lispm major.minor CMUCL major.minor
(defparameter *cl-http-server-version* '(70 23 0 3 2))

;; Load PCL if necessary.
#-pcl
(progn
  (load "target:pcl/pclload")
  #+gencgc (gc :full t)
  #-gencgc (purify))

;;;;;;;;;;;;
(in-package "CL-USER")

#+pcl
(eval-when (compile load eval)
  (pushnew 'values  pcl::*non-variable-declarations*)
  ;; The following are not necessary for CL-HTTP but may be handy in
  ;; some code.
  #+nil (pushnew 'compile pcl::*defclass-times*)
  #+nil (pushnew 'compile pcl::*defgeneric-times*))

#+cmu17
(setf c:*suppress-values-declaration* t)

(defvar *cl-http-options*
  '(:ask-compile #+nil :compile 
;    :cl-http-client	; Basic client substrate.
;    :cl-http-proxy	; Requires the client.
;    :w4-web-walker	; Requires the client.
;    :lambda-ir		; Broken.
;    :html-parser	; Can also be compiled stand-alone.
    ;; May not want to load the examples when dumping a lisp core.
    :cl-http-examples
;    :w4-web-walker-demo
    :ask-enable #+nil :enable))

;;; It's simple to assume the default directory has been set once to
;;; the CL-HTTP directory.  Then access to working-directory is all
;;; that's needed for an implementation to get started.
;;;

(defvar *http-directory*
  ;; The default; compiling from within the cl-http/ directory.
  (ext:default-directory)
  ;; Can override the above which is necessary if not loading from the
  ;; cl-http source root directory.
  #+nil "/<cl-http source root directory>/")

(unless (ignore-errors (logical-pathname-translations "HTTP"))
  (setf (logical-pathname-translations "HTTP")
    `(("**;*.*.*" ,(namestring
		    (merge-pathnames "**/*.*" *http-directory*))))))

(load "HTTP:cmucl;translations.lisp")

;;; Compile and load the defsystem package.
(unless (find-package :mk)
  (let ((obj (make-pathname
	      :defaults (translate-logical-pathname "HTTP:cmucl;defsystem")
	      :type (c:backend-byte-fasl-file-type c:*backend*))))
    (when (< (or (file-write-date obj) 0)
	     (file-write-date "HTTP:cmucl;defsystem.lisp"))
      (compile-file "HTTP:cmucl;defsystem" :byte-compile t))
    (load (translate-logical-pathname "HTTP:cmucl;defsystem"))))

;;;
;;; Load system definitions
;;;
(load "HTTP:cmucl;server;sysdcl.lisp")
(load "HTTP:cmucl;client;sysdcl.lisp")
(load "HTTP:cmucl;proxy;sysdcl.lisp")
(load "HTTP:cmucl;w4;sysdcl.lisp")
(load "HTTP:cmucl;lambda-ir;sysdcl.lisp")

;;;; Compile and load the server.

(cond ((or (member :compile *cl-http-options*)
	   (and (member :ask-compile *cl-http-options*)
		(y-or-n-p "Compile CL-HTTP just in case (y/n)? ")))
       (with-compilation-unit
	(:optimize
	 '(optimize (speed #+byte-compile 0 #-byte-compile 2)
	            (space 2) (inhibit-warnings 2)
		    (safety #+small 0 #-small 1)
		    (debug #+small .5 #-small 2))
	 :optimize-interface
	 '(optimize-interface (safety #+small 1 #-small 2)
			      (debug #+small .5 #-small 2))
	 :context-declarations
	 '(((:and :external :global)
	    (declare (optimize-interface (safety 2) (debug 1))))
	   ((:and :external :macro)
	    (declare (optimize (safety 2))))
	   (:macro (declare (optimize (speed 0))))))
	(compile-system 'cl-http)
	(when (member :cl-http-client *cl-http-options*)
	  (compile-system 'cl-http-client))
	(when (member :cl-http-proxy *cl-http-options*)
	  (compile-system 'cl-http-proxy))
	(when (member :w4-web-walker *cl-http-options*)
	  (compile-system 'w4-web-walker))
	(when (member :lambda-ir *cl-http-options*)
	  (compile-system 'lambda-ir))
	(when (member :cl-http-examples *cl-http-options*)
	  (compile-system 'cl-http-examples))
	(when (member :w4-web-walker-demo *cl-http-options*)
	  (compile-system 'w4-web-walker-demo))))
      (t
       (load-system 'cl-http
		    :compile-during-load ()
		    :load-source-if-no-binary t
		    :bother-user-if-no-binary ())
       (when (member :cl-http-client *cl-http-options*)
	 (load-system 'cl-http-client
		      :compile-during-load ()
		      :load-source-if-no-binary t
		      :bother-user-if-no-binary ()))
       (when (member :cl-http-proxy *cl-http-options*)
	 (load-system 'cl-http-proxy
		      :compile-during-load ()
		      :load-source-if-no-binary t
		      :bother-user-if-no-binary ()))
       (when (member :w4-web-walker *cl-http-options*)
	 (load-system 'w4-web-walker
		      :compile-during-load ()
		      :load-source-if-no-binary t
		      :bother-user-if-no-binary ()))
       (when (member :lambda-ir *cl-http-options*)
	 (load-system 'lambda-ir
		      :compile-during-load ()
		      :load-source-if-no-binary t
		      :bother-user-if-no-binary ()))
       (when (member :cl-http-examples *cl-http-options*)
	 (load-system 'cl-http-examples
		      :compile-during-load ()
		      :load-source-if-no-binary t
		      :bother-user-if-no-binary ()))
       (when (and (member :cl-http-examples *cl-http-options*)
		  (member :w4-web-walker-demo *cl-http-options*))
	 (load-system 'w4-web-walker-demo
		      :compile-during-load ()
		      :load-source-if-no-binary t
		      :bother-user-if-no-binary ()))))

;;; Compile and load the HTML parser which may be loaded stand-alone.
(when (member :html-parser *cl-http-options*)
  (load "HTTP:cmucl;html-parser;sysdcl"))

#+gencgc (gc :full t)
#-gencgc (purify)

(when (or (member :enable *cl-http-options*)
	  (and (member :ask-enable *cl-http-options*)
	       (y-or-n-p "Enable HTTP Service now? ")))
  (http:enable-http-service))

;;; Multi-processing setup.
#+MP
(progn
  ;; Setup the event server timeout so that an interactive process can
  ;; act as the idle loop.
  (setf lisp::*max-event-to-sec* 0
	lisp::*max-event-to-usec* 500000)

  ;; Setup the initial process as the idle process.
  (setf mp::*idle-process* mp::*initial-process*
	mp::*idle-loop-timeout* 0.1d0)

  ;; Start a background SIGALRM driven process-yield, every 10 seconds,
  ;; in case of stuck connections. E.g. The opening of remote
  ;; connections can lockup. Since CMUCL is not yet interrupt safe this
  ;; is not suggested.
  #+nil (mp::start-sigalrm-yield 10 0)

  ;; If not interactive then run the idle loop.
  #+nil (mp::idle-process-loop)
) ; end progn MP

;;; Dump a cl-http lisp core.
#+nil (www-utils::save-cl-http "cl-http.core")
