;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

;;; Start up file for CL-HTTP under LispWorks.

;;; Prompts for options when run interactively.

;;; For command line, syntax is: -dump-cl-http <imagename> <options...>
;;; Options are T of present on the command line, NIL is preceeded by "no-".
;;; For a list of valid options options, see the symbol-names of the keywords
;;; listed in *cl-http-options*.

(in-package "CL-USER")

;;; lispm major.minor LispWorks major.minor
(setq *cl-http-server-version* '(70 23 1 5 3))

(setq *handle-existing-defpackage* '(:add :warn))

(defvar *cl-http-options* '((:compile . :ask)
			    (:debug-info . :ask)
			    (:proxy . :ask)
			    (:w4 . :ask)
			    (:examples . :ask)
			    (:mail-archive . :ask)
			    (:lambda-ir . :ask)
			    (:enable . :ask)))

(defvar *have-clim-sys* (not (null (find-package "CLIM-SYS"))))

(defvar *dump-cl-http-name*
  (member "-dump-cl-http" sys:*line-arguments-list* :test 'string-equal))

(defvar *cl-http-init-script*
  (or (second (member "-cl-http-init-script" sys:*line-arguments-list*
		      :test 'string-equal))
      "http:lw;examples;cl-http-init.lisp"))



(unless (ignore-errors (logical-pathname-translations "HTTP"))
  (setf (logical-pathname-translations "HTTP")
        `(("**;*.*.*"
           ,(merge-pathnames "**/*.*"
                             (truename (pathname-location
                                        (current-pathname "../"))))))))

(load "HTTP:lw;pre-cl-http")

(defvar *cl-http-options-data*
  `((:compile "Compile CL-HTTP just in case" t)
    (:debug-info "Include full debugging information" nil)
    (:proxy "Include proxy" t)
    (:w4 "Include W4 Web Walker" nil)
    (:examples "Include examples" ,(not *dump-cl-http-name*))
    (:mail-archive "Include mail-archive example" nil)
    (:lambda-ir "Include lambda-ir" nil)
    (:enable "Enable HTTP server" ,(not *dump-cl-http-name*))))

(cond (*dump-cl-http-name*
       (loop for (key message default) in *cl-http-options-data*
	     do
	     (when (eq (cdr (assoc key *cl-http-options*)) :ask)
	       (setq *cl-http-options*
		     (cons (cons key (if default
					 (null (member (format nil "no-~A" key)
						       (cddr *dump-cl-http-name*)
						       :test 'string-equal))
				       (not (null (member key
							  (cddr *dump-cl-http-name*)
							  :test 'string-equal)))))
			   (delete key *cl-http-options* :key 'car)))))
       (format t "~%~SCreating CL-HTTP image ~S with options:~%"
	       *dump-cl-http-name*)
       (loop for (key message) in *cl-http-options-data*
	     do
	     (format t "~A = ~A~%" key (cdr (assoc key *cl-http-options*))))
       (terpri))
      ((and (member :CAPI *features*) #+CAPI (capi:screens))
       #+CAPI
       (let ((count -1)
	     (options '())
	     (selection '()))
	 (loop for (key message default) in *cl-http-options-data*
	       do
	       (when (eq (cdr (assoc key *cl-http-options*)) :ask)
		 (push (cons key message) options)
		 (if default
		     (push (incf count) selection)
		   (incf count))))
	 (setq options (nreverse options)
	       selection (nreverse selection))
         (when options
	   (let ((list (make-instance 'capi:check-button-panel
				      :items options
				      :selection selection
				      :print-function 'cdr
				      :layout-class 'capi:column-layout
				      :visible-border t)))
	     (if (capi:popup-confirmer list "Choose CL-HTTP options")
	         (let ((on-options (capi:choice-selected-items list)))
		   (loop for (key) in options
		         do
		         (setq *cl-http-options*
			       (cons (cons key (not (null (assoc key on-options))))
				     (delete key *cl-http-options* :key 'car)))))
	       (exit-load))))))
      (t (loop for (key message default) in *cl-http-options-data*
	       do
	       (when (eq (cdr (assoc key *cl-http-options*)) :ask)
		 (setq *cl-http-options*
		       (cons (cons key (not (null (y-or-n-p (format nil "~A (y/n)?" message)))))
			     (delete key *cl-http-options* :key 'car)))))))

(when (cdr (assoc :w4 *cl-http-options*))
  (push :w4 *features*))

;;; Load system definitions
;;;
(unless *have-clim-sys*
  (load "HTTP:clim;clim-sys;lw-sysdcl.lisp"))

(load "HTTP:lw;server;sysdcl.lisp")

(load "HTTP:lw;translations.lisp")

(load "HTTP:lw;client;sysdcl.lisp")



(eval `(defsystem cl-http-examples (:default-type :system)
	 :members ,(append '(cl-http-base-examples)
			   #+W4 '("W4-EXAMPLES"))
	 :rules ((:in-order-to :compile :all
			       (:requires (:load :serial))))))

(eval `(defsystem cl-http-start-system (:default-type :system)
	 :members ,(append (and (not *have-clim-sys*)
				'(clim-sys))
			   '(cl-http)
			   (and (cdr (assoc :proxy *cl-http-options*))
				'("HTTP-PROXY"))
			   '(http-base-client)
			   #+W4 '("W4")
			   (and (cdr (assoc :mail-archive *cl-http-options*))
				'("MAIL-ARCHIVE"))
			   (and (cdr (assoc :lambda-ir *cl-http-options*))
                                (if (cdr (assoc :mail-archive *cl-http-options*))
                                    '("LAMBDA-IR-AND-MAIL-ARCHIVE-INDEX")
                                  '("LAMBDA-IR")))
			   (and (cdr (assoc :examples *cl-http-options*))
				'(cl-http-examples)))
	 :rules ((:in-order-to :compile :all
			       (:requires (:load :serial))))))

(let* ((debug-info (cdr (assoc :debug-info *cl-http-options*)))
       (compiler::*source-file-recording* debug-info)
       (compiler::*source-level-debugging* debug-info)
       #+(or unix (not LispWorks4.0))
       (compiler::*notice-changed-definitions* debug-info)
       (compiler::*produce-xref-info* debug-info)
       (compiler::*load-xref-info*  debug-info))
  (when debug-info
    (proclaim '(optimize (debug 3))))
  (if (cdr (assoc :compile *cl-http-options*))
      (compile-system 'cl-http-start-system :load t)
    (load-system 'cl-http-start-system)))

(defun cold-enable-http-service ()
  (load *cl-http-init-script*)
  (push '("cl-http init" nil http:enable-http-service) mp:*initial-processes*)
  (mp:initialize-multiprocessing))

(when *dump-cl-http-name*
  (save-image (second *dump-cl-http-name*)
	      :restart-function 'cold-enable-http-service
	      #-LispWorks3.2 :comment #-LispWorks3.2 "CL-HTTP server")
  (quit))

(when (cdr (assoc :enable *cl-http-options*))
  (http:enable-http-service))

