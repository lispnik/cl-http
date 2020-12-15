;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-99, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  ENVIRONMENT DEFINITION FOR MCL CL-HTTP
;;;

;; If you want to just start up the server using the standard demo configuration,
;; you should load http:mcl;mcl-start-server.lisp instead.

;; If you load this file and evaluate the form
;; (http-user:enable-http-service), you can access the URL
;; http://your.host.domain.name/ and peruse the server documentation.

(in-package :cl-user) 

(declaim (special *cl-http-server-version*))

;; lispm major.minor MAC major.minor.rev
(setq *cl-http-server-version* '(70 23 3 6 9))

;; Load the logical pathname translations
(load (merge-pathnames "translations"  ccl:*loading-file-source-file*) :verbose t) 

;; allow the values declaration
(define-declaration values nil)

(define-declaration arglist nil)

;;;------------------------------------------------------------------- 
;;;
;;; DEFINE A LOAD FORM 
;;;

(defparameter *always-compile* (if (boundp '*always-compile*) (symbol-value '*always-compile*) nil)
  "When non-null all files are compiled, whether or not they need it.")

(defmethod require-pathname ((pathname pathname))
  (ccl:require (pathname-name pathname) pathname))

(defun conditional-compile-file (file &key load  verbose  output-file always require
				      (save-local-symbols *fasl-save-local-symbols*) 
				      (save-doc-strings *fasl-save-doc-strings*)
				      (save-definitions *fasl-save-definitions*))
  (let* ((source (merge-pathnames ".lisp" file))
	 (fasl (merge-pathnames ccl::*.fasl-pathname* (or output-file file))))
    (unless (probe-file source)
      (error 'file-error :pathname file))
    (cond ((or always
	       *always-compile*
	       (not (probe-file fasl))
	       (< (file-write-date fasl) (file-write-date source)))
	   (compile-file source :output-file fasl :load load :verbose verbose
			 :save-local-symbols save-local-symbols
			 :save-doc-strings save-doc-strings
			 :save-definitions save-definitions))
	  (require (require-pathname fasl))
	  (load (load fasl :verbose verbose))
	  (t nil))))

(defun execute-system-operations (system operations &aux ops)
   (check-type system symbol)
   (macrolet ((processing-module ((module) &body clauses)
                       `(destructuring-bind (case action &rest files) ,module
                           (ecase case ,@clauses))))
       (flet ((check-operation-arg (arg)
                   (let ((known-operations '(:load :compile :eval-load :compile-load :compile-load-always)))
                      (unless (member arg known-operations)
                         (error "~S is not one of the know options for OPERATIONS."arg known-operations))))
                (eval-load (pathname &rest args)
                   (apply #'load (merge-pathnames ".lisp" pathname)  args))
                (perform-action (action files &optional require-only)
                   (ecase action
                      (:load 
                        (cond (require-only (mapc #'require-pathname files))
                                 (t (dolist (file files)
                                        (load file :verbose t)))))
                      (:compile-load
                        (dolist (file files)
                           (conditional-compile-file file :verbose t :load t :require t))))))
          ;; check arguments to operations
          (mapc #'check-operation-arg operations)
          ;;Combined operations take precedence
          (unless (intersection  operations '(:compile-load :compile-load-always :eval-load))
             (when (member :load operations)
                 (push '(load :verbose t) ops))
             (when (member :compile operations)
                 (push '(conditional-compile-file :verbose t :always t) ops)))
          ;; Combined operations
          (cond ((member :compile-load-always operations)
                     (push '(conditional-compile-file :always  t :load t  :verbose t :save-local-symbols t
                                 :save-doc-strings t :save-definitions t) ops))
                   ((member :compile-load operations)
                     (push '(conditional-compile-file :load t  :verbose t :save-local-symbols t
                                 :save-doc-strings t :save-definitions t) ops))
                   ((member :eval-load operations)
                     (push `(,#'eval-load :verbose t) ops)))
          ;; Perform operations
          (loop for module in (symbol-value (find-system-named system))
                   do (typecase module
                          (cons
                            (processing-module 
                              (module)
                              (:in-order-to-load
                                (perform-action action files t))
                              (:in-order-to-compile
                                (when (intersection operations '(:compile-load :compile :compile-load-always))
                                    (perform-action action files t)))))
                          (t (loop for (fctn . args) in ops
                                      do (apply fctn module args)))))
          (set-system-loaded-p system)
          system))) 

(defvar *systems* nil) 
(defvar *system-define-hook* nil)
(defvar *system-undefine-hook* nil) 

(defun get-system-name (system)
   (get system :system-name))

(defun get-system-keyword (system)
   (get system :system-keyword))

(defun get-system-description (system)
  (get system :system-description))

(defun set-system-loaded-p (system)
   (let* ((sys (find-system-named system))
             (key (get-system-keyword sys)))
      (setf (get key :system-loaded-p) t)))

(defun system-loaded-p (system)
   (let* ((sys (find-system-named system))
             (key (get-system-keyword sys)))
      (get key :system-loaded-p)))

(defun register-system (system name &optional description)
  (prog1 (pushnew system *systems*)
	 (setf (get system :system-name) name
               (get system :system-keyword) (intern (string-upcase name) :keyword))
	 (if description
	     (setf (get system :system-description) description)
	     (remprop system :system-description))
	 (mapc #'funcall *system-define-hook*)))

(defun unregister-system (system)
   (setq *systems* (delete system *systems*))
   (mapc #'funcall *system-undefine-hook*)
   (remprop system :system-name)
   (remprop system :system-keyword))

(defun find-system-named (string &optional (error-p t)) 
   (cond ((cond ((stringp string) (find string *systems* :test #'equalp :key #'get-system-name))
                       ((keywordp string) (find string *systems* :test #'eql :key #'get-system-keyword))
                       ((symbolp string) (find string *systems* :test #'eql))
                       (t nil)))
            (error-p (error "No system named ~A found." string))
            (t nil)))

(defmacro define-system ((name &key description) (&rest operations) &body files)
   "Operations can be :load and :compile."
   (let ((var (intern (concatenate 'string "*" (string name) "*"))))
      `(progn
          (defparameter ,var ',files)
          (register-system ',var ,(string-upcase name) ',description)
          ,(when operations
                ` (execute-system-operations ',var ',operations))
          ',var)))

(defun system-module-file (system-module)
   (etypecase  system-module
      (cons (third system-module))
      (string system-module)))

(defun system-files (system)
   "Returns the files of SYSTEM in compile-load order."
   (loop for module in (symbol-value (find-system-named system))
            collect (system-module-file module)))

(defun system-map-files (system function)
   (loop for module in (symbol-value (find-system-named system))
            for file = (system-module-file module)
            do (funcall function file)))

(defun system-edit-files (system)
   (system-map-files system #'ed))

(defun load-system (system)
   (execute-system-operations system '(:compile-load)))

(defun compile-system (system &key (condition :new-version))
  "Compiles SYSTEM. 
CONDITION can be :NEW-VERSION to compile just changed files or
:ALWAYS to recompile the entire system."
  (execute-system-operations system 
                             (list (ecase condition
                                     (:new-version :compile-load)
                                     (:always :compile-load-always)
                                     (:compile :compile)))))

(defparameter *standard-systems* nil)

(defun register-system-definition (keyword pathname)
   (setf (get keyword :system-pathname) pathname))

(defun get-system-definition-pathname (keyword &optional (error-p t))
   (cond ((get keyword :system-pathname))
            (error-p (error "No system named, ~A, has been registered." keyword))
            (t nil)))

(defmacro note-system-definitions (&body system-clauses)
   `(progn . ,(loop for (keyword pathname) in system-clauses
                           collect `(register-system-definition (intern ,(string-upcase keyword) :keyword) (pathname ,pathname)))))

(defun load-standard-systems (&optional (systems *standard-systems*))
   (loop for keyword in systems
            do (load (get-system-definition-pathname keyword))
            (load-system (find-system-named keyword)))) 

;;;------------------------------------------------------------------- 
;;;
;;;  CL-HTTP SYSTEM CONFIGURATION
;;;

;; This advises the system how to load various CL-HTTP components.
;; The keyword should be identical with the system name, except that it is a keyword.
;; The pathname is typically the system definition, and will load the system.
(note-system-definitions
  (:cl-http "http:mcl;server;sysdcl")		;  Server definition
  (:cl-http-client-substrate "http:mcl;client;sysdcl-substrate")	; Client Substrate
  (:cl-http-proxy "http:mcl;proxy;sysdcl")	; proxy server (includes client substrate)
  (:cl-http-client "http:mcl;client;sysdcl")	;  S-Exp Browser
  (:w4-web-walker "http:mcl;w4;sysdcl")		; W4 Constraint-Guided Web Walker
  (:lambda-ir "http:mcl;lambda-ir;sysdcl")	;  Hybrid Retrieval System
  (:html-parser "http:mcl;html-parser;sysdcl")	;  HTML Parser 
  (:cl-http-examples "http:examples;exports")	; Server Examples
  (:w4-web-walker-demo "http:mcl;w4;sysdcl-examples")
  (:cgi-support "http:mcl;contrib;tnorderhaug;cgi;cgi-sysdcl")	; CGI Support
  (:db-auth "http:contrib;pmeurer;dbauth;sysdcl"))	; Database Authentication

(setq *standard-systems* '(:cl-http :cl-http-proxy :cl-http-client :w4-web-walker))

;; Load the standard CL-HTTP components
(load-standard-systems)
