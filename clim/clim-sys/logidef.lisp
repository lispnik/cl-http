;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: CL-USER; Base: 10 -*-

(in-package "CL-USER")

;;; Logical pathnames supplement. This only adds
;;; minimal logical pathnames where not available.
;;; Copyright (C) 1994, 1995 Olivier (OBC) all rights reserved.
;;; See copyright notice in CLIM-SYS;MINIPROC file.
;;;

(eval-when (compile load eval)
(defvar *original-definitions* (make-hash-table))

(defmethod original-definition ((sym symbol) &optional lambda-list (method sym))
  (let ((class-names (loop for arg in lambda-list
			 until (member arg '(&rest &key &optional &aux &allow-other-keys))
			 collect (or (and (consp arg)
					  (symbolp (second arg))
					  (second arg))
				     t))))
    (if class-names
	(setq sym (intern (format nil "~a ~s" sym class-names) (symbol-package sym))))
    (multiple-value-bind (value found)
	(gethash sym *original-definitions*)
      (if found
	  (values value found)
	(setf (gethash sym *original-definitions*)
	  (or (macro-function sym)
	      (if (fboundp method)
		  (if (null class-names)
		      (fdefinition method)
		    (let ((function (fdefinition method)))
		      (typecase function
			(generic-function
			 (find-method function nil (mapcar #'find-class class-names) nil))
			(t (error "Cannot find a method without specializers ~a." sym))))))))))))

(defmethod original-definition ((method cons) &optional lambda-list (sym (intern (write-to-string method))))
  (original-definition sym lambda-list method))

(defvar *denotify* #+Franz-Inc nil #-Franz-Inc t)

(defmacro handling-redefinition (&rest body)
  `(handler-bind (((or simple-error error condition)
		   ;; package-locked-error?
		   #'(lambda (c)
		       (if *denotify*
			   (format t "~&;;; Redefinition: ~a.~%"
				   c))
		       (continue))))
     ,@body))

(defmethod original-function-p ((sym symbol))
  (and (fboundp sym)
       (eql (original-definition sym) (fdefinition sym))))
)

(defvar *redefinition-error* nil)

(defmacro define (type name redefine . method-specs)
  (let ((#1=#:original (original-definition name))
	(#2=#:origname (intern (concatenate 'string "ORIGINAL-" (symbol-name type))))
	(#3=#:defname (ecase type
			(:function 'defun)
			((:method :macro) (intern (concatenate 'string "DEF" (symbol-name type)))))))
    (cond ((eql redefine :redefine)
	   (if #1#
	       `(let ((#1# (original-definition ',name)))
		  (macrolet ((,#2# () '#1#))
		    (fmakunbound ',name)
		    (,#3# ,name ,@method-specs)))
	     (if *redefinition-error*
		 (error "No original ~a to redefine for ~a." type name)
	       `(,#3# ,name ,@method-specs))))
	  ((null #1#)
	   `(,#3# ,name ,redefine ,@method-specs))
	  (*denotify*
	   (format t "~&Reusing ~a." name)))))

(defmacro define-specific-method (name lambda-list . method-specs)
  (let ((#1=#:original (original-definition name lambda-list)))
    (if #1#
	`(let ((#1# (original-definition ',name ',lambda-list)))
	   (macrolet ((original-method () '#1#))
	     (defmethod ,name ,lambda-list ,@method-specs)))
      `(macrolet ((original-method () nil))
	 (defmethod ,name ,lambda-list ,@method-specs)))))

(defmacro define-method (name redefine . method-specs)
  `(define :method ,name ,redefine ,@method-specs))

(defmacro define-function (name redefine . method-specs)
  `(define :function ,name ,redefine ,@method-specs))

(defmacro define-macro (name redefine . method-specs)
  `(define :macro ,name ,redefine ,@method-specs))

(defvar *logical-translations* (make-hash-table :test #'equalp))

(define-method logical-pathname-translations ((host string))
  (gethash host *logical-translations*))

(define-method (setf logical-pathname-translations) ((translations cons) (host string))
  (setf (gethash host *logical-translations*) translations))

(define-method logical-pathname-host-translations ((path string))
  (let ((ploghost (position #\: path)) loghost)
    (if ploghost
	(setq loghost (subseq path 0 ploghost)))
    (values (if loghost (logical-pathname-translations loghost))
	    loghost)))

(define-method translate-logical-pathname ((path string))
  (multiple-value-bind (translations loghost)
      (logical-pathname-host-translations path)
    (cond (translations
	   (let ((rest (subseq path (1+ (length loghost)))))
	     (let ((match (assoc rest translations :test #'wild-match :key #'wild-pattern))
		   key path)
	       (setq key (wild-keyword (first match))
		     path (second match))
	       (let ((start (search key rest)))
		 (if start
		     (setq rest (subseq rest (+ start (length key))))))
	       (values (append-logical-pathname path rest) t))))
	  (t
	   (pathname path)))))

(define-method translate-logical-pathname ((path pathname))
  (translate-logical-pathname (namestring path)))

(unless (original-function-p 'translate-logical-pathname)
(define-method append-logical-pathname ((path pathname) (logical string) &rest options)
  (multiple-value-bind (translations loghost)
      (logical-pathname-host-translations logical)
    (if translations
	(if options
	    (apply #'make-pathname :defaults (translate-logical-pathname logical) options)
	  (translate-logical-pathname logical))
      (if loghost
	  (error "Logical host undefined, ~a for, ~a." loghost logical)
	(let ((expand (expand-logical-pathname logical)))
	  (let ((host (pathname-host path))
		(device (pathname-device path))
		(directory (pathname-directory path)))
	    (setf (getf expand :directory) (append directory (getf expand :directory))
		  (getf expand :host) host
		  (getf expand :device) device)
	    ;; Overwrites
	    (loop for (key val) on options by #'cddr
		do (setf (getf expand key) val))
	    (apply #'make-pathname expand)))))))
)

(define-method expand-logical-pathname ((string string))
  (let ((pos (position #\: string)) loghost directory rest)
    (if pos
	(setq loghost (subseq string 0 pos))
      (setq pos -1))
    (setq directory
      (loop for start = pos then end
	  as end = (position #\; string :start (incf start))
	  while end
	  collect (subseq string start (setq pos end))))
    (setq rest (pathname (subseq string (1+ pos))))
    `(,@(if loghost `(:host ,loghost)) ,@(if directory `(:directory ,directory))
	:name ,(pathname-name rest) :type ,(pathname-type rest) :version ,(pathname-version rest))))
    
(define-method wild-match ((string string) (pattern string))
  (if (and (zerop (length string))
	   (loop for c across pattern
	       always (char= c #\*)))
      0
    (search-wild pattern string)))

(define-method wild-pattern ((string string))
  (let ((pos (or (search "**" string)
		 (position #\; string :from-end t))))
    (if pos
	(subseq string 0 (+ pos 2))
      (if (setq pos (position #\. string))
	  (subseq string 0 pos)
	"*"))))

(define-method wild-keyword ((string string))
  (let ((pos (or (search "**" string)
		 (position #\; string :from-end t))))
    (if pos
	(subseq string 0 pos)
      "")))

;;; Minimal translation extensions
;;;

(intern "LOGICAL-PATHNAME" "COMMON-LISP")

(defpackage "COMMON-LISP" (:use) (:export "LOGICAL-PATHNAME"))

(unless (find-class 'common-lisp:logical-pathname nil)
  (deftype common-lisp:logical-pathname () 'string)
  (setf (find-class 'common-lisp:logical-pathname) (find-class 'string)))

(define-method logical-pathname (path)
  (pathname path))

(defparameter *source-type* "lisp")

(defparameter *binary-type* #+UNIX "fasl" #-UNIX "fsl")

(unless (original-function-p 'translate-logical-pathname)
(define-method load :redefine (filename &rest args)
  (let ((filepath (translate-logical-pathname filename)))
    (if (pathname-type filepath)
	(apply (original-method) filepath args)
      ;; Check default types loading
      (let ((binpath (make-pathname :type *binary-type* :defaults filepath)))
	(if (probe-file binpath)
	    (apply (original-method) binpath args)
	  (let ((sourcepath (make-pathname :type *source-type* :defaults filepath)))
	    (if (probe-file sourcepath)
		(apply (original-method) sourcepath args)
	      ;; Issue original error
	      (apply (original-method) filepath args))))))))

(define-method compile-file :redefine (filename &rest args &key output-file &allow-other-keys)
  (if output-file
      (setf (getf args :output-file) (translate-logical-pathname output-file)))
  (apply (original-method) (translate-logical-pathname filename) args))

(define-method open :redefine (filename &rest args)
  (apply (original-method) (translate-logical-pathname filename) args))

(define-method probe-file :redefine (pathname)
  (funcall (original-method) (translate-logical-pathname pathname)))

(define-specific-method make-load-form ((object pathname) #+(or :ANSI-CL :DRAFT-ANSI-CL-2) &optional #-(or :ANSI-CL :DRAFT-ANSI-CL-2) &aux environment)
  (let ((dev (pathname-device object)))
    (if (and (stringp dev)
	     (logical-pathname-translations dev))
	`(translate-logical-pathname ,(namestring object))
      (let ((original (original-method)))
	(if original
	    (if environment
		(funcall original object environment)
	      (funcall original object))
	  (call-next-method))))))
)

(let (logical-hosts)
(define-method logical-pathname-translations-database-pathnames ()
  logical-hosts)
(define-method (setf logical-pathname-translations-database-pathnames) (x)
  (setf logical-hosts x))
logical-hosts)

(define-function load-logical-pathname-translations (host)
  (load-logical-pathname-translations-patch host))

(defun load-logical-pathname-translations-patch (host &key all)
  (loop with hostspath = (mapcar #'merge-pathnames (mapcar #'translate-logical-pathname (logical-pathname-translations-database-pathnames)))
      with translations
      for path in hostspath
      do (if (probe-file path)
	     (with-open-file (stream path :direction :input)
	       (loop with eof = '#:eof
		   as read = (read stream nil eof)
		   until (eq read eof)
		   if (equalp read host)
		   do (flet ((coltrans ()
			       (loop do (setq read (read stream nil eof))
				   while (consp read)
				   collect (eval read))))
			(when (setq translations (coltrans))
			  (setf (logical-pathname-translations host) translations)
			  (when all
			    (loop with translations
				as host = read
				do (cond ((stringp host)
					  (if (setq translations (coltrans))
					      (setf (logical-pathname-translations host) translations)
					    (error "Host translations for ~s not found in ~s." host hostspath)))
					 ((eq read eof)
					  (return))
					 (t
					  (error "Unexpected host ~s in ~s." host hostspath))))))
			(return)))))
      if translations
      do (return translations)
      finally (error "Host ~s not found in ~s." host hostspath)))

;;; Simple search for a :WILD pattern expressions into a string
;;; for example: "*a" "*a*" "a*" "*a*b*".
;;;
(defun search-wild (pattern string &rest options &key test test-not collect)
  (declare (ignore test test-not))
  (remf options :collect)
  (prog ((pl (length pattern))
	 (sl (length string))
	 star hit hits (star0 0) (hit0 0))
    (flet ((addhit (hit)
	     (if collect
		 (setq hits (nconc hits (list hit)))
	       (or hits
		   (setq hits hit)))))
      (tagbody loop
	(cond ((setq star (position #\* pattern :start star0))
	       (cond ((null hit)
		      (if (= star 0)
			  (setq hit 0))
		      (if (> star 0)
			  (if (and (<= star sl)
				   ;; Really like string-equal/p
				   (apply #'search pattern string
					  :start1 0 :end1 star
					  :start2 0 :end2 star options))
			      (progn
				(addhit 0)
				(setq hit star))
			    (return nil))))
		     (t
		      (if (= star pl)
			  (return hits))
		      (if (and (< star pl)
			       (setq hit (apply #'search pattern string
						:start1 star0 :end1 star
						 :start2 hit0 options)))
			  (progn
			    (addhit hit)
			    (incf hit (- star star0)))
			(return nil)))))
	      ((= star0 pl)
	       (return hits))
	      (t 
	       (if (if hit
		       (setq hit (apply #'search pattern string
					:start1 star0 :end1 pl
					:start2 hit0 :end2 sl options))
		     ;; Really a string-equal/p
		     (and (eql pl sl)
			  (apply #'search pattern string
				 :start1 star0 :end1 pl
				 :start2 hit0 :end2 pl options)))
		   (progn 
		     (addhit (or hit hit0))
		     (return hits))
		 (return nil))))
	(setq star0 (1+ star) hit0 hit)
	(go loop)))))

#||
;; Test
(translate-logical-pathname "test:foo;bar;logidefs.lisp")

(setf (logical-pathname-translations "test")
  `(("**;*.*.*" ,*default-pathname-defaults*)
    ("*.*.*" ,*default-pathname-defaults*)))

(translate-logical-pathname "test:foo;bar;logidefs.lisp")
||#
