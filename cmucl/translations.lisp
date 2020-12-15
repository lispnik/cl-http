;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright John C. Mallery,  1994-1995
;;; All rights reserved.
;;;
;;; Tried to make MAC-TRANSLATIONS.LISP more portable - OBC
;;; Copyright (C) 1995, OBC for non MCL Genera ports.
;;;
;;;;;;------------------------------------------------------------------- 
;;;
;;; LOGICAL PATHNAME HOST FOR HTTP
;;; 


(defvar *http-directory* (ext:default-directory))

(defun http-pathname (&rest dirnames)
  (merge-pathnames (make-pathname :directory (append '(:relative) dirnames))
		   *http-directory*))

(defun rooted-pathname (&rest dirnames)
  (make-pathname :directory (append '(:absolute) dirnames
				    '("**"))
		 :name :wild
		 :type :wild))

(defun wildify (wild-bit logical-subdirectory &rest dirnames)
  (list (concatenate 'string logical-subdirectory
		     (and logical-subdirectory ";")
		     wild-bit)
	(let ((http-pathname (apply 'http-pathname dirnames)))
	  (namestring
	   (merge-pathnames (substitute #\/ #\; wild-bit)
			    http-pathname)))))

(setf (logical-pathname-translations "http")
  `(,(wildify "*.*.*" "http")
;    ,(wildify "*.*" "cl-http")
    ,(wildify "**;*.*.*" "client" "client")
    ,(wildify "**;*.*.*" "docs" "docs")
    ,(wildify "**;*.*.*" "lispm" "lispm")
    ,(wildify "**;*.*.*" "logs" "log")
    ,(wildify "**;*.*.*" "mcl" "mcl")
    ,(wildify "**;*.*.*" "pw" "log" "pw")
    ,(wildify "**;*.*.*" "server" "server")
    ,(wildify "**;*.*.*" "sources" )
    ,(wildify "**;*.*.*" "standards" "standards")
    ,(wildify "**;*.*.*" "www" "www")
    ,(wildify "**;*.*.*" "cmucl" "cmucl")
    ("root;**;*.*.*" ,(rooted-pathname))
    ,(wildify "**;*.*.*" nil)))
