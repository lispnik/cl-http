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


#+(or Allegro LispWorks lcl4.2)
(in-package "CL-USER")

(defparameter *http-directory*
    (or #+MCL (directory-namestring ccl:*loading-file-source-file*)
	#+Genera "jefferson.ai.mit.edu|host:jefferson hd1:Desktop Folder:MAC-CL-HTTP_0.5.4:"
	#-(or LispWorks lcl4.2) (directory-namestring *default-pathname-defaults*)
	#+LispWorks (directory-namestring (truename (lw:current-directory "../")))
	#+lcl4.2 (directory-namestring (truename (working-directory)))))

(defun http-pathname (&rest dirnames)
  (merge-pathnames (make-pathname :directory (append '(:relative) dirnames))
		   *http-directory*))

(defun rooted-pathname (&rest dirnames)
  (make-pathname :directory (append '(:absolute) dirnames
				    #-Allegro '("**"))
		 #-Allegro :name #-Allegro :wild
		 #-Allegro :type #-Allegro :wild))

(defun wildify (wild-bit logical-subdirectory &rest dirnames)
  (list (concatenate 'string logical-subdirectory
		     (and logical-subdirectory ";")
		     wild-bit)
	(let ((http-pathname (apply 'http-pathname dirnames)))
	  #-Allegro
	  (namestring
	   (merge-pathnames (substitute #-MCL #\/ #+MCL #\: #\; wild-bit)
			    http-pathname))
	  #+Allegro
	  http-pathname)))

(setf (logical-pathname-translations "http")
  `(,(wildify "*.*" "cl-http")
    ,(wildify "**;*.*" "client" "client")
    ,(wildify "**;*.*" "docs" "docs")
    ,(wildify "**;*.*" "lispm" "lispm")
    ,(wildify "**;*.*" "logs" "log")
    ,(wildify "**;*.*" "mac" "mac")
    ,(wildify "**;*.*" "pw" "log" "pw")
    ,(wildify "**;*.*" "server" "server")
    ,(wildify "**;*.*" "sources" )
    ,(wildify "**;*.*" "standards" "standards")
    ,(wildify "**;*.*" "www" "www")
;    ,(wildify "**;**;*.*" "lcl" "lcl")
;    ,(wildify "*.*" "http")
    ("root;**;*.*" ,(rooted-pathname))
    
    ,(wildify "**;*.*" nil)))





