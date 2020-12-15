;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: USER; Base: 10 -*-

;;; Copyright (C) 1994, 1995 Olivier (OBC) all rights reserved.
;;; See copyright notice in MINIPROC file.

(in-package "CL-USER")

;;; To use this independently of CL-HTTP"
;;;
(unless (ignore-errors (logical-pathname-translations "HTTP"))
  (setf (logical-pathname-translations "minp")
    `(("**;*.*.*" ,*default-pathname-defaults*)
      ("*.*.*" ,*default-pathname-defaults*))))

;;; Internal feature for systems that don't have native multiprocessing
#-(or Genera ccl-3 Allegro Lispworks Lucid)
(pushnew :MINIPROC *features*)

;;; Portable integration of resources and processes for CL
;;;
(defsystem clim-sys
     :source-pathname #.(if (ignore-errors
			      (logical-pathname-translations "minp"))
			     "minp:"
			  "HTTP:lcl;clim-sys;")
     :depends-on ()
     :components 
     ((:module system
	       :source-pathname ""
	       :components (
			    "package"	;Integrate resources and processes as CLIM-SYS
			    "process"
			    "miniproc"	;Portable Mini Processes
			    #+MINIPROC
			    "minilock"
			    "resource")))) ;JMCA resource adapted to fit CLIM-SYS spec of resource


