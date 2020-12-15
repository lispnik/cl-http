;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: CL-USER; Base: 10 -*-

;;; Copyright (C) 1994, 1995 Olivier (OBC) all rights reserved.
;;; See copyright notice in MINIPROC file.

(in-package "CL-USER")

(defvar *minp-directory*
    (make-pathname :host (pathname-host *load-pathname*)
		   :device (pathname-device *load-pathname*)
		   :directory (pathname-directory *load-pathname*)
		   :name nil :type nil :version nil))

;; Define or complement tranlations and defsystem.
(let ((path (make-pathname :name "defsystem" :defaults *load-pathname*))
      loaded)
  #-ACLPC
  (mapc #'(lambda (p)
	    (if (null loaded)
		(setq loaded (ignore-errors (load p)))))
	(sort (copy-seq (directory (make-pathname :type :wild :defaults path)))
	      #'> :key #'file-write-date))
  (if (null loaded)
      (load path)))

(setf (logical-pathname-translations "minp")
  `(("**;*.*.*" ,*minp-directory*)
    ("*.*.*" ,*minp-directory*)))

(defsystem "defsys"
    (:pretty-name "Minimal defsys"
     :default-pathname "minp:")
  (:serial "logidefs"
	   "defsystem"))

;;; Internal feature for systems that don't have native multiprocessing
#-(or Genera ccl-3 Allegro Lispworks Lucid)
(pushnew :MINIPROC *features*)

;;; Portable integration of resources and processes for CL
;;;
(defsystem "clim-sys"
    (:pretty-name "Portable CLIM-SYS package"
     :default-pathname #.(if (ignore-errors (logical-pathname-translations "HTTP"))
			     "HTTP:clim;clim-sys;"
			   "minp:")
     .
     #-Genera nil
     #+Genera (
     :journal-directory "HTTP:LISPM;CLIM-SYS;PATCH;"
     :initial-status :experimental
     :patchable t
     :source-category :basic))
  (:serial
   "package"	 ;Integrate resources and processes as CLIM-SYS
   "process"
   "miniproc"	 ;Portable Mini Processes
   #+MINIPROC
   "minilock"
   "resource"	 ;JMCA resource adapted to fit CLIM-SYS spec of resource
   "util"
   "demo"))	 ;systest for Miniproc
