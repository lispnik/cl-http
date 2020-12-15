;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: USER; Base: 10 -*-

;;; See copyright notice in CLIM-SYS;MINIPROC file.

(in-package "USER")

(defvar *aclpc-directory* 
    (make-pathname :host (pathname-host *load-pathname*)
		   :device (pathname-device *load-pathname*)
		   :directory (pathname-directory *load-pathname*)
		   :name nil :type nil :version nil))

(setf (logical-pathname-translations "aclpc")
      `(("**;*.*.*" ,*aclpc-directory*)
	("*.*.*" ,*aclpc-directory*)))

(defsystem "aclpc-update"
    (:pretty-name "ACLPC Update to Portable CLIM-SYS package"
     :default-pathname #.(if (ignore-errors
			      (logical-pathname-translations "HTTP"))
			     "HTTP:acl;aclpc;"
			   "aclpc:"))
  (:serial
   "format"
   "stream-sequence"
   "listener"
   "mintface"))
