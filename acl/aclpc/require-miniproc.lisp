;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: cl-user; Base: 10 -*-

(in-package "USER")

;;; Load this to only compile/load the minimum CLIM-SYS MINIPROC system.
;;;
#+ACLPC
(defvar *http-directory*
    (make-pathname :host (pathname-host *load-pathname*)
		   :device (pathname-device *load-pathname*)
		   :directory (butlast (pathname-directory *load-pathname*) 2)
		   :name nil :type nil :version nil))

;;; Bootstrap: HTTP:clim;clim-sys;sysdcl.lisp
#+ACLPC
(load (merge-pathnames (make-pathname :directory '(:relative "clim" "clim-sys")
				      :name "sysdcl" :type "lisp")
		       *http-directory*))

(unless (ignore-errors (logical-pathname-translations "HTTP"))
  (setf (logical-pathname-translations "HTTP")
    `(("**;*.*.*" ,*http-directory*)
      ("*.*.*" ,*http-directory*))))

#+ACLPC
(load "HTTP:acl;aclpc;sysdcl")

(load "HTTP:clim;clim-sys;sysdcl")

(compile-system 'clim-sys)

(compile-system 'aclpc-update)

(format t "~&To start the mini scheduler, type:~%(minp:scheduling)~%")
