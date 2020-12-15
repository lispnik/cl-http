;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: MINP; Base: 10 -*-

;;; Portable integration of resources and processes for CL
;;;

(in-package "CL-USER")

(defsystem clim-sys ()
  :members ("package"
	    "process"
	    "lw-miniproc")
  :rules
  ((:in-order-to :compile :all (:requires (:load "package")))))

