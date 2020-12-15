;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

(in-package :HTTP)

;;; Make minimum CL-HTTP export for LispWorks UNIX.
;;; This file must be loaded after the default configuration file

;;;------------------------------------------------------------------- 
;;;
;;;  LW EXPORTS
;;;

(export-url #u"/cl-http/sources/lw/-read-me-.text"
            :text-file
            :pathname (pathname "http:lw;-read-me-.text")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :lisp-works)
            :documentation
	    "README file for this alpha version of CL-HTTP for LispWorks for UNIX.")

(export-url #u"/cl-http/sources/lw/http.script"
            :text-file
            :pathname (pathname "http:lw;http.script")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :lisp-works)
            :documentation "Example shell script for installation of CL-HTTP under UNIX.")

(export-url #u"/cl-http/sources/lw/examples/"
            :lisp-directory
            :pathname (pathname "http:lw;examples;*.lisp")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :lisp-works)
            :documentation
	    "Example Lisp files showing configuration of the server and export of URLs.")

;; export lw-specific sources.
(export-url #u"/cl-http/sources/lw/"
            :lisp-directory
            :recursive-p t
            :pathname (pathname "http:lw;*.lisp")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :lisp-works))
