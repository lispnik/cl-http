;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

(in-package :HTTP)

;;; Make minimum CL-HTTP export for Allegro for UNIX.
;;; This file must be loaded after the default configuration file
;;; 

;;;------------------------------------------------------------------- 
;;;
;;;  lcl EXPORTS
;;;

(export-url #u"/cl-http/sources/lcl/-read-me-.text"
            :text-file
            :pathname (pathname "http:lcl;-read-me-.text")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :lucid)
            :documentation
	    "README file for this alpha version of CL-HTTP for LCL under UNIX.")

(export-url #u"/cl-http/sources/lcl/http.script"
            :text-file
            :pathname (pathname "http:lcl;http.script")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :lucid)
            :documentation
	    "Example shell script for installation of CL-HTTP under UNIX.")

(export-url #u"/cl-http/sources/lcl/examples/"
            :lisp-directory
            :pathname (pathname "http:lcl;examples;*.lisp")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :lucid)
            :documentation
	    "Example Lisp files showing configuration of the server and export of URLs.")

;; export lcl-specific sources.
(export-url #u"/cl-http/sources/lcl/"
            :lisp-directory
            :recursive-p t
            :pathname (pathname "http:lcl;*.lisp")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :lucid))
