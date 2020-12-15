;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

(in-package :HTTP)

;;; Make minimum CL-HTTP export for Allegro for UNIX.
;;; This file must be loaded after the default configuration file
;;; - OBC

;;;------------------------------------------------------------------- 
;;;
;;;  ACL EXPORTS
;;;

(export-url #u"/cl-http/sources/acl/-read-me-.text"
            :text-file
            :pathname (pathname "http:acl;-read-me-.text")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :allegro)
            :documentation
	    "README file for this alpha version of CL-HTTP for ACL UNIX.")

(export-url #u"/cl-http/sources/acl/http.script"
            :text-file
            :pathname (pathname "http:acl;http.script")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :allegro)
            :documentation
	    "Example shell script for installation of CL-HTTP under UNIX.")

(export-url #u"/cl-http/sources/acl/examples/"
            :lisp-directory
            :pathname (pathname "http:acl;examples;*.lisp")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :allegro)
            :documentation
	    "Example Lisp files showing configuration of the server and export of URLs.")

;; export ACL-specific sources.
(export-url #u"/cl-http/sources/acl/"
            :lisp-directory
            :recursive-p t
            :pathname (pathname "http:acl;*.lisp")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :allegro))

(export-url #u"/cl-http/sources/clim/clim-sys/"
            :lisp-directory
            :recursive-p t
            :pathname (pathname "http:clim;clim-sys;*.lisp")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :allegro))

;;; Add UNIX CGI access tests
;;;
#+Allegro
(define-unix-cgi-bin "/cl-http/cgi-bin/" (pathname "http:acl;cgi-bin;"))

(conditional-copy-file "http:acl;configure-acl.html" "HTTP:www;cl-http;configure-acl.html")

;;; Provided for user extensions
;;;
#+Allegro
(add-export-pathname "http:custom;user-exports")

;;; Compile and load new exports only
;;;
#+Allegro
(eval-when (eval compile)
  (compile-exports))

#+Allegro
(eval-when (load)
  (load-exports))
