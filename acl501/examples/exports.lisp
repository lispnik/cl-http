;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

(in-package :HTTP)

;;;------------------------------------------------------------------- 
;;;
;;;  ACL EXPORTS
;;;

(export-url #u"/cl-http/sources/acl501/-read-me-.text"
            :text-file
            :pathname (pathname "http:acl501;-read-me-.text")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :allegro)
            :documentation
	    "README file for this alpha version of CL-HTTP for ACL UNIX.")

(export-url #u"/cl-http/sources/acl501/examples/"
            :lisp-directory
            :pathname (pathname "http:acl501;examples;*.lisp")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :allegro)
            :documentation
	    "Example Lisp files showing configuration of the server and export of URLs.")

;; export ACL-specific sources.
(export-url #u"/cl-http/sources/acl501/"
            :lisp-directory
            :recursive-p t
            :pathname (pathname "http:acl501;*.lisp")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :allegro))
