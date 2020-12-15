;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Package: http; Base: 10 -*-

(in-package :HTTP)

;;; Make minimum CL-HTTP export for CMU Common Lisp for UNIX.  This
;;; file must be loaded after the default configuration file
;;; 

;;;------------------------------------------------------------------- 
;;;
;;;  CMU CL EXPORTS
;;;

(export-url #u"/cl-http/sources/cmucl/read-me.text"
            :text-file
            :pathname (pathname "http:cmucl;read-me.text")
            :expiration `(:interval ,(* 15. 60.))
	    :keywords '(:cl-http :documentation :cmu-cl)
            :documentation
	    "README file for this alpha version of CL-HTTP for CMU CL under UNIX.")

;; export cmucl-specific sources.
(export-url #u"/cl-http/sources/cmucl/"
            :lisp-directory
            :recursive-p t
            :pathname (pathname "http:cmucl;*.lisp")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :cmu-cl))

;;; Patch and bug files.
(export-url #u"/cl-http/sources/cmucl/"
            :text-directory
            :recursive-p t
            :pathname (pathname "http:cmucl;*.text")
            :expiration `(:interval ,(* 15. 60.))
            :keywords '(:cl-http :documentation :cmu-cl))
