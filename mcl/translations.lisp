;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright John C. Mallery,  1994-1995
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;; LOGICAL PATHNAME HOST FOR HTTP

(defun cl-http-directory-name-string (mcl-directory)
  (let ((mcl-dir (translate-logical-pathname mcl-directory)))
    (directory-namestring (make-pathname :device (pathname-device mcl-dir)
					 :directory (butlast (pathname-directory mcl-dir) 1)))))

(defparameter *http-directory* (cl-http-directory-name-string ccl:*loading-file-source-file*))

(defun http-pathname (pathname)
   (concatenate 'string *http-directory* pathname))

(defun rooted-pathname (pathname)
   (concatenate 'string (subseq *http-directory* 0 (1+ (position #\: *http-directory* :test #'eql)))  pathname))

(setf (logical-pathname-translations "http")
      `(("http;*.*"          ,(http-pathname "*.*"))
	("logs;**;*.*"       ,(http-pathname "log:**:*.*"))
	("pw;**;*.*"         ,(http-pathname "log:pw:**:*.*"))
	("root;**;*.*"       ,(rooted-pathname "**:*.*"))
	("**;*.*"            ,(http-pathname "**:*.*"))))
