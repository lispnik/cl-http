;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright John C. Mallery,  1994-1995
;;; All rights reserved.
;;;
;;; Tried to make MAC-TRANSLATIONS.LISP more portable - OBC
;;; Copyright (C) 1995, OBC for non MCL Genera ports.
;;;
;;;;;;------------------------------------------------------------------- 
;;;
;;; LOGICAL PATHNAME HOST FOR HTTP
;;; 


(in-package "USER")

(defun http-pathname (&rest dirnames)
  (merge-pathnames (make-pathname :directory (append '(:relative) dirnames))
		   *http-directory*))

(defun rooted-pathname (&rest dirnames)
  (make-pathname :directory (append '(:absolute) dirnames)))

(pushnew "transhosts.lisp" (logical-pathname-translations-database-pathnames) :test #'equal)

(let ((*default-pathname-defaults* *load-pathname*))
  (load-logical-pathname-translations "http"))
