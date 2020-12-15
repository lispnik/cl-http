;;; -*- Syntax: Ansi-Common-Lisp; Package: FUTURE-COMMON-LISP-USER; Base: 10; Mode: lisp -*-

;;; (C) Copyright 1995, John C. Mallery.
;;;     All Rights Reserved.
;;;


;;;------------------------------------------------------------------- 
;;;
;;; LOADING LISP MACHINE PATCH FILES
;;;

;;; This file allows other Common Lisps to compile and load patch files for
;;; the Lisp Machine.

(in-package :cl-user)

(add-nickname "WWW-UTILS" "SCT")

(in-package :www-utils)

;; export the relevant functions
(mapc #'(lambda (x) (export (intern x :www-utils) :www-utils))
      `("FILES-PATCHED-IN-THIS-PATCH-FILE" "BEGIN-PATCH-SECTION" "PATCH-SECTION-SOURCE-FILE" "PATCH-SECTION-ATTRIBUTES"))

(defun files-patched-in-this-patch-file (&rest pathnames)
  (declare (ignore pathnames)))

(defun begin-patch-section ())

(defun patch-section-source-file (pathname)
  pathname)

(defun patch-section-attributes (attribute-list)
  (flet ((get-attributes (string &optional (start 0) (end (length string)))
           (loop for colon = (position #\: string :start start :end end)
                 while colon
                 for semi = (position #\; string :start colon :end end)
                 while semi
                 for key = (http::symbolize (string-trim "-* " (subseq string start colon)) http::*keyword-package*)
                 for value = (string-trim " " (subseq string (1+ colon) semi))
                 collect key
                 collect value
                 do (setq start (1+ semi)))))
    (let* ((plist (get-attributes attribute-list
                                  (position-if-not #'(lambda (ch) (member ch '(#\* #\-) :test #'eql)) attribute-list)))
           (package (getf plist :package))
           (pos (position-if-not #'(lambda (ch) (member ch '(#\( #\space) :test #'eql)) package)))
      (unless (zerop pos)
        (setq package (subseq package pos (position-if #'(lambda (ch) (member ch '(#\( #\space) :test #'eql)) package :start pos))))
      (setq package (http::symbolize package http::*keyword-package*))
      #+ANSI-CL (setq *package* (find-package package))
      #-:ANSI-CL (in-package package))))

#|(PATCH-SECTION-ATTRIBUTES
  "-*- Mode: lisp; Package: (netscape :use (future-common-lisp html2 www-utils url) :nicknames (ntsp)); BASE: 10; Syntax: ANSI-Common-Lisp; Default-Character-Style: (:FIX :ROMAN :NORMAL);-*-")|#
