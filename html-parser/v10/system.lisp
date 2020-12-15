;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: USER; Base: 10 -*-
;;
;; $Id: //depot/rev/lisp/html-parser-10.0-alpha/system.lisp#1 $

(in-package :user)

(unless (ignore-errors (logical-pathname-translations "HTML-PARSER"))
  (setf (logical-pathname-translations "HTML-PARSER")
    `(("**;*.*.*" 
       ,(make-pathname :directory (pathname-directory *load-truename*))))))

(defsystem :html-parser
    (:default-pathname "HTML-PARSER:")
  (:serial
   "packages"
   #-CL-HTTP "tokenizer"
   #-CL-HTTP "plist"
   (:definitions "defs"
       (:serial
	"patmatch"
	"rewrite-engine"
	"rewrite-rules"
	"html-tags"
	"html-reader"
	"html-parser"
	"html-utilities"))))

;;; EOF
