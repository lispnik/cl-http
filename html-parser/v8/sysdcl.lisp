;;; -*- Syntax: Ansi-Common-Lisp; Package: cl-user; Base: 10; Mode: lisp -*-

;;; File: sysdcl.lisp
;;; Last edited by smishra on Mon Aug 11 13:01:49 1997

;;; (c) Copyright 1996-97, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved

(in-package :cl-user)

#-(or LispWorks Genera MCL)
(error "System definitions included here are for the following platforms:
 * LispWorks
 * Genera
 * MCL
If you write a system definition for another setup, please send
me a copy for inclusion.")

(pushnew :html-parser *features*)


;;;------------------------------------------------------------------- 
;;;
;;; LispWorks system definition
;;;

#+LispWorks
(unless (ignore-errors (logical-pathname-translations "HTML-PARSER"))
  (setf (logical-pathname-translations "HTML-PARSER")
	`(("**;*.*.*"
	   ,(merge-pathnames "**/*.*"
			     (truename (lw:current-directory ".")))))))

#+LispWorks
(defsystem html-parser (:default-pathname "HTML-PARSER:")
  :members ("packages"
	    #-CL-HTTP "tokenizer"
	    #-CL-HTTP "plist"
	    "defs"
	    "patmatch"
	    "rewrite-engine"
	    "rewrite-rules"
	    "html-tags"
	    "html-reader"
	    "html-parser"
	    "html-utilities")
  :rules ((:in-order-to :compile :all
			(:requires (:load :previous)))))

#+LispWorks (compile-system 'html-parser :load t)


;;;------------------------------------------------------------------- 
;;;
;;; Genera system definition
;;;

#+ignore
(unless (fs:get-logical-pathname-host "HTML-PARSER" t)
  (fs:make-logical-pathname-host "HTML-PARSER"))

#+Genera
(sct:defsystem HTML-Parser
    (:pretty-name "HTML Parser"
     :default-pathname "html-parser:html-parser;"
     :journal-directory "html-parser:journal;"
     :initial-status :released
     :patchable t
     :source-category :basic)
  (:module pointers
   ("sys:site;html-parser.translations"
    "sys:site;html-parser.system")
   (:type :lisp-example))
  (:module examples
   ("html-parser:html-parser;readme.text")
   (:type :lisp-example))
  (:serial
    "packages"
    #-CL-HTTP "tokenizer"
    #-CL-HTTP "plist"
    "defs"
    "patmatch"
    "rewrite-engine"
    "rewrite-rules"
    "html-tags"
    "html-reader"
    "html-parser"
    "html-utilities"))

;;;-------------------------------------------------------------------
;;;
;;; MCL system definition
;;;

#+(and MCL CL-HTTP) 
(load "http:mcl;html-parser;sysdcl")

#+(and MCL (not CL-HTTP))
(load "html-parser:mac-sysdcl")

;;;-------------------------------------------------------------------
;;;
;;; Default Initialization
;;;

;; 7/13/98 cvince: Took this out for Genera, doesn't compile.
#-(or MCL Genera) (html-parser:initialize-parser)
