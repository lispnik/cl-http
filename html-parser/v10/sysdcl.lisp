;;; -*- Syntax: Ansi-Common-Lisp; Package: cl-user; Base: 10; Mode: lisp -*-

;;; File: sysdcl.lisp
;;; Last edited by smishra on Thu Oct 22 17:27:33 1998

;;; (c) Copyright 1996-98, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved

(in-package :cl-user)

#-(or Allegro LispWorks Genera MCL)
(error "System definitions included here are for the following platforms:
 * Allegro
 * LispWorks
 * Genera
 * MCL
If you write a system definition for another setup, please send
me a copy for inclusion.")

(pushnew :html-parser *features*)


;;;------------------------------------------------------------------- 
;;;
;;; Allegro system definition
;;;

#+Allegro
(unless (ignore-errors (logical-pathname-translations "HTML-PARSER"))
  (setf (logical-pathname-translations "HTML-PARSER")
    `(("**;*.*.*" 
       ,(make-pathname :directory (pathname-directory *load-truename*))))))

#+Allegro
(defsystem :html-parser
    (:default-pathname "HTML-PARSER:")
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

#+MCL (load "html-parser:mac-sysdcl")
