;;; -*- Syntax: Ansi-Common-Lisp; Package: cl-user; Base: 10; Mode: lisp -*-

;;; File: packages.lisp
;;; Last edited by smishra on Fri Feb  6 00:59:11 1998

;;; (c) Copyright 1996-97, Sunil Mishra (smishra@cc.gatech.edu)
;;;     All Rights Reserved


#-CL-HTTP
(defpackage tk1
  (:use #+GENERA :future-common-lisp
	#+(OR LISPWORKS ALLEGRO LUCID MCL CMU) :common-lisp
	#+mcl :ccl)
  (:export
    "DEFINE-TOKENIZER"
    "UNDEFINE-TOKENIZER"))

(defpackage :html-parser
  #+CL-HTTP(:shadowing-import-from :http "COPY-FILE")
  (:use #-Genera :common-lisp
	#+Genera :future-common-lisp
	#+lispworks :clos #+lispworks :defsystem
	#+CL-HTTP :http
	#+mcl :ccl
	:tk1)
  (:export
   ;; Classes
   "ABSTRACT-TAG-INSTANCE"
   "HTML-TAG-INSTANCE"
   "UNKNOWN-TAG-INSTANCE"
   #-CL-HTTP "PROPERTY-LIST-MIXIN"
   "HTML-PARSER-TOKEN"
   "HTML-NAME-TOKEN"
   "HTML-ENTITY-TOKEN"

   ;; Primary functions
   "INITIALIZE-PARSER"
   "IGNORING-EXITS"
   "DEFINE-HTML-PARSER-CONTEXT"
   "DEFINE-HTML-PARSER"

   ;; Secondary functions
   "REWRITE-SEXP"
   "HTML-WHITESPACE-P"
   "RETURN-IF-NOT-WHITESPACE"
   "TOKENIZE-NAME"
   "TOKENIZE-ENTITY"
   "TOKEN-NAME"
   "NAME"
   "ATTR-VALUES"
   "MAKE-PCDATA-STRING"
   "PARTS"
   "ATTR-NAME"
   "ATTR-VAL"
   "HTML-FRAGMENT"
   "PARTS"
   "PART-OF"
   "FILE->STRING"
   "STREAM->STRING"
   "ENSURE-HTML-PARSER-TOKENS"
   "PARSER-INPUT"
   "PARSER-STACK"
   "PARSE-CDATA"
   #-CL-HTTP "GET-VALUE"
   #-CL-HTTP "REMOVE-VALUE"
   #-CL-HTTP "MAP-INDICATORS"
   #-CL-HTTP "MAP-VALUES"
   #-CL-HTTP "PROPERTY-LIST"
   #-CL-HTTP "WITH-VALUE-CACHED"

   ;; Definition macros - probably not for the general user
   "DEFINE-HTML-ELEMENT"
   "DEFINE-HTML-ENTITY"
   "DEFINE-HTML-CHARACTERS"
   "DEFINE-REWRITE"

   ;; Variables & constants
   "*HTML-TAGS*"
   "*HTML-ROOT*"
   "*HTML-CHARACTERS*"
   "*HTML-DTD-LIST*"
   "*CURRENT-DTD*"

   ;; Other symbols
   "PCDATA"
   "CDATA"
   ))
