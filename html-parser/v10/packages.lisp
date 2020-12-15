;;; -*- Mode: LISP; Syntax: ANSI-COMMON-LISP; Package: USER; Base: 10 -*-
;;
;; Copyright (c) 1996-97 Sunil Mishra <smishra@cc.gatech.edu>,
;; portions copyright (c) 1999 Kaelin Colclasure <kaelin@acm.org>,
;; all rights reserved.
;;
;; $Id: //depot/rev/lisp/html-parser-10.0-alpha/packages.lisp#1 $

(in-package :user)

#-CL-HTTP
(defpackage :tk1
  (:use #-Genera :common-lisp
	#+Genera :future-common-lisp
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
   "ATTR-NAME"
   "ATTR-VAL"
   "HTML-FRAGMENT"
   "INSTANCE-OF"
   "PARTS"
   "PART-OF"
   "FILE->STRING"
   "STREAM->STRING"
   "SUBREFERENCE"
   "ENSURE-HTML-PARSER-TOKENS"
   "PARSER-INPUT"
   "PARSER-STACK"
   "PARSER-SAVE-FRAGMENTS"
   "PARSE-CDATA"
   #-CL-HTTP "GET-VALUE"
   #-CL-HTTP "REMOVE-VALUE"
   #-CL-HTTP "MAP-INDICATORS"
   #-CL-HTTP "MAP-VALUES"
   #-CL-HTTP "PROPERTY-LIST"
   #-CL-HTTP "WITH-VALUE-CACHED"

   ;; Definition macros & DTD access - probably not for the general user
   "DEFINE-HTML-ELEMENT"
   "DEFINE-HTML-ENTITY"
   "DEFINE-HTML-CHARACTERS"
   "DEFINE-REWRITE"
   "TAG-ATTRIBUTE-DEFINITION"
   "TAG-DEFINITION"
   "CONTAINS"

   ;; Variables & constants
   "*DTD-TAGS*"
   "*DTD-TOPLEVEL-TAGS*"
   "*HTML-CHARACTERS*"
   "*HTML-DTD-LIST*"
   "*CURRENT-DTD*"

   ;; Other symbols
   "PCDATA"
   "CDATA"
   ))
