;;;-*- Syntax: Ansi-Common-Lisp; Base: 10; Mode: lisp; Package: common-lisp-user -*-
;;;
;;; Copyright 1994-96, John C. Mallery.
;;; All rights reserved.
;;;------------------------------------------------------------------- 
;;;
;;;  SYSTEM DEFINITION FOR BASIC CL-HTTP CLIENT
;;;

;; This requires that the server be loaded first as there is
;; considerable shared code.

(in-package "CL-USER")

(defsystem cl-http-client
  :source-pathname "HTTP:"
  :components
  ("client;variables"			; Client variables
   "cmucl;client;unix"	                ; Platform support for client
   "client;connection"			; Persistent connections
   "client;client"			; Portable client code
   "client;sexp-browser"		; S-Expression browser application
   "client;flogger"))			; CL-HTTP flogger
